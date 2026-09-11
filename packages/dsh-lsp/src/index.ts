/**
 * dsh-lsp — LSP semantic code navigation for DeepSeek Harness.
 *
 * A single model-facing `lsp` tool (definition / references / implementation
 * / typeDefinition / hover / workspaceSymbol / documentSymbol / diagnostics,
 * plus an aggregated `explore` that mirrors codegraph's one-call symbol
 * lookup) and one `/lsp` command for manual server control, over managed
 * language-server subprocesses.
 *
 * The plugin is self-contained and dependency-light: it imports only
 * `vscode-languageserver-protocol`, and consumes the harness through the same
 * structural contracts `dsh-hindsight` uses (`ctx.tools`, `ctx.commands`,
 * `ctx.subprocess`, `ctx.effect`).
 *
 * @module dsh-lsp
 */

import { LspClient } from './client.js'
import { resolveConfig } from './config.js'
import { diagnosticsMessage, filterDiagnostics } from './diagnostics.js'
import { isFileUri, pathToFileUri, readLinePreview, renderLocations, toDiagnosticEntries, toLspLocation, toSymbolEntry } from './protocol.js'
import { ServerRegistry } from './registry.js'
import type { ToolExecution } from '@deepseek-ai/dsh-tools'
import type {
  CommandInvocation,
  CommandResult,
  DshContext,
  ExploreResult,
  Logger,
  LspLocation,
  LspQueryResult,
  QueryArgs,
  SymbolEntry,
  ToolRunContext,
} from './types.js'

export const name = 'dsh-lsp'
export const inject = ['tools', 'commands', 'subprocess', 'systemPrompt']
export { resolveConfig }
export { LspClient } from './client.js'
export { ServerRegistry } from './registry.js'
export { findRoot, RootResolver } from './root.js'
export { pathToFileUri, fileUriToPath, toLspPosition, fromLspPosition } from './protocol.js'
export type { LspConfig, ServerSpec, QueryOperation, LspLocation, ExploreResult } from './types.js'

const TOOL_SECTION_ORDER = 136

function makeLogger(ctx: DshContext): Logger {
  // ctx.logger is the official LoggerService, always present and callable.
  try {
    return ctx.logger('lsp')
  } catch {
    /* fall through to a no-op facade */
  }
  // Logger is a class type (private `service`/`_method`); the no-op facade
  // only needs its public severity methods, so cast through unknown.
  return { name: 'lsp', debug() {}, info() {}, warn() {}, error() {} } as unknown as Logger
}

function requireString(args: any, key: string): string {
  const value = args?.[key]
  if (typeof value !== 'string' || value.trim() === '') {
    throw new Error(`Missing required string parameter: ${key}`)
  }
  return value
}

function optionalInteger(args: any, key: string, minimum = 1): number | undefined {
  const value = args?.[key]
  if (value === undefined || value === null || value === '') return undefined
  const parsed = Math.trunc(Number(value))
  if (!Number.isFinite(parsed) || parsed < minimum) return undefined
  return parsed
}

function cwdOf(exec: ToolExecution | undefined): string | undefined {
  return exec?.agent?.session?.header?.cwd
}

function hoverToText(hover: { contents: unknown } | null | undefined): string | null {
  if (!hover) return null
  const contents = hover.contents
  if (contents === null || contents === undefined) return null
  if (typeof contents === 'string') return contents
  if (Array.isArray(contents)) {
    return contents
      .map(part => {
        if (typeof part === 'string') return part
        if (part && typeof part === 'object' && 'value' in part) return String((part as { value: unknown }).value)
        return ''
      })
      .filter(Boolean)
      .join('\n')
  }
  if (typeof contents === 'object' && 'value' in (contents as Record<string, unknown>)) {
    return String((contents as { value: unknown }).value)
  }
  return null
}

function renderSymbols(symbols: SymbolEntry[], maxLocations: number, withPreview = false): { text: string; truncated: boolean } {
  const truncated = symbols.length > maxLocations
  const shown = truncated ? symbols.slice(0, maxLocations) : symbols
  const lines: string[] = []
  for (const sym of shown) {
    const loc = `${sym.location.path}:${sym.location.range.startLine}:${sym.location.range.startCharacter}`
    if (withPreview) {
      const preview = readLinePreview(sym.location)
      lines.push(`${sym.kind.padEnd(12)} ${sym.name}  ${loc}${preview ? ` │ ${preview}` : ''}`)
    } else {
      lines.push(`${sym.kind.padEnd(12)} ${sym.name}  ${loc}`)
    }
  }
  if (truncated) lines.push(`… ${symbols.length - maxLocations} more symbol(s) omitted (maxLocations=${maxLocations})`)
  return { text: lines.join('\n'), truncated }
}

/**
 * Run one LSP operation and return the canonical structured result.
 */
async function runQuery(
  registry: ServerRegistry,
  args: QueryArgs,
  config: ReturnType<typeof resolveConfig>,
  exec: ToolRunContext | undefined,
): Promise<LspQueryResult> {
  const signal = exec?.signal
  const cwd = cwdOf(exec)
  const filePath = args.filePath

  if (args.operation === 'workspaceSymbol') {
    const query = args.query ?? ''
    // workspaceSymbol routes through the file's server; use the file path as
    // the server selector (per the AGENTS.md LSP guidance).
    const { spec, client, root } = await registry.resolve(filePath, cwd, signal)
    const raw = await client.workspaceSymbol(filePath, query, signal)
    const rootUri = pathToFileUri(root)
    const symbols: SymbolEntry[] = (raw ?? []).map(sym => toSymbolEntry({
      name: sym.name,
      kind: sym.kind,
      location: sym.location,
      ...(sym.containerName ? { containerName: sym.containerName } : {}),
    }, rootUri))
    const rendered = renderSymbols(symbols, config.maxLocations)
    return { kind: 'symbols', symbols, truncated: rendered.truncated }
  }

  if (args.operation === 'documentSymbol') {
    const { client, root } = await registry.resolve(filePath, cwd, signal)
    const raw = await client.documentSymbol(filePath, signal)
    const rootUri = pathToFileUri(root)
    const symbols: SymbolEntry[] = []
    const walk = (items: any[]) => {
      for (const item of items ?? []) {
        if (item && typeof item === 'object' && 'name' in item) {
          if ('range' in item && 'selectionRange' in item) {
            // DocumentSymbol (hierarchical)
            symbols.push(toSymbolEntry({
              name: item.name,
              kind: item.kind,
              location: { uri: pathToFileUri(filePath), range: item.selectionRange },
              ...(item.detail ? { containerName: item.detail } : {}),
            }, rootUri))
            if (Array.isArray(item.children)) walk(item.children)
          } else if ('location' in item) {
            symbols.push(toSymbolEntry(item, rootUri))
          }
        }
      }
    }
    walk(raw as any[])
    const rendered = renderSymbols(symbols, config.maxLocations)
    return { kind: 'symbols', symbols, truncated: rendered.truncated }
  }

  if (args.operation === 'diagnostics') {
    const { client } = await registry.resolve(filePath, cwd, signal)
    const raw = await client.diagnostics(filePath, signal)
    const diagnostics = toDiagnosticEntries(raw)
    return { kind: 'diagnostics', diagnostics }
  }

  // Position-requiring operations.
  if (args.line === undefined || args.character === undefined) {
    throw new Error(`operation '${args.operation}' requires both line and character`)
  }
  const line = args.line
  const character = args.character
  const { client, root } = await registry.resolve(filePath, cwd, signal)
  const rootUri = pathToFileUri(root)

  const toLocations = (raw: Array<{ uri: string; range: import('vscode-languageserver-protocol').Range }>): LspLocation[] =>
    raw.map(loc => toLspLocation(loc.uri, loc.range, rootUri))

  switch (args.operation) {
    case 'goToDefinition': {
      const raw = await client.definition(filePath, line, character, signal)
      const locations = toLocations(raw)
      const rendered = renderLocations('definition:', locations, config.maxLocations)
      return { kind: 'locations', locations, truncated: rendered.truncated }
    }
    case 'typeDefinition': {
      const raw = await client.typeDefinition(filePath, line, character, signal)
      const locations = toLocations(raw)
      const rendered = renderLocations('type definition:', locations, config.maxLocations)
      return { kind: 'locations', locations, truncated: rendered.truncated }
    }
    case 'goToImplementation': {
      const raw = await client.implementation(filePath, line, character, signal)
      const locations = toLocations(raw)
      const rendered = renderLocations('implementation:', locations, config.maxLocations)
      return { kind: 'locations', locations, truncated: rendered.truncated }
    }
    case 'findReferences': {
      const raw = await client.references(filePath, line, character, signal)
      const locations = toLocations(raw)
      const rendered = renderLocations('references:', locations, config.maxLocations)
      return { kind: 'locations', locations, truncated: rendered.truncated }
    }
    case 'hover': {
      const raw = await client.hover(filePath, line, character, signal)
      return { kind: 'hover', hover: hoverToText(raw) }
    }
    case 'explore': {
      const [definition, typeDef, impl, refs, hover] = await Promise.all([
        client.definition(filePath, line, character, signal),
        client.typeDefinition(filePath, line, character, signal),
        client.implementation(filePath, line, character, signal),
        client.references(filePath, line, character, signal),
        client.hover(filePath, line, character, signal),
      ])
      // Best-effort symbol name: prefer documentSymbol range hit, else empty.
      let symbolName = ''
      try {
        const docSymbols = await client.documentSymbol(filePath, signal)
        const target = { line: line - 1, character: character - 1 }
        const findName = (items: any[]): string => {
          for (const item of items ?? []) {
            if (item && typeof item === 'object' && 'name' in item) {
              const sel = item.selectionRange ?? item.range
              if (sel && sel.start.line === target.line && sel.start.character === target.character) {
                return item.name
              }
              if (Array.isArray(item.children)) {
                const child = findName(item.children)
                if (child) return child
              }
            }
          }
          return ''
        }
        symbolName = findName(docSymbols as any[])
      } catch {
        /* symbol name is optional */
      }
      const definitionLoc = toLocations(definition)
      const typeDefLoc = toLocations(typeDef)
      const implLoc = toLocations(impl)
      const refsLoc = toLocations(refs)
      const total = definitionLoc.length + typeDefLoc.length + implLoc.length + refsLoc.length
      const truncated = total > config.maxLocations
      const explore: ExploreResult = {
        symbolName,
        definition: definitionLoc,
        typeDefinition: typeDefLoc,
        hover: hoverToText(hover),
        implementation: implLoc,
        references: refsLoc,
        truncated,
      }
      return { kind: 'explore', explore }
    }
    default:
      throw new Error(`unknown operation '${String((args as any).operation)}'`)
  }
}

function renderResult(args: QueryArgs, value: any, config: ReturnType<typeof resolveConfig>): string {
  switch (value.kind) {
    case 'locations': {
      const rendered = renderLocations(value.kind, value.locations, config.maxLocations, true)
      return value.locations.length === 0 ? 'No results.' : rendered.text
    }
    case 'hover':
      return value.hover ?? 'No hover information.'
    case 'symbols': {
      const rendered = renderSymbols(value.symbols, config.maxLocations, true)
      return value.symbols.length === 0 ? 'No symbols.' : rendered.text
    }
    case 'diagnostics':
      if (value.diagnostics.length === 0) return 'No diagnostics.'
      return value.diagnostics.map((d: any) =>
        `[${d.severity}] ${d.source ? d.source + ': ' : ''}${d.range.startLine}:${d.range.startCharacter} ${d.message}`).join('\n')
    case 'explore': {
      const e = value.explore
      const lines: string[] = []
      if (e.symbolName) lines.push(`symbol: ${e.symbolName}`)
      if (e.hover) lines.push(`\n${e.hover}`)
      lines.push('')
      lines.push(renderLocations('definition:', e.definition, config.maxLocations, true).text)
      lines.push(renderLocations('type definition:', e.typeDefinition, config.maxLocations, true).text)
      lines.push(renderLocations('implementation:', e.implementation, config.maxLocations, true).text)
      lines.push(renderLocations('references:', e.references, config.maxLocations, true).text)
      if (e.truncated) lines.push(`\n(aggregate truncated at maxLocations=${config.maxLocations})`)
      return lines.filter(Boolean).join('\n')
    }
    default:
      return JSON.stringify(value)
  }
}

function capText(text: string, maxChars: number): string {
  if (text.length <= maxChars) return text
  return text.slice(0, maxChars) + `\n… (truncated at maxResultChars=${maxChars})`
}

/**
 * Produce a stable JSON projection of a query result for programmatic
 * consumption (e.g. DSH's PTC mode, where a JS snippet post-processes the
 * tool output). Locations/symbols flatten to `{ path, line, character, preview }`
 * so a consumer never re-parses the human `text` form. All values are plain
 * JSON-serializable (no live objects).
 */
function toJson(result: LspQueryResult): unknown {
  const loc = (l: LspLocation) => {
    const preview = l.preview ?? readLinePreview(l)
    return {
      path: l.path,
      line: l.range.startLine,
      character: l.range.startCharacter,
      ...(preview ? { preview } : {}),
    }
  }
  switch (result.kind) {
    case 'locations':
      return { kind: 'locations', locations: result.locations.map(loc), truncated: result.truncated }
    case 'hover':
      return { kind: 'hover', hover: result.hover }
    case 'symbols':
      return {
        kind: 'symbols',
        symbols: result.symbols.map(s => ({
          name: s.name,
          kind: s.kind,
          ...(s.containerName ? { containerName: s.containerName } : {}),
          location: loc(s.location),
        })),
        truncated: result.truncated,
      }
    case 'diagnostics':
      return { kind: 'diagnostics', diagnostics: result.diagnostics }
    case 'explore': {
      const e = result.explore
      return {
        kind: 'explore',
        symbolName: e.symbolName,
        hover: e.hover,
        definition: e.definition.map(loc),
        typeDefinition: e.typeDefinition.map(loc),
        implementation: e.implementation.map(loc),
        references: e.references.map(loc),
        truncated: e.truncated,
      }
    }
    default:
      return result
  }
}

/**
 * Combine several AbortSignals into one that aborts when any source aborts.
 * Lets a write's diagnostics run observe both the caller's turn signal and
 * the superseding controller that cancels a stale in-flight run.
 */
function mergeAbortSignals(...signals: Array<AbortSignal | undefined>): AbortSignal {
  const controller = new AbortController()
  for (const signal of signals) {
    if (!signal) continue
    if (signal.aborted) {
      controller.abort()
      break
    }
    signal.addEventListener('abort', () => controller.abort(), { once: true })
  }
  return controller.signal
}

/**
 * Pull diagnostics for a just-written file and inject them into the calling
 * agent's next pre-step context via Agent.inject. Runs fire-and-forget
 * off the write result so a write never waits on the LSP. `signal` carries
 * both the turn's cancellation and the superseding write's abort, so a stale
 * run never injects after a newer edit. Fail-open: an aborted/superseded run
 * logs at debug level; an unexpected failure logs a warning but never throws.
 */
async function runAsyncDiagnostics(
  registry: ServerRegistry,
  config: ReturnType<typeof resolveConfig>,
  logger: Logger,
  exec: ToolExecution,
  filePath: string,
  signal: AbortSignal | undefined,
): Promise<void> {
  try {
    const entries = await registry.diagnosticsFor(filePath, cwdOf(exec), signal)
    if (signal?.aborted) return
    const relevant = filterDiagnostics(entries, config.diagnosticsMinSeverity)
    if (relevant.length === 0) return
    const agent = exec.agent
    if (!agent || typeof agent.inject !== 'function') {
      logger.debug('dsh-lsp: no injectable agent for ' + filePath + ' diagnostics')
      return
    }
    agent.inject(diagnosticsMessage(filePath, relevant))
    logger.info('dsh-lsp: injected ' + relevant.length + ' diagnostic(s) for ' + filePath + ' into agent ' + agent.id)
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error)
    if (signal?.aborted) {
      // Expected: superseded by a newer write or the turn was cancelled.
      logger.debug('dsh-lsp: async diagnostics cancelled for ' + filePath + ': ' + message)
    } else {
      logger.warn('dsh-lsp: async diagnostics failed for ' + filePath + ': ' + message)
    }
  }
}

export function apply(ctx: DshContext, rawConfig: Record<string, unknown> = {}): void {
  const config = resolveConfig(rawConfig)
  const logger = makeLogger(ctx)
  const registry = new ServerRegistry(config, ctx.subprocess, logger)

  // In-flight write-diagnostics runs keyed by the agent's session id. Editing
  // one file can surface errors in OTHER files (cross-file diagnostics), so any
  // newer write aborts the previous run: only the latest run's results are
  // ever injected, never a stale snapshot.
  const diagnosticRuns = new Map<string, AbortController>()

  // Lifespan: every server subprocess belongs to this fiber. On unload / HMR
  // the disposer stops and joins all live trees, and aborts in-flight runs.
  const dispose = ctx.effect(() => {
    return () => {
      for (const controller of diagnosticRuns.values()) controller.abort()
      void registry.stopAll()
    }
  })


  ctx.systemPrompt.section({
    name: 'dsh-lsp',
    order: TOOL_SECTION_ORDER,
    text: [
      '# Language Server Protocol',
      '代码库较大, 使用 `lsp` 工具进行代码导航.',
      '查 C++ 符号时 `filePath` 优先用 `.cpp`（编译单元在 compile_commands.json 中，命中更快更全）；`.h` 也能工作但依赖后台索引预热，命中略慢. ',
      '读文件时 LSP 自动加载该文件; 写文件后 LSP 自动异步检查诊断, 结果会注入到你的上下文, 请据此修复问题.',
      'dsh-lsp插件可迭代升级',
    ].join('\n'),
  });

  // ------------------------------------------------------------------
  // Model-facing tool
  // ------------------------------------------------------------------
  ctx.tools.register({
    name: 'lsp',
    description:
      'Language-server code navigation. Query a symbol or file via operation: explore (one call: definition + type + hover + implementation + references), goToDefinition, findReferences, goToImplementation, typeDefinition, hover, workspaceSymbol (fuzzy/partial name), documentSymbol, diagnostics. filePath selects the language server by extension; line/character are 1-based UTF-16 (required for position ops and workspaceSymbol per server routing). Use /lsp to control servers.',
    parameters: {
      type: 'object',
      additionalProperties: false,
      properties: {
        operation: {
          type: 'string',
          enum: ['explore', 'goToDefinition', 'findReferences', 'goToImplementation', 'typeDefinition', 'hover', 'workspaceSymbol', 'documentSymbol', 'diagnostics'],
        },
        filePath: { type: 'string', description: 'Absolute or workspace-relative source file path; its extension selects the server.' },
        line: { type: 'integer', description: '1-based line of the cursor (position ops and workspaceSymbol).' },
        character: { type: 'integer', description: '1-based UTF-16 character of the cursor.' },
        query: { type: 'string', description: 'Symbol name for workspaceSymbol; supports partial/fuzzy names.' },
        json: { type: 'boolean', description: 'When true, also return a structured `json` projection (flattened {path,line,character,preview}) for programmatic/PTC consumption.' },
      },
      required: ['operation', 'filePath'],
    },
    output: {
      schema: {
        type: 'object',
        additionalProperties: false,
        properties: {
          ok: { type: 'boolean' },
          kind: { type: 'string' },
          text: { type: 'string' },
          json: { type: 'object', description: 'Structured result for programmatic (PTC/JS) consumption; locations/symbols flatten to {path,line,character,preview}.' },
        },
        required: ['ok', 'kind', 'text'],
      },
      render: (_args, value) => [{ type: 'text', text: String((value as { text?: string } | null)?.text ?? '') }],
    },
    async execute(rawArgs, exec) {
      const args = rawArgs as unknown as QueryArgs
      try {
        const result = await runQuery(registry, args, config, exec)
        const text = capText(renderResult(args, result, config), config.maxResultChars)
        if (args.json === true) {
          const json = toJson(result)
          return { ok: true, kind: result.kind, text, json }
        }
        return { ok: true, kind: result.kind, text }
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error)
        return { ok: false, kind: 'error', text: `lsp failed: ${message}` }
      }
    },
  })

  // ------------------------------------------------------------------
  // /lsp command
  // ------------------------------------------------------------------
  ctx.commands.register({
    name: 'lsp',
    description: 'Control language-server subprocesses: status | start [id...] | stop [id...] | restart',
    input: { hint: 'status | start [id...] | stop [id...] | restart' },
    async handler(invocation: CommandInvocation): Promise<CommandResult> {
      const raw = invocation.rawInput.trim()
      const [verb, ...rest] = raw.split(/\s+/).filter(Boolean)
      // Empty input (bare `/lsp` or a picked no-arg invocation) defaults to status.
      const effectiveVerb = verb === undefined || verb === '' ? 'status' : verb
      const ids = rest.filter(Boolean)
      const cwd = undefined // command handlers do not carry session cwd; use process.cwd()

      try {
        switch (effectiveVerb) {
          case 'status': {
            const rows = registry.status()
            if (rows.length === 0) return { kind: 'success', text: 'No LSP servers configured.' }
            const lines = rows.map(r => {
              const root = r.root ?? '-'
              const pid = r.pid ?? '-'
              const err = r.error ? ` (${r.error})` : ''
              return `- ${r.id}: ${r.state}${err}\n    languageId=${r.languageId}, extensions=[${r.extensions.join(', ')}], root=${root}, pid=${pid}`
            })
            return { kind: 'success', text: `LSP servers:\n${lines.join('\n')}` }
          }
          case 'start': {
            const targets = ids.length > 0 ? ids : registry.servers.map(s => s.id)
            const results: string[] = []
            for (const id of targets) {
              try {
                await registry.startById(id, cwd)
                results.push(`- ${id}: started`)
              } catch (error) {
                results.push(`- ${id}: ${error instanceof Error ? error.message : String(error)}`)
              }
            }
            return { kind: 'success', text: results.join('\n') }
          }
          case 'stop': {
            const targets = ids.length > 0 ? ids : registry.servers.map(s => s.id)
            for (const id of targets) {
              await registry.stopById(id)
            }
            return { kind: 'success', text: `stopped: ${targets.join(', ')}` }
          }
          case 'restart': {
            const targets = ids.length > 0 ? ids : registry.servers.map(s => s.id)
            for (const id of targets) {
              await registry.stopById(id)
            }
            const results: string[] = []
            for (const id of targets) {
              try {
                await registry.startById(id, cwd)
                results.push(`- ${id}: restarted`)
              } catch (error) {
                results.push(`- ${id}: ${error instanceof Error ? error.message : String(error)}`)
              }
            }
            return { kind: 'success', text: results.join('\n') }
          }
          default:
            return { kind: 'error', text: `unknown /lsp subcommand '${effectiveVerb}'; expected status | start | stop | restart` }
        }
      } catch (error) {
        return { kind: 'error', text: `/lsp failed: ${error instanceof Error ? error.message : String(error)}` }
      }
    },
  })

  // ------------------------------------------------------------------
  // File-access integration
  // ------------------------------------------------------------------
  // Synchronously didOpen files the AI reads (warm servers) so the document
  // is queryable the moment the read result returns; after a write, pull
  // diagnostics in the background and inject findings into the agent's next
  // pre-step context. A superseding write by the same agent aborts the
  // in-flight run so stale results never pollute a newer edit.
  ctx.on('tools/post-execute', async (exec, result, next) => {
    try {
      const args = (exec.arguments ?? {}) as Record<string, unknown>
      const filePath = typeof args.file_path === 'string' ? args.file_path.trim() : ''
      if (!filePath) return next()
      if (exec.name === 'read' && config.syncLoadOnRead) {
        if (registry.serverRunning(filePath)) {
          // Warm server: didOpen before the read result reaches the model.
          await registry.openFile(filePath, cwdOf(exec), exec.signal)
        } else {
          // Cold server: warm it in the background; never stall a read.
          void registry.openFile(filePath, cwdOf(exec), exec.signal).catch((error) => {
            logger.debug('dsh-lsp: background open failed for ' + filePath + ': ' + (error instanceof Error ? error.message : String(error)))
          })
        }
      } else if (config.diagnosticsOnWrite && (exec.name === 'write' || exec.name === 'edit')) {
        const agent = exec.agent
        // Key by session id, not file: a change to one file can break others,
        // so every in-flight run for this agent must give way to the newest.
        const key = agent ? String(agent.id) : ''
        // Supersede any in-flight diagnostics run for this agent.
        const previous = diagnosticRuns.get(key)
        if (previous) previous.abort()
        const controller = new AbortController()
        diagnosticRuns.set(key, controller)
        void runAsyncDiagnostics(
          registry,
          config,
          logger,
          exec,
          filePath,
          mergeAbortSignals(exec.signal, controller.signal),
        ).finally(() => {
          if (diagnosticRuns.get(key) === controller) diagnosticRuns.delete(key)
        })
      }
    } catch (error) {
      // File-access integration must never break a tool call, but the failure
      // should be visible in the lsp log rather than silently swallowed.
      logger.debug('dsh-lsp: file-access integration error: ' + (error instanceof Error ? error.message : String(error)))
    }
    return next()
  }, { global: true })
}
