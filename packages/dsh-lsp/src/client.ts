/**
 * LSP server client: one managed subprocess + one JSON-RPC protocol
 * connection.
 *
 * Process management rides the harness's own `ctx.subprocess` seam (the same
 * tree-scoped spawn/terminate primitive the bash executor uses), so helper
 * processes cannot outlive the server and teardown is a single `terminate()`.
 * Protocol framing uses `vscode-jsonrpc`'s stream readers/writers, and every
 * request type is the type-safe `vscode-languageserver-protocol` constant.
 */

import { createProtocolConnection } from 'vscode-languageserver-protocol/node'
import {
  DefinitionRequest,
  DidCloseTextDocumentNotification,
  DidOpenTextDocumentNotification,
  DocumentDiagnosticRequest,
  DocumentSymbolRequest,
  ExitNotification,
  HoverRequest,
  ImplementationRequest,
  InitializeRequest,
  InitializedNotification,
  ReferencesRequest,
  ShutdownRequest,
  TypeDefinitionRequest,
  WorkspaceSymbolRequest,
} from 'vscode-languageserver-protocol/node'
import type {
  CancellationToken,
  ClientCapabilities,
  Diagnostic,
  DocumentDiagnosticParams,
  DocumentSymbol,
  Hover,
  InitializeParams,
  Location,
  LocationLink,
  ProtocolConnection,
  SymbolInformation,
  WorkspaceSymbol,
} from 'vscode-languageserver-protocol'
import { appendFileSync, mkdirSync, readFileSync } from 'node:fs'
import { homedir } from 'node:os'
import { isAbsolute, join, resolve } from 'node:path'

import { pathToFileUri, toLspPosition } from './protocol.js'
import type { LoggerLike, ServerSpec, SubprocessHandle, SubprocessRuntime } from './types.js'

export type LspClientState = 'stopped' | 'starting' | 'running' | 'failed'

/** The three capabilities the plugin actually queries. */
export const CLIENT_CAPABILITIES = {
  workspace: {
    workspaceFolders: true,
  },
  textDocument: {
    hover: { contentFormat: ['markdown', 'plaintext'] },
    definition: { linkSupport: true },
    typeDefinition: { linkSupport: true },
    implementation: { linkSupport: true },
    references: {},
    documentSymbol: {
      hierarchicalDocumentSymbolSupport: true,
    },
    diagnostic: {},
    publishDiagnostics: {},
  },
} as const

interface InitializationResult {
  serverInfo?: { name?: string; version?: string }
}

export class LspClient {
  readonly spec: ServerSpec
  private readonly subprocess: SubprocessRuntime
  private readonly logger: LoggerLike
  private readonly initializeResult: InitializationResult

  state: LspClientState = 'stopped'
  root: string | null = null
  pid: number | null = null
  error: string | null = null

  private connection: ProtocolConnection | null = null
  private handle: SubprocessHandle | null = null
  private initialized = false
  private nextId = 1
  private readonly logDir?: string

  constructor(
    spec: ServerSpec,
    subprocess: SubprocessRuntime,
    logger: LoggerLike,
    initializeResult: InitializationResult = {},
    logDir?: string,
  ) {
    this.spec = spec
    this.subprocess = subprocess
    this.logger = logger
    this.initializeResult = initializeResult
    this.logDir = logDir
  }

  /**
   * Resolve a possibly-relative file path to an absolute one. Queries carry a
   * `filePath` that may be workspace-relative (the tool accepts either form);
   * the server is spawned with cwd=root, so a bare relative path would resolve
   * against the wrong directory inside `readFileSync`/`pathToFileUri` and
   * produce a malformed `file:///Coding/...` URI that crashes clangd.
   */
  private absPath(filePath: string): string {
    if (isAbsolute(filePath)) return filePath
    const base = this.root ?? process.cwd()
    return resolve(base, filePath)
  }

  /** Start the server subprocess and complete the initialize handshake. */
  async start(root: string, signal?: AbortSignal): Promise<void> {
    if (this.state === 'running' || this.state === 'starting') return
    this.state = 'starting'
    this.root = root
    this.error = null

    try {
      const handle = this.subprocess.spawn({
        argv: [this.spec.command, ...this.spec.args],
        cwd: root,
        stdio: {
          stdin: 'pipe',
          stdout: 'pipe',
          stderr: { maxBytes: 64 * 1024 },
        },
        graceMs: 5000,
        ...(signal ? { signal } : {}),
      })
      this.handle = handle
      this.pid = handle.pid

      // A process that exits before initialization fails the start.
      handle.done.then((outcome) => {
        if (this.state === 'starting') {
          this.state = 'failed'
          this.error = `server exited before initialization (exitCode=${outcome.exitCode}, signal=${outcome.signal})`
        }
      }).catch((err) => {
        if (this.state === 'starting') {
          this.state = 'failed'
          this.error = `spawn failed: ${err instanceof Error ? err.message : String(err)}`
        }
      })

      if (!handle.stdout || !handle.stdin) {
        throw new Error('server stdio did not expose piped streams')
      }

      const connection = createProtocolConnection(handle.stdout, handle.stdin)
      this.connection = connection
      connection.listen()

      const rootUri = pathToFileUri(root)
      const initParams: InitializeParams = {
        processId: process.pid,
        rootUri,
        workspaceFolders: [{ uri: rootUri, name: root }],
        capabilities: CLIENT_CAPABILITIES as unknown as ClientCapabilities,
        initializationOptions: undefined,
      }

      const result = (await connection.sendRequest(InitializeRequest.type, initParams)) as {
        serverInfo?: { name?: string; version?: string }
      }
      this.initializeResult.serverInfo = result?.serverInfo
      await connection.sendNotification(InitializedNotification.type, {})
      this.initialized = true
      this.state = 'running'
      this.error = null

      // Surface an unexpected exit so the registry can drop the instance and
      // the next query restarts it. Persist the server's stderr tail to the
      // log directory so a crash (e.g. clangd exit code 1) is diagnosable:
      // without it, the fatal error clangd wrote to stderr is silently lost.
      handle.done.then((outcome) => {
        if (this.state === 'running') {
          this.state = 'failed'
          const stderrText = this.drainStderr()
          if (stderrText) this.writeLog(stderrText)
          this.error = `server exited unexpectedly (exitCode=${outcome.exitCode}, signal=${outcome.signal})`
          this.logger.warn(`dsh-lsp: ${this.spec.id} exited: ${this.error}${stderrText ? `\n${stderrText.slice(0, 2000)}` : ''}`)
        }
      }).catch(() => { /* handled on the starting path */ })
    } catch (err) {
      this.state = 'failed'
      this.error = err instanceof Error ? err.message : String(err)
      this.logger.warn(`dsh-lsp: failed to start ${this.spec.id}: ${this.error}`)
      // Best-effort cleanup of a half-started process.
      try {
        this.handle?.terminate()
      } catch {
        /* ignore */
      }
      throw err
    }
  }

  private requireConnection(): ProtocolConnection {
    if (!this.connection || !this.initialized || this.state !== 'running') {
      throw new Error(`server '${this.spec.id}' is not running`)
    }
    return this.connection
  }

  /**
   * Ensure a document is open on the server. LSP servers (clangd included)
   * answer position-sensitive queries only for documents they know; without
   * `textDocument/didOpen`, clangd rejects hover/typeDefinition with
   * `trying to get AST for non-added document`. Documents are opened lazily
   * and kept open for the server's lifetime (cheap, and matches how an editor
   * keeps buffers resident).
   */
  private readonly openDocuments = new Set<string>()

  private ensureOpen(filePath: string): void {
    if (this.openDocuments.has(filePath)) return
    const uri = pathToFileUri(filePath)
    let text: string
    try {
      text = readFileSync(filePath, 'utf8')
    } catch {
      // The file may be unsaved/missing; open with empty content so the
      // server still has a buffer. Queries will simply return no result.
      text = ''
    }
    const conn = this.requireConnection()
    void conn.sendNotification(DidOpenTextDocumentNotification.type, {
      textDocument: {
        uri,
        languageId: this.spec.languageId,
        version: 1,
        text,
      },
    })
    this.openDocuments.add(filePath)
  }

  private token(signal?: AbortSignal): CancellationToken {
    if (!signal) return { get isCancellationRequested() { return false }, onCancellationRequested() { return { dispose() {} } } }
    return {
      get isCancellationRequested() { return signal.aborted },
      onCancellationRequested(listener) {
        if (signal.aborted) {
          listener(new Error('aborted'))
          return { dispose() {} }
        }
        const handler = () => listener(new Error('aborted'))
        signal.addEventListener('abort', handler, { once: true })
        return { dispose() { signal.removeEventListener('abort', handler) } }
      },
    }
  }

  private async normalizeLocations(
    value: Location | Location[] | LocationLink[] | null,
  ): Promise<Array<{ uri: string; range: import('vscode-languageserver-protocol').Range }>> {
    if (value === null || value === undefined) return []
    const arr: Array<Location | LocationLink> = Array.isArray(value) ? value : [value]
    const out: Array<{ uri: string; range: import('vscode-languageserver-protocol').Range }> = []
    for (const item of arr) {
      if ('targetUri' in item && item.targetUri) {
        // LocationLink
        out.push({ uri: item.targetUri, range: item.targetSelectionRange })
      } else if ('uri' in item && item.uri) {
        out.push({ uri: item.uri, range: item.range })
      }
    }
    return out
  }

  async definition(filePath: string, line: number, character: number, signal?: AbortSignal) {
    const abs = this.absPath(filePath)
    this.ensureOpen(abs)
    const conn = this.requireConnection()
    const params = {
      textDocument: { uri: pathToFileUri(abs) },
      position: toLspPosition(line, character),
    }
    const value = await conn.sendRequest(DefinitionRequest.type, params, this.token(signal))
    return this.normalizeLocations(value as Location | Location[] | LocationLink[] | null)
  }

  async typeDefinition(filePath: string, line: number, character: number, signal?: AbortSignal) {
    const abs = this.absPath(filePath)
    this.ensureOpen(abs)
    const conn = this.requireConnection()
    const params = {
      textDocument: { uri: pathToFileUri(abs) },
      position: toLspPosition(line, character),
    }
    const value = await conn.sendRequest(TypeDefinitionRequest.type, params, this.token(signal))
    return this.normalizeLocations(value as Location | Location[] | LocationLink[] | null)
  }

  async implementation(filePath: string, line: number, character: number, signal?: AbortSignal) {
    const abs = this.absPath(filePath)
    this.ensureOpen(abs)
    const conn = this.requireConnection()
    const params = {
      textDocument: { uri: pathToFileUri(abs) },
      position: toLspPosition(line, character),
    }
    const value = await conn.sendRequest(ImplementationRequest.type, params, this.token(signal))
    return this.normalizeLocations(value as Location | Location[] | LocationLink[] | null)
  }

  async references(filePath: string, line: number, character: number, signal?: AbortSignal) {
    const abs = this.absPath(filePath)
    this.ensureOpen(abs)
    const conn = this.requireConnection()
    const params = {
      textDocument: { uri: pathToFileUri(abs) },
      position: toLspPosition(line, character),
      context: { includeDeclaration: true },
    }
    const value = await conn.sendRequest(ReferencesRequest.type, params, this.token(signal))
    const arr: Array<Location> = Array.isArray(value) ? (value as Location[]) : []
    return arr.map(loc => ({ uri: loc.uri, range: loc.range }))
  }

  async hover(filePath: string, line: number, character: number, signal?: AbortSignal): Promise<Hover | null> {
    const abs = this.absPath(filePath)
    this.ensureOpen(abs)
    const conn = this.requireConnection()
    const params = {
      textDocument: { uri: pathToFileUri(abs) },
      position: toLspPosition(line, character),
    }
    return conn.sendRequest(HoverRequest.type, params, this.token(signal))
  }

  async workspaceSymbol(filePath: string, query: string, signal?: AbortSignal): Promise<SymbolInformation[] | WorkspaceSymbol[] | null> {
    // Warm the dynamic index before querying. clangd's workspace/symbol only
    // searches its Dex index, which is dynamic (didOpen'd files) plus the
    // background index. On a cold start the 1.5 GB background index loads
    // slowly (140s+ of "Failed to load shard" before it answers), but
    // didOpen'ing the target file and forcing an AST parse fills the dynamic
    // index immediately. A documentSymbol round-trip is the parse barrier: it
    // returns only once the translation unit's AST is ready, after which the
    // file's symbols are queryable.
    try {
      await this.documentSymbol(filePath, signal)
    } catch {
      // A broken translation unit may fail the parse; still attempt the query.
    }
    const conn = this.requireConnection()
    return conn.sendRequest(WorkspaceSymbolRequest.type, { query }, this.token(signal))
  }

  async documentSymbol(filePath: string, signal?: AbortSignal): Promise<DocumentSymbol[] | SymbolInformation[] | null> {
    const abs = this.absPath(filePath)
    this.ensureOpen(abs)
    const conn = this.requireConnection()
    return conn.sendRequest(DocumentSymbolRequest.type, { textDocument: { uri: pathToFileUri(abs) } }, this.token(signal))
  }

  async diagnostics(filePath: string, signal?: AbortSignal): Promise<Diagnostic[] | null> {
    const abs = this.absPath(filePath)
    this.ensureOpen(abs)
    const conn = this.requireConnection()
    const params: DocumentDiagnosticParams = {
      textDocument: { uri: pathToFileUri(abs) },
      identifier: `dsh-lsp-${this.nextId++}`,
      previousResultId: undefined,
    }
    const report = await conn.sendRequest(DocumentDiagnosticRequest.type, params, this.token(signal))
    if (!report) return null
    if (report.kind === 'full') return report.items ?? []
    // 'unchanged' reports carry no items; return empty rather than erroring.
    return []
  }

  /** Drain the subprocess's collected stderr tail (if any) to a string. */
  private drainStderr(): string {
    const stderr = this.handle?.collected?.stderr
    if (!stderr) return ''
    try {
      const { text } = stderr.finalize()
      return text.trim()
    } catch {
      return ''
    }
  }

  /** Resolve the per-server log file path and append one stderr snapshot. */
  private writeLog(text: string): void {
    if (!this.logDir || !text) return
    try {
      const dir = this.logDir.replace(/^~(?=$|[\\/])/, homedir())
      mkdirSync(dir, { recursive: true })
      const safeId = this.spec.id.replace(/[^a-zA-Z0-9_-]/g, '_')
      const file = join(dir, `${safeId}.log`)
      appendFileSync(file, `\n--- ${new Date().toISOString()} (pid=${this.pid}) ---\n${text}\n`)
    } catch {
      /* logging is best-effort; never break server teardown on a log failure */
    }
  }

  /** Graceful shutdown: `shutdown` request, `exit` notification, then tree termination. */
  async stop(): Promise<void> {
    if (this.state === 'stopped') return
    // Persist whatever stderr was collected before teardown completes.
    const stderrText = this.drainStderr()
    if (stderrText) this.writeLog(stderrText)
    const conn = this.connection
    if (conn && this.initialized) {
      try {
        await conn.sendRequest(ShutdownRequest.type, undefined)
        await conn.sendNotification(ExitNotification.type)
      } catch {
        /* server may already be gone */
      }
    }
    try {
      this.connection?.dispose()
    } catch {
      /* ignore */
    }
    try {
      this.handle?.terminate()
    } catch {
      /* ignore */
    }
    this.connection = null
    this.handle = null
    this.initialized = false
    this.pid = null
    this.state = 'stopped'
    this.error = null
  }
}
