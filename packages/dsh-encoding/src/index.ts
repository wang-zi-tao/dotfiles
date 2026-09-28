/**
 * dsh-encoding — post-write source encoding repair for DeepSeek Harness.
 *
 * The harness file tools produce two invisible side effects on C/C++/CMake
 * sources:
 *
 *  1. the UTF-8 BOM is dropped, because the write path decodes through
 *     `TextDecoder` (which strips U+FEFF) and writes the decoded text back;
 *  2. `edit` re-applies the file's *majority* terminator to the *whole* file,
 *     so a single edit flips every minority-terminator line and the diff
 *     explodes.
 *
 * Both matter in a WPS-sized tree: a C/C++ source with non-ASCII and no BOM
 * raises C4819, and `/WX` turns that into C2220; a bare CR raises C4335 and
 * lands in the same place. The repair is therefore:
 *
 *  * snapshot the target's raw bytes in `tools/pre-execute` (the only hook that
 *    still sees the pre-write file);
 *  * rewrite them in `tools/post-execute` — add the BOM a non-ASCII source
 *    needs, give newly written lines the configured terminator, and put back the
 *    original terminator on every line the write did not actually change.
 *
 * Both hooks register with `{ global: true }` so writes made by a subagent are
 * repaired too. The plugin never throws into the tool pipeline and never touches
 * a file it cannot round-trip through strict UTF-8.
 *
 * @module dsh-encoding
 */

import { readFile, writeFile } from 'node:fs/promises'
import { isAbsolute, resolve as resolveAbsolute } from 'node:path'

import { isSelected, resolveConfig, resolveFilePolicy } from './config.js'
import {
  countRewrittenEndings,
  decideBom,
  decodeUtf8,
  encodeUtf8,
  looksBinary,
  majorityEol,
  repairLineEndings,
  sniffBom,
  splitLines,
  type EolPlan,
} from './encoding.js'
import { SnapshotStore } from './snapshot.js'
import type {
  Context,
  FilePolicy,
  Logger,
  RepairOutcome,
  ResolvedConfig,
  ToolExecution,
  ToolExecutionResult,
} from './types.js'

export const name = 'dsh-encoding'
export const inject = ['tools']

export { resolveConfig, resolveFilePolicy, isSelected } from './config.js'
export { GlobSet, globToRegExpSource } from './glob.js'
export { SnapshotStore } from './snapshot.js'
export {
  countRewrittenEndings,
  decideBom,
  decodeUtf8,
  encodeUtf8,
  hasNonAscii,
  joinLines,
  looksBinary,
  majorityEol,
  repairLineEndings,
  sniffBom,
  splitLines,
  UTF8_BOM,
} from './encoding.js'
export type {
  BomPolicy,
  EncodingConfig,
  EncodingOverride,
  EolPolicy,
  FilePolicy,
  LogLevel,
  RepairOutcome,
  ResolvedConfig,
} from './types.js'

const FILE_PATH_ARGUMENT = 'file_path'

// ---------------------------------------------------------------------------
// Harness plumbing
// ---------------------------------------------------------------------------

interface AgentShape {
  readonly session?: { readonly header?: { readonly cwd?: string } }
}

function makeLogger(ctx: Context): Logger {
  try {
    return ctx.logger('encoding')
  } catch {
    /* fall through to a no-op facade */
  }
  const noop = (): void => {}
  return { debug: noop, info: noop, warn: noop, error: noop } as unknown as Logger
}

function describe(error: unknown): string {
  return error instanceof Error ? error.message : String(error)
}

/** The `file_path` argument of a `write`/`edit` call, when present and usable. */
export function argumentPath(exec: ToolExecution): string | undefined {
  const args: unknown = exec.arguments
  if (args === null || typeof args !== 'object') return undefined
  const value = (args as Record<string, unknown>)[FILE_PATH_ARGUMENT]
  return typeof value === 'string' && value.trim() !== '' ? value.trim() : undefined
}

/** The canonical result path a successful `write`/`edit` reports. */
export function resultPath(result: Readonly<ToolExecutionResult>): string | undefined {
  if (result.isError === true) return undefined
  const value: unknown = result.value
  if (value === null || typeof value !== 'object') return undefined
  const path = (value as Record<string, unknown>).path
  return typeof path === 'string' && path.trim() !== '' ? path.trim() : undefined
}

/** Session working directory, which relative tool paths resolve against. */
export function sessionCwd(exec: ToolExecution): string | undefined {
  const agent = exec.agent as AgentShape | undefined
  const cwd = agent?.session?.header?.cwd
  return typeof cwd === 'string' && cwd.trim() !== '' ? cwd : undefined
}

/** Absolute form of a tool path, using the session cwd like the fs backend does. */
export function toAbsolute(filePath: string, cwd: string | undefined): string {
  return isAbsolute(filePath) ? filePath : resolveAbsolute(cwd ?? process.cwd(), filePath)
}

/** Case-folded store key; Windows paths are case-insensitive. */
export function snapshotKey(filePath: string): string {
  const normalized = filePath.replace(/\\/g, '/')
  return process.platform === 'win32' ? normalized.toLowerCase() : normalized
}

// ---------------------------------------------------------------------------
// Pure repair core
// ---------------------------------------------------------------------------

export interface RepairResult {
  /** Exactly the bytes the file should hold. */
  readonly bytes: Uint8Array
  /** `true` when {@link RepairResult.bytes} differs from the input. */
  readonly changed: boolean
  /** `true` when the repair adds a BOM the file did not have. */
  readonly bomAdded: boolean
  /** Number of lines whose terminator the repair changes. */
  readonly endingsRewritten: number
}

function sameBytes(left: Uint8Array, right: Uint8Array): boolean {
  if (left.length !== right.length) return false
  for (let index = 0; index < left.length; index += 1) {
    if (left[index] !== right[index]) return false
  }
  return true
}

/**
 * Compute the repaired bytes for one file. Returns `undefined` for anything
 * this plugin must not touch: UTF-16 content, binaries, or text that is not
 * valid UTF-8 (re-encoding those would corrupt them).
 *
 * @param raw         current bytes on disk (what the tool just wrote)
 * @param originalRaw pre-write bytes, or `null` when the file did not exist or
 *                    no snapshot was taken
 * @param policy      the effective policy for this path, overrides already folded in
 */
export function repairBytes(
  raw: Uint8Array,
  originalRaw: Uint8Array | null,
  policy: FilePolicy,
): RepairResult | undefined {
  const sniff = sniffBom(raw)
  if (sniff.kind === 'utf16le' || sniff.kind === 'utf16be') return undefined
  if (looksBinary(raw)) return undefined
  const text = decodeUtf8(raw.subarray(sniff.length))
  if (text === undefined) return undefined

  let originalText: string | null = null
  let originalHadBom = sniff.kind === 'utf8'
  if (originalRaw !== null) {
    const originalSniff = sniffBom(originalRaw)
    if ((originalSniff.kind === 'utf8' || originalSniff.kind === 'none') && !looksBinary(originalRaw)) {
      const decoded = decodeUtf8(originalRaw.subarray(originalSniff.length))
      if (decoded !== undefined) {
        originalText = decoded
        originalHadBom = originalSniff.kind === 'utf8'
      }
    }
  }

  const target: '\r\n' | '\n' =
    policy.eol === 'lf'
      ? '\n'
      : policy.eol === 'preserve'
        ? originalText === null
          ? '\r\n'
          : majorityEol(splitLines(originalText))
        : '\r\n'

  const plan: EolPlan = {
    target,
    preserveUntouched: policy.preserveUntouchedEol,
    fixLoneCr: policy.fixLoneCr,
  }
  const repairedText = repairLineEndings(text, originalText, plan)
  const withBom = decideBom(policy.bom, repairedText, originalHadBom)
  const bytes = encodeUtf8(repairedText, withBom)

  // Refuse to write anything that does not decode back to exactly what was
  // computed; a silent encoding corruption in a source tree is unrecoverable.
  if (decodeUtf8(bytes.subarray(withBom ? 3 : 0)) !== repairedText) return undefined

  return {
    bytes,
    changed: !sameBytes(bytes, raw),
    bomAdded: withBom && sniff.kind !== 'utf8',
    endingsRewritten: countRewrittenEndings(text, repairedText),
  }
}

// ---------------------------------------------------------------------------
// Plugin entry
// ---------------------------------------------------------------------------

function takeSnapshot(
  store: SnapshotStore,
  keys: readonly string[],
): { readonly found: boolean; readonly bytes: Uint8Array | null } {
  for (const key of keys) {
    const entry = store.take(key)
    if (entry !== undefined) return { found: true, bytes: entry.bytes }
  }
  return { found: false, bytes: null }
}

async function readBytes(filePath: string): Promise<Uint8Array | null> {
  try {
    return new Uint8Array(await readFile(filePath))
  } catch {
    return null
  }
}

export function apply(ctx: Context, rawConfig: Record<string, unknown> = {}): void {
  const config = resolveConfig(rawConfig)
  const logger = makeLogger(ctx)
  const snapshots = new SnapshotStore(config.maxSnapshotEntries, config.snapshotTtlMs)

  if (config.log !== 'off') {
    logger.info(
      'dsh-encoding: ' +
        `enabled=${config.enabled} bom=${config.bom} eol=${config.eol} ` +
        `preserveUntouchedEol=${config.preserveUntouchedEol} fixLoneCr=${config.fixLoneCr} ` +
        `tools=${config.tools.join(',')} include=${config.include.length} exclude=${config.exclude.length} ` +
        `overrides=${config.overrides.length}` +
        (config.dryRun ? ' dryRun=true' : ''),
    )
  }

  const handles = (toolName: string): boolean => config.enabled && config.tools.includes(toolName)

  // The pre-write bytes are the only record of the original terminators and the
  // original BOM: the tool result's own `before`/`after` are LF-normalized and
  // BOM-stripped by construction.
  ctx.on(
    'tools/pre-execute',
    async (exec, next) => {
      try {
        if (handles(exec.name)) {
          const argPath = argumentPath(exec)
          if (argPath !== undefined) {
            const target = toAbsolute(argPath, sessionCwd(exec))
            if (isSelected(config, target)) snapshots.put(snapshotKey(target), await readBytes(target))
          }
        }
      } catch (error) {
        logger.debug(`dsh-encoding: snapshot failed: ${describe(error)}`)
      }
      return next()
    },
    { global: true },
  )

  ctx.on(
    'tools/post-execute',
    async (exec, result, next) => {
      try {
        if (handles(exec.name) && result.isError !== true) {
          await repairWrittenFile(exec, result, config, snapshots, logger)
        }
      } catch (error) {
        logger.warn(`dsh-encoding: repair failed: ${describe(error)}`)
      }
      return next()
    },
    { global: true },
  )
}

async function repairWrittenFile(
  exec: ToolExecution,
  result: Readonly<ToolExecutionResult>,
  config: ResolvedConfig,
  snapshots: SnapshotStore,
  logger: Logger,
): Promise<void> {
  const argPath = argumentPath(exec)
  const reported = resultPath(result)
  if (argPath === undefined && reported === undefined) return

  const cwd = sessionCwd(exec)
  const target = toAbsolute(reported ?? argPath!, cwd)
  if (!isSelected(config, target)) return

  const keys = [snapshotKey(target)]
  if (argPath !== undefined) {
    const fromArgument = snapshotKey(toAbsolute(argPath, cwd))
    if (!keys.includes(fromArgument)) keys.push(fromArgument)
  }
  const snapshot = takeSnapshot(snapshots, keys)
  const originalRaw = snapshot.found ? snapshot.bytes : null

  const raw = await readFile(target)
  const policy = resolveFilePolicy(config, target)
  const outcome = repairBytes(raw, originalRaw, policy)
  if (outcome === undefined) {
    if (config.log === 'debug') logger.debug(`dsh-encoding: skipped ${target} (not repairable UTF-8 text)`)
    return
  }
  if (!outcome.changed) {
    if (config.log === 'debug') logger.debug(`dsh-encoding: ${target} already conforms`)
    return
  }

  const summary =
    (outcome.bomAdded ? '+BOM, ' : '') +
    `${outcome.endingsRewritten} terminator(s) normalized, bom=${policy.bom}, eol=${policy.eol}`

  if (config.dryRun) {
    logger.info(`dsh-encoding: would repair ${target} (${summary})`)
    return
  }

  await writeFile(target, outcome.bytes)
  if (config.log !== 'off') logger.info(`dsh-encoding: repaired ${target} (${summary})`)
}

/** Build a {@link RepairOutcome} for a caller that wants the decision, not the I/O. */
export function planRepair(
  target: string,
  raw: Uint8Array,
  originalRaw: Uint8Array | null,
  config: ResolvedConfig,
): RepairOutcome | undefined {
  const outcome = repairBytes(raw, originalRaw, resolveFilePolicy(config, target))
  if (outcome === undefined) return { path: target, wrote: false, bomAdded: false, endingsRewritten: 0, skipped: 'not repairable UTF-8 text' }
  if (!outcome.changed) return { path: target, wrote: false, bomAdded: false, endingsRewritten: 0, skipped: 'already conforms' }
  return {
    path: target,
    wrote: true,
    bomAdded: outcome.bomAdded,
    endingsRewritten: outcome.endingsRewritten,
  }
}
