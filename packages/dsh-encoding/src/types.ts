/**
 * Internal vocabulary for dsh-encoding.
 *
 * Harness-facing types are the official ones (`@deepseek-ai/cordis`
 * Context/Logger and `@deepseek-ai/dsh-tools`
 * ToolExecution/ToolExecutionResult), so the two hook verbs this plugin
 * subscribes to are checked against the real event map. The plugin has no
 * runtime dependencies at all.
 *
 * @module dsh-encoding/types
 */

import type { Context, Logger } from '@deepseek-ai/cordis'
import type { ToolExecution, ToolExecutionResult } from '@deepseek-ai/dsh-tools'
import type { GlobSet } from './glob.js'
import type { BomPolicy } from './encoding.js'

export type { Context, Logger, ToolExecution, ToolExecutionResult }
export type { BomPolicy }

/** Which terminator newly written lines receive. */
export type EolPolicy = 'crlf' | 'lf' | 'preserve'

/** Log verbosity for the plugin's own diagnostics. */
export type LogLevel = 'off' | 'summary' | 'debug'

/**
 * A per-file-type policy patch.
 *
 * The C4819 rule that forces a BOM onto non-ASCII C/C++ sources does not apply
 * to CMake, which MSVC never compiles - so the two families need different BOM
 * policies, and this is where that difference lives.
 */
export interface EncodingOverride {
  /** Globs selecting the files this override applies to. */
  match: readonly string[]
  /** Replaces `bom` for matching files. */
  bom?: BomPolicy
  /** Replaces `eol` for matching files. */
  eol?: EolPolicy
}

/** Row config as written in cordis.patch.yml; every field is optional. */
export interface EncodingConfig {
  /** Master switch. Default true. */
  enabled?: boolean
  /** Tool names whose file writes trigger the repair. Default ['write', 'edit']. */
  tools?: readonly string[]
  /** Glob patterns selecting which files the repair applies to. */
  include?: readonly string[]
  /** Glob patterns excluded from the repair; wins over include. */
  exclude?: readonly string[]
  /** BOM policy for files no override matches. Default 'non-ascii'. */
  bom?: BomPolicy
  /** Terminator for newly written lines. Default 'crlf'. */
  eol?: EolPolicy
  /** Per-file-type policy patches; first match wins, field by field. */
  overrides?: readonly EncodingOverride[]
  /** Keep the pre-write terminator of every line the write did not change. Default true. */
  preserveUntouchedEol?: boolean
  /** Never leave a bare CR behind (it triggers MSVC C4335). Default true. */
  fixLoneCr?: boolean
  /** Upper bound on remembered pre-write snapshots. Default 64. */
  maxSnapshotEntries?: number
  /** How long a pre-write snapshot stays usable. Default 300000 (5 min). */
  snapshotTtlMs?: number
  /** Log verbosity. Default 'summary'. */
  log?: LogLevel
  /** Compute the repair but do not write it back; log what would change. Default false. */
  dryRun?: boolean
}

/** One compiled override. */
export interface ResolvedOverride {
  readonly match: GlobSet
  readonly bom?: BomPolicy
  readonly eol?: EolPolicy
}

/** EncodingConfig with every default filled in and the globs compiled. */
export interface ResolvedConfig {
  readonly enabled: boolean
  readonly tools: readonly string[]
  readonly include: readonly string[]
  readonly exclude: readonly string[]
  readonly includeSet: GlobSet
  readonly excludeSet: GlobSet
  readonly bom: BomPolicy
  readonly eol: EolPolicy
  readonly overrides: readonly ResolvedOverride[]
  readonly preserveUntouchedEol: boolean
  readonly fixLoneCr: boolean
  readonly maxSnapshotEntries: number
  readonly snapshotTtlMs: number
  readonly log: LogLevel
  readonly dryRun: boolean
}

/**
 * Everything the repair needs for one concrete file, with overrides already
 * folded in. Passing this instead of the whole config keeps repairBytes free of
 * path logic.
 */
export interface FilePolicy {
  readonly bom: BomPolicy
  readonly eol: EolPolicy
  readonly preserveUntouchedEol: boolean
  readonly fixLoneCr: boolean
}

/** What one repair pass did (or would have done, under dryRun). */
export interface RepairOutcome {
  /** Absolute path that was inspected. */
  readonly path: string
  /** true when the file on disk was rewritten. */
  readonly wrote: boolean
  /** true when the repair added a UTF-8 BOM that was missing. */
  readonly bomAdded: boolean
  /** Number of lines whose terminator was changed. */
  readonly endingsRewritten: number
  /** Short human-readable reason when nothing was done. */
  readonly skipped?: string
}
