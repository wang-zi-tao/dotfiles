/**
 * Internal vocabulary for dsh-lsp.
 *
 * The harness-facing types come from the official `@deepseek-ai/dsh-*`
 * packages: cordis `Context`/`Logger`, dsh-tools
 * `ToolDefinition`/`ToolRunContext`, dsh-commands
 * `CommandDefinition`/`CommandInvocation`/`CommandResult`, and dsh-session
 * `Session`. The one custom seam kept is `ctx.subprocess`, which the official
 * `Context` does not carry; `DshContext` intersects it onto `Context`.
 * Runtime dependencies remain limited to `vscode-languageserver-protocol`.
 */

import type { Context, Logger } from '@deepseek-ai/cordis'
import type { ToolDefinition, ToolRunContext } from '@deepseek-ai/dsh-tools'
import type { CommandDefinition, CommandInvocation, CommandResult } from '@deepseek-ai/dsh-commands'
import type { Session } from '@deepseek-ai/dsh-session'
import type { ContextFormed } from '@deepseek-ai/dsh-llm'

export type {
  Logger,
  ToolDefinition,
  ToolRunContext,
  CommandDefinition,
  CommandInvocation,
  CommandResult,
  Session,
}

/**
 * This plugin's own injected-context kind. DSH 0.1.7 dropped the shared
 * catch-all `plugin` source: every producer declares its kind by merging into
 * the harness's `MessageSourceMap`.
 */
declare module '@deepseek-ai/dsh-llm' {
  interface MessageSourceMap {
    'dsh-lsp': { kind: 'dsh-lsp' } & ContextFormed
  }
}

// ---------------------------------------------------------------------------
// Harness contracts: official types plus the dsh-lsp subprocess seam
// ---------------------------------------------------------------------------

export interface SubprocessOutcome {
  exitCode: number | null
  signal: string | null
}

export interface SubprocessHandle {
  readonly pid: number
  readonly stdin: import('node:stream').Writable | undefined
  readonly stdout: import('node:stream').Readable | undefined
  readonly stderr: import('node:stream').Readable | undefined
  readonly done: Promise<SubprocessOutcome>
  /** Collected output (present when a stream used a collect disposition). */
  readonly collected?: {
    readonly stdout?: OutputCollectorLike
    readonly stderr?: OutputCollectorLike
  }
  terminate(): void
  waitForExit(signal?: AbortSignal): Promise<boolean>
}

/** Tail-keeping collector exposed by the subprocess seam's collect disposition. */
export interface OutputCollectorLike {
  /** Final collected text plus truncation flag and optional spill path. */
  finalize(): { text: string; truncated: boolean; spillPath?: string }
}

export interface SubprocessSpawnSpec {
  argv: readonly string[]
  cwd: string
  stdio: {
    stdin: 'ignore' | 'pipe' | { readonly data: string }
    stdout: 'pipe' | 'inherit' | { maxBytes: number }
    stderr: 'pipe' | 'inherit' | { maxBytes: number }
  }
  graceMs: number
  signal?: AbortSignal
  env?: Record<string, string | undefined>
}

export interface SubprocessRuntime {
  spawn(spec: SubprocessSpawnSpec): SubprocessHandle
}

/** Logger facade alias kept for registry/client consumers. */
export type LoggerLike = Logger

/**
 * dsh-lsp's harness context: the official Cordis `Context` (which carries
 * `tools`, `commands`, `effect`, `logger`, `on` through the official
 * augmentations) plus the custom `subprocess` seam, which the official
 * `Context` does not provide.
 */
export type DshContext = Context & { subprocess: SubprocessRuntime }

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

export interface ServerSpec {
  id: string
  command: string
  args: string[]
  extensions: string[]
  languageId: string
  rootMarkers: string[]
  enabled?: boolean
}

/** LSP diagnostic severity levels, in ascending severity order. */
export type DiagnosticSeverity = 'hint' | 'information' | 'warning' | 'error'

/** When language servers are started without waiting for a query. */
export type AutoStartMode = 'mount' | 'session' | 'off'

/** How one file's diagnostics are obtained: pushed by the server or pulled from it. */
export type DiagnosticsMode = 'auto' | 'push' | 'pull' | 'off'

export interface LspConfig {
  /**
   * Auto-start: spawn servers as soon as a session's cwd (or the process cwd)
   * roots them, so the first read/query never pays the spawn+initialize cost.
   * Auto-starting does *not* load a server's index: clangd only activates its
   * project index on the first didOpen, which the read hook already performs.
   */
  autoStart: AutoStartMode
  /** Server ids eligible for auto-start; empty means every enabled server. */
  autoStartServers: string[]
  /** Extra directories tried as auto-start roots; empty means the session cwd. */
  autoStartRoots: string[]
  maxLocations: number
  maxResultChars: number
  /**
   * Hard deadline for one LSP request and for the initialize handshake (ms).
   * Expiry cancels that request (or the start) and never aborts the server.
   */
  timeoutMs: number
  /** Directory where per-server stderr logs are written (absolute or ~-relative). */
  logDir: string
  servers: ServerSpec[]
  /** Sync-load the file into its LSP server when the AI reads it. */
  syncLoadOnRead: boolean
  /**
   * Continuously inject diagnostics changes. Every push a server makes is
   * observed and diffed against that file's last report; only added / removed
   * findings are injected, into every agent session rooted at that server.
   */
  diagnosticsInject: boolean
  /** Only diagnostics at or above this severity are injected. */
  diagnosticsMinSeverity: DiagnosticSeverity
  /**
   * How one file's diagnostics are obtained: `auto` keeps the pull request only
   * when the server advertises it (clangd does not, it pushes), `push`/`pull`
   * pin one path, and `off` disables the post-write refresh entirely.
   */
  diagnosticsMode: DiagnosticsMode
  /**
   * Upper bound on waiting for one push during an on-demand `lsp diagnostics`
   * query. The timeout only ends the wait: the server and its open documents
   * are never aborted.
   */
  diagnosticsTimeoutMs: number
  /** Upper bound on diagnostics listed in one injected message. */
  maxDiagnostics: number
  /**
   * Coalescing window: pushes that arrive inside one window are folded into a
   * single injected message (0 injects each flush immediately).
   */
  diagnosticsFlushMs: number
  /**
   * When `workspaceSymbol` finds nothing in the server's own index, converge
   * on candidate files, warm them, and retry the index query (slower, capped).
   */
  symbolFallback: boolean
  /** Upper bound on candidate files warmed during a symbol fallback. */
  symbolFallbackMaxCandidates: number
  /** Per-search-command timeout for the symbol fallback. */
  symbolFallbackTimeoutMs: number
  /** External file-search command used by the fallback (ripgrep-compatible). */
  fallbackSearchCommand: string
}

// ---------------------------------------------------------------------------
// Query vocabulary
// ---------------------------------------------------------------------------

export type QueryOperation =
  | 'explore'
  | 'goToDefinition'
  | 'findReferences'
  | 'goToImplementation'
  | 'typeDefinition'
  | 'hover'
  | 'workspaceSymbol'
  | 'documentSymbol'
  | 'diagnostics'

export interface QueryArgs {
  operation: QueryOperation
  filePath: string
  line?: number
  character?: number
  query?: string
  /** Minimum severity for `diagnostics`; `all` (the default) keeps everything. */
  severity?: DiagnosticSeverity | 'all'
}

/** 1-based UTF-16 cursor position (the tool's convention). */
export interface CursorPosition {
  line: number
  character: number
}

/** 1-based range for model-facing rendering. */
export interface LspRange {
  startLine: number
  startCharacter: number
  endLine: number
  endCharacter: number
}

export interface LspLocation {
  uri: string
  /** Workspace-relative path when inside the root, otherwise the absolute path. */
  path: string
  range: LspRange
  /** Trimmed content of the line the range starts on (best-effort, may be ''). */
  preview?: string
}

export interface SymbolEntry {
  name: string
  kind: string
  location: LspLocation
  containerName?: string
}

/** Which discovery pass produced the candidate files. */
export type CandidateStrategy = 'name' | 'content' | 'none'

export interface CandidateSearchOptions {
  /** Project root the search runs from (LSP root). */
  root: string
  /** Raw symbol query as typed by the model. */
  query: string
  /** File extensions the target server claims. */
  extensions: readonly string[]
  maxCandidates: number
  timeoutMs: number
  /** Search command override; defaults to `rg`. */
  command?: string
  signal?: AbortSignal
}

export interface CandidateSearchResult {
  candidates: string[]
  strategy: CandidateStrategy
  /** Candidates that matched before the cap was applied. */
  matched: number
  truncated: boolean
}

/** Provenance of a `workspaceSymbol` result that needed candidate convergence. */
export interface SymbolFallbackInfo {
  used: boolean
  strategy: CandidateStrategy
  candidates: number
  warmed: number
  matched: number
  truncated: boolean
}

/**
 * The last state of one `$/progress` stream reported by a server.
 *
 * clangd's background index reports itself this way (`backgroundIndexProgress`:
 * begin → report → end). Keeping the last statement per token lets a query
 * explain an empty answer while the index is still building, and lets the
 * plugin announce the moment it becomes usable.
 */
export interface IndexProgress {
  /** Progress token as chosen by the server (clangd: `backgroundIndexProgress`). */
  token: string
  /** `begin.title` (clangd: "indexing"). */
  title: string
  /** Last reported completeness, 0–100, or null before the first report. */
  percentage: number | null
  /** Translation units finished, parsed from the report's "completed/enqueued" message. */
  completed: number | null
  /** Translation units enqueued, parsed from the report's "completed/enqueued" message. */
  enqueued: number | null
  /** True once the server sent `end`: the queue drained and the index is usable. */
  done: boolean
  /** Epoch milliseconds of the last update. */
  updatedAt: number
}

export interface DiagnosticEntry {
  severity: DiagnosticSeverity
  message: string
  range: LspRange
  code?: string
  source?: string
}

/**
 * One diagnostics answer: the entries plus an optional note explaining why the
 * answer may be incomplete (a pull timed out, no push arrived in time, ...).
 */
export interface DiagnosticsReport {
  entries: DiagnosticEntry[]
  note?: string
}

export type LspQueryResult =
  | { kind: 'locations'; locations: LspLocation[]; truncated: boolean }
  | { kind: 'hover'; hover: string | null }
  | {
      kind: 'symbols'
      symbols: SymbolEntry[]
      truncated: boolean
      /** Present when the server's own index answered nothing and the candidate fallback ran. */
      fallback?: SymbolFallbackInfo
      /** Present when the answer was empty while a background index was still building. */
      indexing?: string
    }
  /** Present when the answer is incomplete (a timed-out pull, a missing push). */
  | { kind: 'diagnostics'; diagnostics: DiagnosticEntry[]; note?: string }
  | { kind: 'explore'; explore: ExploreResult }

export interface ExploreResult {
  symbolName: string
  definition: LspLocation[]
  typeDefinition: LspLocation[]
  hover: string | null
  implementation: LspLocation[]
  references: LspLocation[]
  truncated: boolean
}

/** A server whose runtime state the /lsp command reports. */
export interface ServerStatus {
  id: string
  languageId: string
  extensions: string[]
  state: 'running' | 'stopped' | 'starting' | 'failed'
  root: string | null
  pid: number | null
  error: string | null
  /** Last background-index progress the server reported, if any. */
  index?: IndexProgress | null
}
