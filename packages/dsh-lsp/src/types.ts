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

export type {
  Logger,
  ToolDefinition,
  ToolRunContext,
  CommandDefinition,
  CommandInvocation,
  CommandResult,
  Session,
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

/** How post-write diagnostics are obtained from a server. */
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
  timeoutMs: number
  /** Directory where per-server stderr logs are written (absolute or ~-relative). */
  logDir: string
  servers: ServerSpec[]
  /** Sync-load the file into its LSP server when the AI reads it. */
  syncLoadOnRead: boolean
  /** Async diagnostics + agent.inject after the AI writes a file. */
  diagnosticsOnWrite: boolean
  /** Only diagnostics at or above this severity are injected. */
  diagnosticsMinSeverity: DiagnosticSeverity
  /**
   * How diagnostics are obtained: `auto` keeps the pull request only when the
   * server advertises it (clangd does not), `push`/`pull` pin one path, and
   * `off` disables post-write diagnostics entirely.
   */
  diagnosticsMode: DiagnosticsMode
  /**
   * Upper bound on waiting for one push. The timeout only ends the wait: the
   * server and its open documents are never aborted.
   */
  diagnosticsTimeoutMs: number
  /** Window in which an identical finding set is injected only once. */
  diagnosticsDedupeMs: number
  /** Upper bound on injected diagnostics per push. */
  maxDiagnostics: number
  /**
   * Inject a push that arrives with no armed write watch, into the agent that
   * most recently wrote that file. Off by default: a background re-publish
   * (another file's edit invalidating this translation unit) would otherwise
   * interrupt an unrelated turn.
   */
  diagnosticsOnAnyPublish: boolean
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
  /** When true, the result also carries a structured `json` projection. */
  json?: boolean
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

export interface DiagnosticEntry {
  severity: DiagnosticSeverity
  message: string
  range: LspRange
  code?: string
  source?: string
}

export type LspQueryResult =
  | { kind: 'locations'; locations: LspLocation[]; truncated: boolean }
  | { kind: 'hover'; hover: string | null }
  | { kind: 'symbols'; symbols: SymbolEntry[]; truncated: boolean; fallback?: SymbolFallbackInfo }
  | { kind: 'diagnostics'; diagnostics: DiagnosticEntry[] }
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
}
