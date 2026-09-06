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

export interface LspConfig {
  lazyStart: boolean
  maxLocations: number
  maxResultChars: number
  timeoutMs: number
  /** Directory where per-server stderr logs are written (absolute or ~-relative). */
  logDir: string
  servers: ServerSpec[]
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

export interface DiagnosticEntry {
  severity: string
  message: string
  range: LspRange
  code?: string
  source?: string
}

export type LspQueryResult =
  | { kind: 'locations'; locations: LspLocation[]; truncated: boolean }
  | { kind: 'hover'; hover: string | null }
  | { kind: 'symbols'; symbols: SymbolEntry[]; truncated: boolean }
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
