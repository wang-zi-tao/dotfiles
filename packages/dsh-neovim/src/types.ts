/**
 * Internal vocabulary for dsh-neovim.
 *
 * The plugin is self-contained: it defines structural types for the small
 * surface of the harness it consumes (`ctx.tools`, `ctx.commands`, `ctx.on`,
 * `ctx.agents`, `ctx.effect`) instead of importing the harness packages, so it
 * builds with only `neovim` as a runtime dependency (the same dependency-light
 * pattern as `dsh-hindsight` / `dsh-lsp`).
 */

// ---------------------------------------------------------------------------
// Harness structural contracts (what apply() actually consumes)
// ---------------------------------------------------------------------------

export interface SessionLike {
  id: string
  header?: { cwd?: string; origin?: string }
}

/**
 * The slice of a harness Agent that dsh-neovim touches: injected context
 * delivery (DAP events) and session identity (the dapSessions registry).
 */
export interface AgentLike {
  session?: SessionLike
  /** Queue model-facing context for the next pre-step without waking the driver. */
  inject(message: InjectedUserMessage): void
}

export interface InjectedUserMessage {
  content: string
  source: { kind: 'plugin'; plugin: string }
}

export interface ToolRunContext {
  signal?: AbortSignal
  agent?: AgentLike
}

export interface ToolDefinition {
  name: string
  description: string
  parameters: Record<string, unknown>
  output: {
    schema: Record<string, unknown>
    render(args: unknown, value: any): Array<{ type: 'text'; text: string }>
  }
  execute(args: unknown, exec: ToolRunContext): Promise<unknown>
}

export interface CommandInvocation {
  agent: unknown
  rawInput: string
  signal: AbortSignal
}

export type CommandResult =
  | { kind: 'success'; text: string }
  | { kind: 'error'; text: string }

export interface CommandDefinition {
  name: string
  description: string
  input?: { hint: string; images?: boolean }
  handler(invocation: CommandInvocation): CommandResult | Promise<CommandResult>
}

/** The frozen `tools/result` execution view (observe-only). */
export interface ToolResultExecution {
  readonly name?: string
  readonly arguments?: unknown
}

export interface DshContext {
  tools: { register(definition: ToolDefinition): () => void }
  commands: { register(definition: CommandDefinition): () => void }
  agents: {
    list(): AgentLike[]
  }
  on(name: string, listener: (...args: any[]) => unknown): () => void
  effect<T>(callback: () => void | (() => void), label?: string): () => void
  logger?: ((name: string) => LoggerLike) | LoggerLike
}

export interface LoggerLike {
  debug(message: string): void
  info(message: string): void
  warn(message: string): void
  error(message: string): void
}

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

export interface NeovimConfig {
  /**
   * Explicit Neovim RPC address. Empty string = environment resolution:
   * $NVIM_LISTEN_ADDRESS → $NVIM → `\\.\pipe\nvim` (win32) →
   * `$XDG_RUNTIME_DIR|/tmp/nvim.$USER`.
   */
  socket: string
  /**
   * Lua module hosting the debugger bridge functions
   * (`dap_subscribe`, `dap_*`, `reload_file`, `run_async`).
   */
  luaModule: string
}
