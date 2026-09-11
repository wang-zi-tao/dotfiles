/**
 * Internal vocabulary for dsh-neovim.
 *
 * The plugin imports the real types from the official `@deepseek-ai/*` packages
 * instead of defining structural types for the harness surface it consumes
 * (`ctx.tools`, `ctx.commands`, `ctx.on`, `ctx.agents`, `ctx.effect`). The
 * plugin still builds with only `neovim` as a runtime dependency: every import
 * here is type-only and erased at compile time, with one runtime exception —
 * `createUserMessage` from `@deepseek-ai/dsh-llm`, used by `apply()` for DAP
 * event injection, which is why dsh-llm sits in `dependencies`.
 */

import type {Context, Logger} from '@deepseek-ai/cordis';
import type {ToolDefinition, ToolRunContext, ToolExecution} from '@deepseek-ai/dsh-tools';
import type {CommandDefinition, CommandInvocation, CommandResult} from '@deepseek-ai/dsh-commands';
import type {Agent} from '@deepseek-ai/dsh-agent';
import type {Session} from '@deepseek-ai/dsh-session';

export type {
  Context,
  Logger,
  ToolDefinition,
  ToolRunContext,
  ToolExecution,
  CommandDefinition,
  CommandInvocation,
  CommandResult,
  Agent,
  Session,
};

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

export interface NeovimConfig {
  /**
   * Explicit Neovim RPC address. Empty string = environment resolution:
   * $NVIM_LISTEN_ADDRESS → $NVIM → `\\.\pipe\nvim` (win32) →
   * `$XDG_RUNTIME_DIR|/tmp/nvim.$USER`.
   */
  socket: string;
  /**
   * Lua module hosting the debugger bridge functions
   * (`dap_subscribe`, `dap_*`, `reload_file`, `run_async`).
   */
  luaModule: string;
}
