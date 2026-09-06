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
export {};
