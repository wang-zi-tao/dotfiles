/**
 * Internal vocabulary for dsh-neovim.
 *
 * The plugin is self-contained: it defines structural types for the small
 * surface of the harness it consumes (`ctx.tools`, `ctx.commands`, `ctx.on`,
 * `ctx.agents`, `ctx.effect`) instead of importing the harness packages, so it
 * builds with only `neovim` as a runtime dependency (the same dependency-light
 * pattern as `dsh-hindsight` / `dsh-lsp`).
 */
export {};
