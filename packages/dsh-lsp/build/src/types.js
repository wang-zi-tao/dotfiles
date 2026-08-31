/**
 * Internal vocabulary for dsh-lsp.
 *
 * The plugin is self-contained: it defines structural types for the small
 * surface of the harness it consumes (`ctx.tools`, `ctx.commands`,
 * `ctx.subprocess`, `ctx.effect`) instead of importing the harness packages,
 * so it builds with only `vscode-languageserver-protocol` as a runtime
 * dependency (the same dependency-free pattern as `dsh-hindsight`).
 */
export {};
