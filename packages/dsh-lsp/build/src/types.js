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
export {};
