local null_ls = require("null-ls")
local b = null_ls.builtins
local sources = {
  b.code_actions.gitsigns,
  -- Rust
  -- b.formatting.rustfmt,
  -- C++
  -- b.formatting.clang_format,
  -- java
  b.formatting.google_java_format,
  -- mark
  b.formatting.prettier.with({ filetypes = { "html", "markdown", "css" } }),
  -- js/ts
  b.formatting.deno_fmt,
  -- Lua
  -- b.formatting.stylua.with({ extra_args = { "--indent-type", "Spaces", "--indent-width", "2" } }),
  -- b.diagnostics.luacheck.with({ extra_args = { "--global vim" } }),
  -- Shell
  b.formatting.shfmt,
  b.diagnostics.shellcheck.with({ diagnostics_format = "#{m} [#{c}]" }),
  -- git
  b.code_actions.gitsigns,
}
null_ls.setup({
  sources = sources,
  on_attach = function(client)
    if client.resolved_capabilities.document_formatting then
      vim.cmd("autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()")
    end
  end,
})
