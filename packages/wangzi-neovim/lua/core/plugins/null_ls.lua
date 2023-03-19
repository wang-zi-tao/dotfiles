local null_ls = require("null-ls")
local b = null_ls.builtins
local sources = {
    -- Rust
    b.formatting.rustfmt,
    -- C++
    -- b.formatting.clang_format,
    -- Python
    b.formatting.autopep8,
    -- java
    b.formatting.google_java_format,
    -- mark
    b.formatting.prettier.with({ filetypes = { "html", "markdown", "css" } }),
    -- js/ts
    b.formatting.deno_fmt,
    -- Lua
    -- b.formatting.stylua.with({ extra_args = { "--indent-type", "Spaces", "--indent-width", "2" } }),
    b.diagnostics.luacheck.with({ extra_args = { "--global vim" } }),
    -- Shell
    b.formatting.shfmt,
    b.diagnostics.shellcheck.with({ diagnostics_format = "#{m} [#{c}]" }),
    -- git
    b.code_actions.gitsigns,

    -- other
    null_ls.builtins.code_actions.cspell,
    null_ls.builtins.code_actions.gitrebase,
    -- null_ls.builtins.code_actions.gitsigns,
    -- null_ls.builtins.code_actions.refactoring,
    null_ls.builtins.code_actions.statix,
    null_ls.builtins.diagnostics.checkmake,
    null_ls.builtins.diagnostics.cmake_lint,
    null_ls.builtins.diagnostics.cpplint,
    null_ls.builtins.formatting.json_tool,
    

}
null_ls.setup({
    sources = sources,
    on_attach = function(client)
        if client.server_capabilities.document_formatting then
            vim.cmd("autocmd BufWritePre <buffer> silent! lua vim.lsp.buf.format()")
        end
    end,
})
