local function config()
    local null_ls = require("null-ls")
    local b = null_ls.builtins
    local sources = {
        b.code_actions.refactoring,
        b.diagnostics.semgrep,
        -- Rust
        -- b.formatting.rustfmt,
        -- C++
        b.diagnostics.cmake_lint,
        null_ls.builtins.diagnostics.clazy,
        b.diagnostics.cppcheck.with({
            args = {
                "--enable=warning,style,performance,portability",
                "--template=gcc",
                "--project=compile-commands.json",
                "$FILENAME",
            },
        }),
        -- b.formatting.clang_format,
        -- Python
        b.formatting.yapf,
        b.diagnostics.pylint,
        -- java
        b.formatting.google_java_format,
        -- mark
        b.formatting.prettier.with({ filetypes = { "html", "markdown", "css" } }),
        -- go
        b.formatting.gofmt,
        -- js/ts
        -- b.formatting.deno_fmt,
        -- Lua
        -- b.formatting.stylua.with({ extra_args = { "--indent-type", "Spaces", "--indent-width", "4" } }),
        -- Shell
        b.formatting.shfmt,
        -- git
        b.code_actions.gitsigns.with({
            config = {
                filter_actions = function(title)
                    return title:lower():match("blame") == nil
                end,
            },
        }),
        -- nix
        b.formatting.nixfmt,

        -- other
        b.diagnostics.codespell.with({ extra_args = { "--ignore-words=crate" } }),
        -- null_ls.builtins.code_actions.cspell,
        b.code_actions.gitrebase,
        -- null_ls.builtins.code_actions.gitsigns,
        -- null_ls.builtins.code_actions.refactoring,
        b.code_actions.statix,
        b.diagnostics.checkmake,
        b.diagnostics.cmake_lint,
        -- null_ls.builtins.diagnostics.cpplint,
        -- mark
        b.diagnostics.stylelint,
        b.formatting.xmllint,
        b.formatting.yamlfix,
        b.formatting.yamlfmt,
        b.formatting.rustywind,
    }
    vim.notify("none_ls init")
    null_ls.setup({
        sources = sources,
        on_attach = function(client)
            if client.server_capabilities.document_formatting then
                vim.cmd("autocmd BufWritePre <buffer> silent! lua vim.lsp.buf.format()")
            end
        end,
    })
end
return {
    "nvimtools/none-ls.nvim",
    dir = gen.none_ls,
    name = "none_ls",
    lazy = true,
    event = "LspAttach",
    config = config,
}
