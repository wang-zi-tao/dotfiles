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
        -- null_ls.builtins.diagnostics.cpplint,
        b.diagnostics.cppcheck.with({
            root_dir = require("null-ls.utils").root_pattern("compile_commands.json"),
            to_temp_file = false,
            args = function(params)
                local Path = require("plenary.path")
                local uri = params.lsp_params.textDocument.uri
                local p = "file:///" .. params.cwd .. "/"
                local relative_path = (uri:sub(0, #p) == p) and uri:sub(#p + 1) or uri
                vim.notify(relative_path)
                return {
                    "--platform=win32W",
                    "--std=c++17",
                    "-D_MSC_VER=1924",
                    "-D__RPCNDR_H_VERSION__=(500)",
                    "-D__i386__=1",
                    "-D__cplusplus",
                    "-IC:/Program Files (x86)/Microsoft Visual Studio/2019/Professional/VC/Tools/MSVC/14.29.30133/include",
                    "-I../3rdparty/qt5/build_x86/qtbase/include/",
                    "-I../3rdparty/qt5/build_x86/qtbase/include/QtCore",
                    "-I../3rdparty/qt5/build_x86/qtbase/include/QtGui",
                    "-I../3rdparty/qt5/build_x86/qtbase/include/QtWidgets",
                    "-I../3rdparty/default/x86-windows/include/",
                    "--inconclusive",
                    "--check-level=exhaustive",
                    "--performance-valueflow-max-if-count=16",
                    -- "--enable=warning,style,performance,portability",
                    "--enable=all",
                    "--template=gcc",
                    -- "--project=../debug/WPSOffice.sln",
                    "--project=./compile_commands.json",
                    "--cppcheck-build-dir=./.cppcheck",
                    "--file-filter=" .. relative_path,
                }
            end,
        }),
        b.formatting.clang_format,
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
        -- b.code_actions.gitsigns.with({
        --     config = {
        --         filter_actions = function(title)
        --             return title:lower():match("blame") == nil
        --         end,
        --     },
        -- }),
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
    null_ls.setup({
        sources = sources,
        debug = false,
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
