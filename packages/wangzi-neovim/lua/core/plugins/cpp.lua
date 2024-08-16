local function clangd_config(on_attach, capabilities)
    local num_of_job = require("core.utils").num_of_core()
    require("clangd_extensions").setup({
        server = {
            on_attach = on_attach,
            capabilities = capabilities,
            flags = {
                debounce_text_changes = 150,
            },
            -- cmd = { "clangd", "--background-index", "--pch-storage=disk", "-j=" .. tostring(num_of_job) }
            cmd = {
                "clangd",
                "--background-index",
                "--pch-storage=disk",
                "--log=error",
                num_of_job ~= 0 and "-j=" .. tostring(num_of_job) or nil,
            },
        },
        extensions = {
            -- defaults:
            -- Automatically set inlay hints (type hints)
            autoSetHints = true,
            -- These apply to the default ClangdSetInlayHints command
            inlay_hints = {
                -- Only show inlay hints for the current line
                only_current_line = false,
                -- Event which triggers a refersh of the inlay hints.
                -- You can make this "CursorMoved" or "CursorMoved,CursorMovedI" but
                -- not that this may cause  higher CPU usage.
                -- This option is only respected when only_current_line and
                -- autoSetHints both are true.
                only_current_line_autocmd = "CursorHold",
                -- whether to show parameter hints with the inlay hints or not
                show_parameter_hints = true,
                -- prefix for parameter hints
                parameter_hints_prefix = "<- ",
                -- prefix for all the other hints (type, chaining)
                other_hints_prefix = "=> ",
                -- whether to align to the length of the longest line in the file
                max_len_align = false,
                -- padding from the left if max_len_align is true
                max_len_align_padding = 1,
                -- whether to align to the extreme right or not
                right_align = false,
                -- padding from the right if right_align is true
                right_align_padding = 7,
                -- The color of the hints
                highlight = "Comment",
                -- The highlight group priority for extmark
                priority = 100,
            },
            ast = {
                role_icons = {
                    type = "",
                    declaration = "",
                    expression = "",
                    specifier = "",
                    statement = "",
                    ["template argument"] = "",
                },
                kind_icons = {
                    Compound = "",
                    Recovery = "",
                    TranslationUnit = "",
                    PackExpansion = "",
                    TemplateTypeParm = "",
                    TemplateTemplateParm = "",
                    TemplateParamObject = "",
                },

                highlights = {
                    detail = "Comment",
                },
            },
            memory_usage = {
                border = "rounded",
            },
            symbol_info = {
                border = "rounded",
            },
        },
    })
end

return {
    clangd_config = clangd_config,
    {
        "p00f/clangd_extensions.nvim",
        dir = gen.clangd_extensions_nvim,
        name = "clangd_extensions_nvim",
        ft = { "h", "cpp", "cc", "c" },
        dependencies = "nvim_lspconfig",
        module = "clangd_extensions",
        lazy = true,
        config = function() end,
    },
    {
        "Shatur/neovim-cmake",
        dir = gen.cmake,
        name = "cmake",
        cmd = { "CMake" },
        dependencies = "nvim_lspconfig",
        ft = { "cpp", "c", "hpp", "h", "CMakeLists.txt", "cmake" },
        lazy = true,
        config = function()
            require("core.plugins.cmake")
        end,
        keys = {
            {
                "<leader>cm",
                function()
                    vim.cmd(":CMake<CR>")
                end,
                desc = "CMake",
            },
            { "<leader>cc", ":CMake configure<CR>",         desc = "CMake configure" },
            { "<leader>cC", ":CMake clean<CR>",             desc = "CMake clean" },
            { "<leader>cr", ":CMake build_and_run<CR>",     desc = "CMake run" },
            { "<leader>cd", ":CMake build_and_debug<CR>",   desc = "CMake debug" },
            { "<leader>ct", ":CMake select_build_type<CR>", desc = "CMake build type" },
            { "<leader>cs", ":CMake select_target<CR>",     desc = "CMake select target" },
            { "<leader>cB", ":CMake build_all<CR>",         desc = "CMake build all" },
            { "<leader>cb", ":CMake build<CR>",             desc = "CMake build" },
        },
    },
    {
        dir = gen.xmake,
        "Mythos-404/xmake.nvim",
        lazy = true,
        event = "BufReadPost xmake.lua",
        config = true,
        dependencies = { "nui_nvim", "plenary_nvim" },
        keys = {
        },
    }
}
