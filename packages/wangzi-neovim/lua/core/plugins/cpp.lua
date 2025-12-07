return {
    require("core.plugins.cmake"),
    {
        "p00f/clangd_extensions.nvim",
        dir = gen.clangd_extensions_nvim,
        name = "clangd_extensions_nvim",
        ft = { "h", "cpp", "cc", "c" },
        -- dependencies = "nvim_lspconfig",
        module = "clangd_extensions",
        lazy = true,
        config = function() end,
        keys = {
            { "<leader>ls", "<cmd>ClangdSwitchSourceHeader<CR>", desc = "Switch source/header" },
            { "<leader>lt", "<cmd>ClangdTypeHierarchy<CR>",      desc = "Type Hierarchy" },
            { "<leader>lA", "<cmd>ClangdAst<CR>",                desc = "AST" },
        }
    },
    {
        dir = gen.xmake,
        "Mythos-404/xmake.nvim",
        lazy = true,
        event = "BufReadPost xmake.lua",
        opts = {
            on_save = {
                reload_project_info = false,
            }
        },
        dependencies = { "nui_nvim", "plenary_nvim" },
        keys = {
        },
    }
}
