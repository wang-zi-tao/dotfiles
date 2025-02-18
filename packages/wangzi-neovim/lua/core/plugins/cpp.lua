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
    })
end

return {
    clangd_config = clangd_config,
    require("core.plugins.cmake"),
    {
        "p00f/clangd_extensions.nvim",
        dir = gen.clangd_extensions_nvim,
        name = "clangd_extensions_nvim",
        ft = { "h", "cpp", "cc", "c" },
        dependencies = "nvim_lspconfig",
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
        config = true,
        dependencies = { "nui_nvim", "plenary_nvim" },
        keys = {
        },
    }
}
