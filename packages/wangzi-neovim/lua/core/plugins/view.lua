return {
    {
        "mbbill/undotree",
        dir = gen.undotree,
        name = "undotree",
        cmd = "UndotreeToggle",
        lazy = true,
        keys = {
            { "<leader>u", "<cmd>UndotreeToggle<CR>", mode = "n", desc = "Undo Tree" },
        },
    },
    {
        "sindrets/diffview.nvim",
        dir = gen.diffview,
        name = "diffview",
        dependencies = "plenary_nvim",
        module = "diffview",
        lazy = true,
        cmd = {
            "DiffviewOpen",
            "DiffviewClose",
            "DiffviewClose",
            "DiffviewFocusFiles",
            "DiffviewRefresh",
            "DiffviewFileHistory",
        },
        config = function()
            require("diffview").setup({})
        end,
        keys = {
            {
                "<leader>gd",
                function()
                    require("diffview").open()
                end,
                desc = "Open Diff",
            },
            {
                "<leader>gD",
                function()
                    require("diffview").close()
                end,
                desc = "Close Diff",
            },
            {
                "<leader>gh",
                function()
                    require("diffview").file_history()
                end,
                desc = "Git Log",
            },
            {
                "<leader>gH",
                function()
                    vim.cmd.DiffviewFileHistory("%")
                end,
                desc = "Git Log This File",
            },
            { "<leader>gf", "<cmd>DiffviewFileHistory %<CR>", desc = "File History" },
        },
    },
    gen.markdown_preview and {
        "davidgranstrom/nvim-markdown-preview",
        dir = gen.markdown_preview,
        name = "markdown_preview",
        ft = "markdown",
        cmd = "MarkdownPreview",
        lazy = true,
        disable = not gen.markdown_preview,
    } or {},
}
