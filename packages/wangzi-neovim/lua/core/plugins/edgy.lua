local function isFloatWindow(win)
    return vim.api.nvim_win_get_config(win).relative == "win"
end

return {
    "folke/edgy.nvim",
    name = "edgy",
    dir = gen.edgy,
    event = "VeryLazy",
    opts = {
        options = {
            left = { size = 40 },
            bottom = { size = 10 },
            right = { size = 30 },
            top = { size = 10 },
        },
        left = {
            {
                title = "Neo-Tree",
                ft = "neo-tree",
                size = { height = 0.3 },
                collapsed = true,
                filter = function(buf, win)
                    return vim.b[buf].neo_tree_source == "filesystem" and not isFloatWindow(win)
                end,
            },
            {
                title = "Neo-Tree Git",
                ft = "neo-tree",
                filter = function(buf, win)
                    return vim.b[buf].neo_tree_source == "git_status" and not isFloatWindow(win)
                end,
                open = "Neotree position=left git_status",
            },
            {
                title = "Neo-Tree Buffers",
                ft = "neo-tree",
                filter = function(buf, win)
                    return vim.b[buf].neo_tree_source == "buffers" and not isFloatWindow(win)
                end,
                open = "Neotree position=left buffers",
            },
            {
                ft = "trouble",
                title = "Trouble LSP",
                pinned = true,
                filter = function(buf, win)
                    local trouble = vim.w[win].trouble
                    return trouble and trouble.mode == "lsp"
                end,
                open = "Trouble lsp win.position=left",
            },
            {
                ft = "trouble",
                title = "Trouble Outline",
                pinned = true,
                filter = function(buf, win)
                    local trouble = vim.w[win].trouble
                    return trouble and trouble.mode == "lsp_document_symbols"
                end,
                open = "Trouble lsp_document_symbols win.position=left",
            },
        },
        right = {
            { ft = "codecompanion", title = "Code Companion Chat", size = { width = 0.25 } },
            { ft = "sagaoutline",   title = "Outline",             size = {} },
            {
                ft = "markdown",
                filter = function(buf)
                    return vim.api.nvim_buf_get_name(buf):sub(- #"gen.nvim") == "gen.nvim"
                end,
                title = "AI",
                size = { width = 0.25 },
            },
        },
        bottom = {
            {
                ft = "Trouble",
                title = "Trouble",
                filter = function(buf, win)
                    local trouble = vim.w[win].trouble
                    return trouble and trouble.position == "bottom"
                end,
            },
        },
        keys = {
          -- increase width
          ["<c-Right>"] = function(win)
            win:resize("width", 2)
          end,
          -- decrease width
          ["<c-Left>"] = function(win)
            win:resize("width", -2)
          end,
          -- increase height
          ["<c-Up>"] = function(win)
            win:resize("height", 2)
          end,
          -- decrease height
          ["<c-Down>"] = function(win)
            win:resize("height", -2)
          end,
        },
    },
    keys = {
        {
            "<leader>ww",
            function()
                require("edgy").toggle("left")
            end,
            mode = "n",
            desc = "next tab",
        },
    },
}
