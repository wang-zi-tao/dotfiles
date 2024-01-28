local function config()
    require("gitsigns").setup({
        signs = {
            add = { hl = "GitSignsAdd", text = "│", numhl = "GitSignsAddNr", linehl = "GitSignsAddLn" },
            change = { hl = "GitSignsChange", text = "│", numhl = "GitSignsChangeNr", linehl = "GitSignsChangeLn" },
            delete = { hl = "GitSignsDelete", text = "_", numhl = "GitSignsDeleteNr", linehl = "GitSignsDeleteLn" },
            topdelete = { hl = "GitSignsDelete", text = "‾", numhl = "GitSignsDeleteNr", linehl = "GitSignsDeleteLn" },
            changedelete = { hl = "GitSignsChange", text = "~", numhl = "GitSignsChangeNr", linehl = "GitSignsChangeLn" },
        },
        signcolumn = true, -- Toggle with `:Gitsigns toggle_signs`
        numhl = false, -- Toggle with `:Gitsigns toggle_numhl`
        linehl = false, -- Toggle with `:Gitsigns toggle_linehl`
        word_diff = true, -- Toggle with `:Gitsigns toggle_word_diff`
        watch_gitdir = {
            interval = 1000,
            follow_files = true,
        },
        attach_to_untracked = true,
        current_line_blame = true, -- Toggle with `:Gitsigns toggle_current_line_blame`
        current_line_blame_opts = {
            virt_text = true,
            virt_text_pos = "eol", -- 'eol' | 'overlay' | 'right_align'
            delay = 1000,
            ignore_whitespace = false,
        },
        sign_priority = 6,
        update_debounce = 100,
        status_formatter = nil, -- Use default
        max_file_length = 40000,
        preview_config = {
            -- Options passed to nvim_open_win
            border = "rounded",
            style = "minimal",
            relative = "cursor",
            row = 0,
            col = 1,
        },
        yadm = {
            enable = false,
        },
    })

    require("scrollbar.handlers.gitsigns").setup()
end

return {
    "lewis6991/gitsigns.nvim",
    dir = gen.gitsigns_nvim,
    lazy = true,
    name = "gitsigns.nvim",
    module = "gitsigns",
    config = config,
    event = "VeryLazy",
    init = function()
        require("which-key").register({
            h = { name = "Hunk" },
            b = { name = "Buffer" },
            s = { name = "Select / Swap" },
            g = { name = "Git" },
        }, { prefix = "<leader>" })
    end,
    keys = {
        { "<leader>hs", "<cmd>Gitsigns stage_hunk<CR>",                mode = { "n", "v" }, desc = "Stage" },
        { "<leader>hr", "<cmd>Gitsigns reset_hunk<CR>",                mode = { "n", "v" }, desc = "Reset" },
        { "<leader>hp", "<cmd>Gitsigns preview_hunk<CR>",              mode = "n",          desc = "Preview" },
        { "<leader>hu", "<cmd>Gitsigns undo_stage_hunk<CR>",           mode = "n",          desc = "Undo" },
        { "<leader>bs", "<cmd>Gitsigns stage_buffer<CR>",              mode = "n",          desc = "Stage" },
        { "<leader>br", "<cmd>Gitsigns reset_buffer<CR>",              mode = "n",          desc = "Reset" },
        { "<leader>hb", "<cmd>Gitsigns reset_buffer<CR>",              mode = "n",          desc = "Reset" },
        { "<leader>hB", "<cmd>Gitsigns toggle_current_line_blame<CR>", mode = "n",          desc = "Blame" },
        { "<leader>hd", "<cmd>Gitsigns diffthis<CR>",                  mode = "n",          desc = "Diff This" },
        { "<leader>hD", "<cmd>Gitsigns toggle_deleted<CR>",            mode = "n",          desc = "Delete" },
        { "<leader>sh", "<cmd><C-U>Gitsigns select_hunk<CR>",          mode = "o",          desc = "Select Hunk" },
        { "<leader>sh", "<cmd><C-U>Gitsigns select_hunk<CR>",          mode = "x",          desc = "Select Hunk" },
        { "<leader>gb", "<cmd>Gitsigns blame_line<CR>",                mode = "n",          desc = "Blame line" },
        { "[h",         "<cmd>Gitsigns prev_hunk<CR>",                 mode = "n",          desc = "Prev Hunk" },
        { "]h",         "<cmd>Gitsigns next_hunk<CR>",                 mode = "n",          desc = "Next Hunk" },
    },
}
