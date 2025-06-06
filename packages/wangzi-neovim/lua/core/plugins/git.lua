local function gitsigns_config()
    require("gitsigns").setup({
        signcolumn = true, -- Toggle with `:Gitsigns toggle_signs`
        numhl = false,     -- Toggle with `:Gitsigns toggle_numhl`
        linehl = false,    -- Toggle with `:Gitsigns toggle_linehl`
        word_diff = true,  -- Toggle with `:Gitsigns toggle_word_diff`
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
    })

    require("scrollbar.handlers.gitsigns").setup()
end

return {
    {
        "lewis6991/gitsigns.nvim",
        dir = gen.gitsigns_nvim,
        lazy = true,
        name = "gitsigns.nvim",
        module = "gitsigns",
        config = gitsigns_config,
        event = "LazyFile",
        init = function()
            require("which-key").add({
                { "<leader>h", group = "Hunk" },
                { "<leader>b", group = "Buffer" },
                { "<leader>s", group = "Select / Swap" },
                { "<leader>g", group = "Git" },
            })
        end,
        keys = {
            { "<leader>hs", "<cmd>Gitsigns stage_hunk<CR>",      mode = { "n", "v" }, desc = "Stage" },
            { "<leader>hr", "<cmd>Gitsigns reset_hunk<CR>",      mode = { "n", "v" }, desc = "Reset" },
            { "<leader>hp", "<cmd>Gitsigns preview_hunk<CR>",    mode = "n",          desc = "Preview" },
            { "<leader>hu", "<cmd>Gitsigns undo_stage_hunk<CR>", mode = "n",          desc = "Undo" },
            { "<leader>bs", "<cmd>Gitsigns stage_buffer<CR>",    mode = "n",          desc = "Stage" },
            { "<leader>br", "<cmd>Gitsigns reset_buffer<CR>",    mode = "n",          desc = "Reset" },
            {
                "<leader>hb",
                function()
                    require("gitsigns").blame_line({ full = true })
                end,
                mode = "n",
                desc = "Blame line"
            },
            { "<leader>hB", "<cmd>Gitsigns toggle_current_line_blame<CR>", mode = "n", desc = "Blame" },
            { "<leader>hd", "<cmd>Gitsigns diffthis<CR>",                  mode = "n", desc = "Diff This" },
            { "<leader>hD", "<cmd>Gitsigns toggle_deleted<CR>",            mode = "n", desc = "Delete" },
            { "<leader>sh", "<cmd><C-U>Gitsigns select_hunk<CR>",          mode = "o", desc = "Select Hunk" },
            { "<leader>sh", "<cmd><C-U>Gitsigns select_hunk<CR>",          mode = "x", desc = "Select Hunk" },
            { "<leader>gB", "<cmd>Gitsigns blame<CR>",                     mode = "n", desc = "Blame" },
            { "<leader>gb", "<cmd>Gitsigns blame_line<CR>",                mode = "n", desc = "Blame line" },
            {
                "[h",
                function()
                    require("gitsigns").nav_hunk("prev", { target = 'all', preview = true })
                end,
                mode = "n",
                desc = "Prev Hunk"
            },
            {
                "]h",
                function()
                    require("gitsigns").nav_hunk("next", { target = 'all', preview = true })
                end,
                mode = "n",
                desc = "Next Hunk"
            },
            { "ih", ":<C-U>Gitsigns select_hunk<CR>", mode = { "o", "x" }, desc = "select hunk" },

        },
    },
    {
        "NeogitOrg/neogit",
        dir = gen.neogit,
        lazy = true,
        name = "neogit",
        cmd = "Neogit",
        event = "VeryLazy",
        keys = {
            { "<leader>gg", "<cmd>Neogit cwd=%:p:h<CR>",                        "Neogit" },
            { "<leader>G",  "<cmd>Neogit cwd=%:p:h<CR>",                        "Neogit" },
            { "<leader>gl", function() require "neogit".open({ "log" }) end,    desc = "Git Log" },
            { "<leader>gP", function() require "neogit".open({ "pull" }) end,   desc = "Git Pull" },
            { "<leader>gc", function() require "neogit".open({ "commit" }) end, desc = "Git Commit" },
            { "<leader>gp", function() require "neogit".open({ "push" }) end,   desc = "Git Push" },
        },
        -- dependencies = {
        --     "plenary_nvim",
        --     "diffview",
        --     "telescope_nvim",
        -- },
        config = function()
            local neogit = require('neogit')
            local symbols = require("core.theme").symbols
            neogit.setup {
                graph_style = "unicode",
                signs = {
                    -- { CLOSED, OPENED }
                    hunk = { symbols.checkbox_false, symbols.checkbox_true },
                    item = { symbols.checkbox_false, symbols.checkbox_true },
                    section = { symbols.checkbox_false, symbols.checkbox_true },
                },
                graph_style = "unicode",
            }
        end
    }
}
