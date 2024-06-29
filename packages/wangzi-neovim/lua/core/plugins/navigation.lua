return {
    {
        "phaazon/hop.nvim",
        dir = gen.hop_nvim,
        name = "hop_nvim",
        module = "hop",
        lazy = true,
        event = { "BufNewFile", "BufReadPost" },
        config = function()
            require("hop").setup({})
        end,
        init = function()
            require("which-key").register({
                T = { name = "Hop" },
            }, { prefix = "<leader>" })
        end,
        keys = {
            {
                "<leader>j",
                function()
                    require("hop").hint_char1()
                end,
                desc = "hop char1",
            },
            {
                "<leader>k",
                function()
                    require("hop").hint_char2()
                end,
                desc = "hop char1",
            },
            {
                "<leader>Ta",
                function()
                    require("hop").hint_anywhere()
                end,
                desc = "any",
            },
            {
                "<leader>Tw",
                function()
                    require("hop").hint_words()
                end,
                desc = "words",
            },
            {
                "<leader>Tc",
                function()
                    require("hop").hint_char1()
                end,
                desc = "char1",
            },
            {
                "<leader>Th",
                function()
                    require("hop").hint_char1()
                end,
                desc = "char1",
            },
            {
                "<leader>Te",
                function()
                    require("hop").hint_char1({ direction = require("hop.hint").HintDirection.AFTER_CURSOR })
                end,
                desc = "back",
            },
            {
                "<leader>Tb",
                function()
                    require("hop").hint_char1({ direction = require("hop.hint").HintDirection.BEFORE_CURSOR })
                end,
                desc = "forward",
            },
        },
    },
    {
        "numToStr/Navigator.nvim",
        dir = gen.navigator,
        name = "navigator",
        module = "Navigator",
        lazy = true,
        config = function()
            require("Navigator").setup({ autosave = "all" })
        end,
        keys = {
            {
                "<C-h>",
                function()
                    require("Navigator").left()
                end,
                mode = { "n", "t" },
                desc = "Navigator left",
            },
            {
                "<C-k>",
                function()
                    require("Navigator").up()
                end,
                mode = { "n", "t" },
                desc = "Navigator up",
            },
            {
                "<C-l>",
                function()
                    require("Navigator").right()
                end,
                mode = { "n", "t" },
                desc = "Navigator right",
            },
            {
                "<C-j>",
                function()
                    require("Navigator").down()
                end,
                mode = { "n", "t" },
                desc = "Navigator down",
            },
            {
                "<A-p>",
                function()
                    require("Navigator").previous()
                end,
                mode = "n",
                desc = "Navigator previous",
            },
        },
    },
    {
        "beauwilliams/focus.nvim",
        dir = gen.focus,
        name = "focus_nvim",
        enabled = false,
        lazy = true,
        cmd = {
            "FocusDisable",
            "FocusEnable",
            "FocusToggle",
            "FocusSplitNicely",
            "FocusSplitCycle",
            "FocusDisableWindow",
            "FocusEnableWindow",
            "FocusToggleWindow",
            "FocusGetDisabledWindows",
            "FocusSplitLeft",
            "FocusSplitDown",
            "FocusSplitUp",
            "FocusSplitRight",
            "FocusEqualise",
            "FocusMaximise",
            "FocusMaxOrEqual",
        },
        config = function()
            require("focus").setup({
                excluded_filetypes = { "toggleterm", "notify", "markdown" },
                -- hybridnumber = true,
                treewidth = 30,
                width = 96,
                height = 30,
            })
        end,
        event = "VeryLazy",
        keys = {
            { "<leader>wh", ":FocusSplitLeft<CR>",          silent = true, desc = "Split left" },
            { "<leader>wk", ":FocusSplitUp<CR>",            silent = true, desc = "Split up" },
            { "<leader>wl", ":FocusSplitRight<CR>",         silent = true, desc = "Split right" },
            { "<leader>wj", ":FocusSplitDown<CR>",          silent = true, desc = "Split down" },
            { "<leader>wt", ":FocusSplitDown cmd term<CR>", silent = true, desc = "Terminal" },
        },
    },
    {
        "max397574/better-escape.nvim",
        dir = gen.better_escape_nvim,
        name = "better_escape_nvim",
        disable = true,
        lazy = true,
        event = "VeryLazy",
        config = function()
            require("better_escape").setup({})
        end,
    },
    {
        "folke/flash.nvim",
        dir = gen.flash_nvim,
        name = "flash_nvim",
        event = "VeryLazy",
        ---@type Flash.Config
        opts = {},
        -- stylua: ignore
        keys = {
            { "s",     mode = { "n", "x", "o" }, function() require("flash").jump() end,              desc = "Flash" },
            { "S",     mode = { "n", "x", "o" }, function() require("flash").treesitter() end,        desc = "Flash Treesitter" },
            { "r",     mode = "o",               function() require("flash").remote() end,            desc = "Remote Flash" },
            { "R",     mode = { "o", "x" },      function() require("flash").treesitter_search() end, desc = "Treesitter Search" },
            { "<c-s>", mode = { "c" },           function() require("flash").toggle() end,            desc = "Toggle Flash Search" },
        },
    }
}
