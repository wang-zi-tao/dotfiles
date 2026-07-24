return {
    {
        "phaazon/hop.nvim",
        dir = gen.hop_nvim,
        name = "hop",
        module = "hop",
        lazy = true,
        event = { "BufNewFile", "BufReadPost" },
        opts = {},
        init = function()
            require("which-key").add({
                { "<leader>T", group = "Hop" },
            })
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
        enabled = false,
        lazy = true,
        opts = { autosave = "all" },
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
        "swaits/zellij-nav.nvim",
        name = "zellij-nav",
        dir = gen.zellij_nav,
        lazy = true,
        event = "VeryLazy",
        keys = {
            { "<C-h>", "<cmd>ZellijNavigateLeftTab<CR>",  { mode = { "n", "t" }, silent = true, desc = "navigate left or tab" } },
            { "<C-j>", "<cmd>ZellijNavigateDown<CR>",     { mode = { "n", "t" }, silent = true, desc = "navigate down" } },
            { "<C-k>", "<cmd>ZellijNavigateUp<CR>",       { mode = { "n", "t" }, silent = true, desc = "navigate up" } },
            { "<C-l>", "<cmd>ZellijNavigateRightTab<CR>", { mode = { "n", "t" }, silent = true, desc = "navigate right or tab" } },
        },
        opts = {},
        init = function()
            vim.api.nvim_create_autocmd("VimLeave", {
                pattern = "*",
                command = "silent !zellij action switch-mode normal"
            })
        end
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
        opts = {
            -- hybridnumber = true,
            treewidth = 30,
            autoresize = {
                minwidth = 20,
                minheight = 20,
                focusedwindow_minwidth = 48,
                focusedwindow_minheight = 48,
            }
        },
        config = function(opts)
            local ignore_filetypes = require("core.utils").file_type_blacklist
            local augroup = vim.api.nvim_create_augroup('FocusDisable', { clear = true })
            vim.api.nvim_create_autocmd('FileType', {
                group = augroup,
                callback = function(_)
                    if vim.tbl_contains(ignore_filetypes, vim.bo.filetype) then
                        vim.b.focus_disable = true
                    else
                        vim.b.focus_disable = false
                    end
                end,
                desc = 'Disable focus autoresize for FileType',
            })

            require("focus").setup(vim.tbl_deep_extend("force", opts, {
            }))
        end,
        event = "VeryLazy",
        keys = {
            { "<leader>wh", "<cmd>FocusSplitLeft<CR>",          silent = true, desc = "Split left" },
            { "<leader>wk", "<cmd>FocusSplitUp<CR>",            silent = true, desc = "Split up" },
            -- { "<leader>wl", "<cmd>FocusSplitRight<CR>",         silent = true, desc = "Split right" },
            { "<leader>wj", "<cmd>FocusSplitDown<CR>",          silent = true, desc = "Split down" },
            { "<leader>wt", "<cmd>FocusSplitDown cmd term<CR>", silent = true, desc = "Terminal" },
        },
    },
    {
        "max397574/better-escape.nvim",
        dir = gen.better_escape_nvim,
        name = "better_escape_nvim",
        disable = true,
        lazy = true,
        event = "VeryLazy",
        opts = {},
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
            -- { "S",     mode = { "n", "x", "o" }, function() require("flash").treesitter() end,        desc = "Flash Treesitter" },
            { "r",     mode = "o",               function() require("flash").remote() end,            desc = "Remote Flash" },
            { "R",     mode = { "o", "x" },      function() require("flash").treesitter_search() end, desc = "Treesitter Search" },
            { "<c-s>", mode = { "c" },           function() require("flash").toggle() end,            desc = "Toggle Flash Search" },
        },
    },
    {
        "ThePrimeagen/harpoon",
        branch = "harpoon2",
        dir = gen.harpoon2,
        name = "harpoon2",
        module = "harpoon",
        keys = {
            { "<leader>ha", function() require("harpoon"):list():add() end, desc = "Mark add" },
            {
                "<leader>hl",
                function()
                    local harpoon = require("harpoon")
                    harpoon.ui:toggle_quick_menu(harpoon:list())
                end,
                desc = "Mark List"
            },
            {
                "<leader>ht",
                function()
                    local harpoon = require("harpoon")
                    local conf = require("telescope.config").values
                    local function toggle_telescope(harpoon_files)
                        local file_paths = {}
                        for _, item in ipairs(harpoon_files.items) do
                            table.insert(file_paths, item.value)
                        end

                        require("telescope.pickers").new({}, {
                            prompt_title = "Harpoon",
                            finder = require("telescope.finders").new_table({
                                results = file_paths,
                            }),
                            previewer = conf.file_previewer({}),
                            sorter = conf.generic_sorter({}),
                        }):find()
                    end
                    toggle_telescope(harpoon:list())
                end,
                desc = "Mark Telescope"
            },
        },
        config = function()
            local harpoon = require("harpoon")
            harpoon:setup()
        end
    },
    {
        "otavioschwanck/arrow.nvim",
        dir = gen.arrow_nvim,
        name = "arrow_nvim",
        module = "arrow",
        event = "LazyFile",
        opts = {
            show_icons = true,
            leader_key = ';',
            buffer_leader_key = 'm',
            per_buffer_config = {
                lines = 6,                 -- Number of lines showed on preview.
                sort_automatically = true, -- Auto sort buffer marks.
                satellite = {              -- defualt to nil, display arrow index in scrollbar at every update
                    enable = false,
                    overlap = true,
                    priority = 1000,
                },
                zindex = 10,              --default 50
                treesitter_context = nil, -- it can be { line_shift_down = 2 }, currently not usable, for detail see https://github.com/otavioschwanck/arrow.nvim/pull/43#issue-2236320268
            },
            mappings = {
                edit = "e",
                delete_mode = "d",
                clear_all_items = "C",
                toggle = "t", -- used as save if separate_save_and_remove is true
                open_vertical = "v",
                open_horizontal = "s",
                quit = "q",
                remove = "x", -- only used if separate_save_and_remove is true
                next_item = "]",
                prev_item = "["
            },
            window = { -- controls the appearance and position of an arrow window (see nvim_open_win() for all options)
                width = "auto",
                height = "auto",
                row = "auto",
                col = "auto",
                border = "single",
            },
        },
        keys = {
            {
                "[A",
                function()
                    require("arrow.persist").previous()
                end,
                desc = "previous mark"
            },
            {
                "]A",
                function()
                    require("arrow.persist").next()
                end,
                desc = "next mark"
            },
            {
                "[a",
                "<cmd>Arrow prev_buffer_bookmark<CR>",
                desc = "previous mark"
            },
            {
                "]a",
                "<cmd>Arrow next_buffer_bookmark<CR>",
                desc = "next mark"
            },
        },
    }
}
