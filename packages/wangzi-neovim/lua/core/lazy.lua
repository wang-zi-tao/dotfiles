local gen = gen

vim.g.mapleader = " "
require("lazy").setup({
    require("core.plugins.cmp"),
    require("core.plugins.telescope"),
    require("core.plugins.treesitter"),
    require("core.plugins.dap"),
    require("core.plugins.lspconfig"),
    require("core.plugins.ui"),

    require("core.plugins.nvimtree"),
    require("core.plugins.symbols_outline_pre"),
    require("core.plugins.gitsigns"),
    require("core.plugins.vgit"),

    {
        dir = ".",
        name = "core",
        module = "core",
        priority = 100,
        config = function()
            require("core")
        end,
    },
    {
        "nvim-lua/plenary.nvim",
        dir = gen.plenary_nvim,
        module = "plenary",
        dependencies = "core",
        lazy = true,
        name = "plenary_nvim",
    },
    {
        "andymass/vim-matchup",
        dir = gen.vim_matchup,
        name = "vim_matchup",
        lazy = true,
        event = "InsertEnter",
        dependencies = "core",
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
        "windwp/nvim-autopairs",
        dir = gen.nvim_autopairs,
        name = "nvim_autopairs",
        lazy = true,
        event = "InsertEnter",
        config = function()
            local autopairs = require("nvim-autopairs")
            autopairs.setup({ fast_wrap = {} })
        end,
    },


    {
        "terrortylor/nvim-comment",
        dir = gen.nvim_comment,
        name = "nvim_comment",
        dependencies = "core",
        module = "nvim_comment",
        keys = { "gcc", "gc" },
        lazy = true,
        config = function()
            require("core.plugins.others").comment()
        end,
    },

    {
        "kkharji/sqlite.lua",
        dir = gen.sqlite,
        name = "sqlite",
        module = "sqlite",
        lazy = true,
        init = function()
            if gen.libsqlite then
                vim.g.sqlite_clib_path = gen.libsqlite
            end
        end,
    },
    require("core.plugins.trouble"),
    {
        "davidgranstrom/nvim-markdown-preview",
        dir = gen.markdown_preview,
        name = "markdown_preview",
        ft = "markdown",
        cmd = "MarkdownPreview",
        lazy = true,
        disable = not gen.markdown_preview,
    },
    {
        "chentoast/marks.nvim",
        dir = gen.marks,
        name = "marks",
        dependencies = "core",
        lazy = true,
        config = function()
            require("core.plugins.others").marks()
        end,
        keys = { "m" },
    },
    {
        "Pocco81/AutoSave.nvim",
        dir = gen.auto_save,
        name = "auto_save",
        dependencies = "core",
        lazy = true,
        event = { "TextChanged", "InsertEnter" },
        config = function()
            require("core.plugins.others").auto_save()
        end,
    },
    {
        "mbbill/undotree",
        dir = gen.undotree,
        dependencies = "core",
        name = "undotree",
        cmd = "UndotreeToggle",
        lazy = true,
        keys = {
            { "<leader>u", "<cmd>UndotreeToggle<CR>", mode = "n", desc = "Undo Tree" },
        }
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
        },
        config = function()
            require("diffview").setup({})
        end,
        keys = {
            { "<leader>gd", function() require("diffview").open() end, desc = "Open Diff", },
            { "<leader>gD", function() require("diffview").close() end, desc = "Close Diff", },
            { "<leader>gh", function() require("diffview").file_history() end, desc = "Git Log", },
            { "<leader>gf", ":DiffviewFileHistory %<CR>", desc = "File History", },
        }
    },
    {
        'numToStr/Navigator.nvim',
        dir = gen.navigator,
        name = "navigator",
        dependencies = "core",
        module = "Navigator",
        lazy = true,
        config = function()
            require("Navigator").setup({ autosave = "all" })
        end,
        keys = {
            { "<C-h>", function() require('Navigator').left() end, mode = { "n", "t" }, desc = "Navigator left" },
            { "<C-k>", function() require('Navigator').up() end, mode = { "n", "t" }, desc = "Navigator up" },
            { "<C-l>", function() require('Navigator').right() end, mode = { "n", "t" }, desc = "Navigator right" },
            { "<C-j>", function() require('Navigator').down() end, mode = { "n", "t" }, desc = "Navigator down" },
            { "<A-p>", function() require('Navigator').previous() end, mode = "n", desc = "Navigator previous" },
        }
    },
    {
        "RRethy/vim-illuminate",
        dir = gen.illuminate,
        name = "illuminate",
        module = "illuminate",
        lazy = true,
        event = "VeryLazy",
    },
    {
        "anuvyklack/keymap-amend.nvim",
        dir = gen.keymap_amend,
        name = "keymap_amend",
        lazy = true,
        module = "keymap-amend",
    },

    {
        "numToStr/FTerm.nvim",
        dir = gen.fterm,
        name = "fterm",
        module = "FTerm",
        lazy = true,
        config = function()
            require("FTerm").setup({
                border = "rounded",
                dimensions = {
                    height = 0.9,
                    width = 0.9,
                },
            })
        end,
        keys = {
            { "\\'", function() require("FTerm").toggle() end, mode = "n", desc = "Float Terminal" },
            { "<C-\\>", function() require("FTerm").toggle() end, mode = { "n", "t" }, { "n", "t" },
                desc = "Float Terminal" }
        }
    },
    {
        "echasnovski/mini.nvim",
        dir = gen.mini,
        name = "mini",
        dependencies = "core",
        lazy = true,
        config = function()
            require("core.plugins.others").mini()
        end,
        event = "VeryLazy",
    },
    {
        "Shatur/neovim-session-manager",
        dir = gen.session_manager,
        name = "session_manager",
        lazy = true,
        config = function()
            require("core.plugins.others").session_manager()
        end,
    },
    {
        "glacambre/firenvim",
        dir = gen.firenvim,
        name = "firenvim",
        dependencies = "core",
        lazy = true,
        build = function()
            vim.fn["firenvim#install"](0)
        end,
    },
    {
        "t-troebst/perfanno.nvim",
        dir = gen.perfanno_nvim,
        name = "perfanno_nvim",
        cmd = { "PerfLoadFlat", "PerfLoadCallGraph", "PerfLoadFlameGraph", "PerfLuaProfileStart", "PerfLuaProfileStop",
            "PerfPickEvent", "PerfCycleFormat", "PerfAnnotate", "PerfToggleAnnotations", "PerfAnnotateSelection",
            "PerfAnnotateFunction", "PerfHottestLines", "PerfHottestSymbols", "PerfHottestCallersSelection",
            "PerfHottestCallersFunction" },
        lazy = true,
        config = function()
            require("core.plugins.perf")
        end,
        init = function()
            require("which-key").register({ p = { name = "Perf", l = { name = "Load" } }, }, { prefix = "<leader>" })
        end,
        keys = {
            { "<leader>plf", ":PerfLoadFlat<CR>", desc = "load flat" },
            { "<leader>plg", ":PerfLoadCallGraph<CR>", desc = "load call graph" },
            { "<leader>plo", ":PerfLoadFlameGraph<CR>", desc = "load flame graph" },

            { "<leader>pe", ":PerfPickEvent<CR>", desc = "pick event" },
            { "<leader>pa", ":PerfAnnotate<CR>", desc = "annotate" },
            { "<leader>pf", ":PerfAnnotateFunction<CR>", desc = "annotate function" },
            { "<leader>pA", ":PerfAnnotateSelection<CR>", desc = "annotate selection" },
            { "<leader>pn", ":PerfToggleAnnotations<CR>", desc = "toggle annotate" },
            { "<leader>ph", ":PerfHottestLines<CR>", desc = "hottest lines" },
            { "<leader>ps", ":PerfHottestSymbols<CR>", desc = "hottest symbols" },
            { "<leader>pc", ":PerfHottestCallersFunction<CR>", desc = "hottest callers function" },
            { "<leader>pC", ":PerfHottestCallersSelection<CR>", desc = "hottest callers selection" },
        }
    },
    {
        "phaazon/hop.nvim",
        dir = gen.hop_nvim,
        name = "hop_nvim",
        module = "hop",
        lazy = true,
        config = function()
            require 'hop'.setup({

            })
        end,
        init = function()
            require("which-key").register({
                t = { name = "Hop" },
            }, { prefix = "<leader>" })
        end,
        keys = {
            { "<leader>j", function() require 'hop'.hint_char1() end, desc = "hop char1" },
            { "<leader>k", function() require 'hop'.hint_char2() end, desc = "hop char1" },

            { "<leader>ta", function() require 'hop'.hint_anywhere() end, desc = "any" },
            { "<leader>tw", function() require 'hop'.hint_words() end, desc = "words" },
            { "<leader>tc", function() require 'hop'.hint_char1() end, desc = "char1" },
            { "<leader>th", function() require 'hop'.hint_char1() end, desc = "char1" },
            { "<leader>te",
                function() require 'hop'.hint_char1({ direction = require 'hop.hint'.HintDirection.AFTER_CURSOR }) end,
                desc = "back" },
            { "<leader>tb",
                function() require 'hop'.hint_char1({ direction = require 'hop.hint'.HintDirection.BEFORE_CURSOR }) end,
                desc = "forward" },
        }
    },
    {
        "chipsenkbeil/distant.nvim",
        dir = gen.distant,
        name = "distant",
        module = "distant",
        cmd = { "DistantOpen", "DistantLaunch", "DistantInstall" },
        lazy = true,
        config = function()
            require("core.plugins.others").distant()
        end,
    }
}, {
    -- root = gen.core or (vim.fn.stdpath("data") .. "/lazy"), -- directory where plugins will be installed
    defaults = {
        -- lazy = true, -- should plugins be lazy-loaded?
    },
    ui = {
        border = "rounded",
    },
    readme = {
        enabled = gen.core ~= nil,
        -- root = gen.core or (vim.fn.stdpath("state") .. "/lazy/readme"),
        files = { "README.md", "lua/**/README.md" },
        -- only generate markdown helptags for plugins that dont have docs
        skip_if_doc_exists = true,
    },
    performance = {
        cache = {
            enabled = true,
        },
        reset_packpath = true, -- reset the package path to improve startup time
        rtp = {
            reset = true, -- reset the runtime path to $VIMRUNTIME and your config directory
            ---@type string[]
            paths = {}, -- add any custom paths here that you want to includes in the rtp
            ---@type string[] list any plugins you want to disable here
            disabled_plugins = {
                -- "gzip",
                -- "matchit",
                -- "matchparen",
                -- "netrwPlugin",
                -- "tarPlugin",
                -- "tohtml",
                -- "tutor",
                -- "zipPlugin",
            },
        },
    },
}
)
