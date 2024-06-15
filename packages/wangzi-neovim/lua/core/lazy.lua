local gen = gen
local state = {}
global = {}

function global:toggle_term(number)
    require("toggleterm")
    self.toggleterm_nvim[number]:toggle()
end

vim.g.mapleader = " "
require("lazy").setup({
    require("core.plugins.cmp"),
    require("core.plugins.telescope"),
    require("core.plugins.treesitter"),
    require("core.plugins.dap"),
    require("core.plugins.lspconfig"),
    require("core.plugins.ui"),

    -- require("core.plugins.nvimtree"),
    require("core.plugins.neotree"),
    require("core.plugins.symbols_outline_pre"),
    require("core.plugins.gitsigns"),
    require("core.plugins.vgit"),
    require("core.plugins.ai"),

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
        name = "plenary_nvim",
        dir = gen.plenary_nvim,
        module = "plenary",
        dependencies = "core",
        lazy = true,
        keys = {
            {
                "<leader>pp",
                function()
                    require("plenary.profile").start("profile.log", { flame = true })
                    vim.notify("start profiling")
                end,
                desc = "Profile Start",
            },
            {
                "<leader>pP",
                function()
                    require("plenary.profile").stop()
                    vim.notify("end profiling")
                end,
                desc = "Profile Stop",
            },
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
        module = { "sqlite", "sqlite.tbl", "sqlite.db" },
        lazy = false,
        init = function()
            if gen.libsqlite then
                vim.g.sqlite_clib_path = gen.libsqlite
            end
        end,
    },
    require("core.plugins.trouble"),
    require("core.plugins.trailblazer"),
    gen.markdown_preview and {
        "davidgranstrom/nvim-markdown-preview",
        dir = gen.markdown_preview,
        name = "markdown_preview",
        ft = "markdown",
        cmd = "MarkdownPreview",
        lazy = true,
        disable = not gen.markdown_preview,
    } or {},
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
        "pocco81/auto-save.nvim",
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
                    vim.cmd([[NvimTreeClose]])
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
                    require("diffview").file_history("%")
                end,
                desc = "Git Log This File",
            },
            { "<leader>gf", ":DiffviewFileHistory %<CR>", desc = "File History" },
        },
    },
    {
        "numToStr/Navigator.nvim",
        dir = gen.navigator,
        name = "navigator",
        dependencies = "core",
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
        "RRethy/vim-illuminate",
        dir = gen.illuminate,
        name = "illuminate",
        module = "illuminate",
        lazy = true,
        event = "VeryLazy",
    },
    {
        "stevearc/profile.nvim",
        dir = gen.profile_nvim,
        name = "profile_nvim",
        module = "profile",
        lazy = true,
        cmd = { "ToggleProfile" },
        config = function()
            local function toggle_profile()
                local prof = require("profile")
                if prof.is_recording() then
                    prof.stop()
                    vim.ui.input(
                        { prompt = "Save profile to:", completion = "file", default = "profile.json" },
                        function(filename)
                            if filename then
                                prof.export(filename)
                                vim.notify(string.format("Wrote %s", filename))
                            end
                        end
                    )
                else
                    prof.start("*")
                end
            end
            vim.api.nvim_create_user_command("ToggleProfile", toggle_profile, {})
        end,
    },
    {
        "anuvyklack/keymap-amend.nvim",
        dir = gen.keymap_amend,
        name = "keymap_amend",
        lazy = true,
        module = "keymap-amend",
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
        "beauwilliams/focus.nvim",
        dir = gen.focus,
        name = "focus_nvim",
        dependencies = "core",
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
        "Shatur/neovim-session-manager",
        dir = gen.session_manager,
        name = "session_manager",
        lazy = true,
        cmd = { "SessionManager" },
        module = "session_manager",
        event = "VeryLazy",
        dependencies = { "trailblazer" },
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
        cmd = {
            "PerfLoadFlat",
            "PerfLoadCallGraph",
            "PerfLoadFlameGraph",
            "PerfLuaProfileStart",
            "PerfLuaProfileStop",
            "PerfPickEvent",
            "PerfCycleFormat",
            "PerfAnnotate",
            "PerfToggleAnnotations",
            "PerfAnnotateSelection",
            "PerfAnnotateFunction",
            "PerfHottestLines",
            "PerfHottestSymbols",
            "PerfHottestCallersSelection",
            "PerfHottestCallersFunction",
        },
        lazy = true,
        config = function()
            require("core.plugins.perf")
        end,
        init = function()
            require("which-key").register({ p = { name = "Perf", l = { name = "Load" } } }, { prefix = "<leader>" })
        end,
        keys = {
            { "<leader>plf", ":PerfLoadFlat<CR>",                desc = "load flat" },
            { "<leader>plg", ":PerfLoadCallGraph<CR>",           desc = "load call graph" },
            { "<leader>plo", ":PerfLoadFlameGraph<CR>",          desc = "load flame graph" },
            { "<leader>pe",  ":PerfPickEvent<CR>",               desc = "pick event" },
            { "<leader>pa",  ":PerfAnnotate<CR>",                desc = "annotate" },
            { "<leader>pf",  ":PerfAnnotateFunction<CR>",        desc = "annotate function" },
            { "<leader>pA",  ":PerfAnnotateSelection<CR>",       desc = "annotate selection" },
            { "<leader>pn",  ":PerfToggleAnnotations<CR>",       desc = "toggle annotate" },
            { "<leader>ph",  ":PerfHottestLines<CR>",            desc = "hottest lines" },
            { "<leader>ps",  ":PerfHottestSymbols<CR>",          desc = "hottest symbols" },
            { "<leader>pc",  ":PerfHottestCallersFunction<CR>",  desc = "hottest callers function" },
            { "<leader>pC",  ":PerfHottestCallersSelection<CR>", desc = "hottest callers selection" },
        },
    },
    {
        "phaazon/hop.nvim",
        dir = gen.hop_nvim,
        name = "hop_nvim",
        module = "hop",
        lazy = true,
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
        "chipsenkbeil/distant.nvim",
        dir = gen.distant,
        name = "distant",
        module = "distant",
        cmd = { "DistantOpen", "DistantLaunch", "DistantInstall" },
        lazy = true,
        config = function()
            require("core.plugins.others").distant()
        end,
    },
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
    concurrency = 16,
    git = {
        -- defaults for the `Lazy log` command
        -- log = { "-10" }, -- show the last 10 commits
        log = { "--since=3 days ago" }, -- show commits from the last 3 days
        timeout = 120,                  -- kill processes that take more than 2 minutes
        -- lazy.nvim requires git >=2.19.0. If you really want to use lazy with an older version,
        -- then set the below to false. This should work, but is NOT supported and will
        -- increase downloads a lot.
        filter = true,
    },
    performance = {
        cache = {
            enabled = true,
        },
        reset_packpath = true, -- reset the package path to improve startup time
        rtp = {
            reset = true,      -- reset the runtime path to $VIMRUNTIME and your config directory
            ---@type string[]
            paths = {},        -- add any custom paths here that you want to includes in the rtp
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
})
