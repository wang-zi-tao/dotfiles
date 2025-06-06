return {
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
        "numToStr/Comment.nvim",
        dir = gen.comment_nvim,
        name = "comment_nvim",
        module = "nvim_comment",
        event = "BufEnter",
        config = function()
            require('Comment').setup({
                ---Add a space b/w comment and the line
                padding = true,
                ---Whether the cursor should stay at its position
                sticky = true,
                ---Lines to be ignored while (un)comment
                ignore = nil,
                ---LHS of toggle mappings in NORMAL mode
                toggler = {
                    ---Line-comment toggle keymap
                    line = 'gcc',
                    ---Block-comment toggle keymap
                    block = 'gbc',
                },
                ---LHS of operator-pending mappings in NORMAL and VISUAL mode
                opleader = {
                    ---Line-comment keymap
                    line = 'gc',
                    ---Block-comment keymap
                    block = 'gb',
                },
                ---LHS of extra mappings
                extra = {
                    ---Add comment on the line above
                    above = 'gcO',
                    ---Add comment on the line below
                    below = 'gco',
                    ---Add comment at the end of line
                    eol = 'gcA',
                },
                ---Enable keybindings
                ---NOTE: If given `false` then the plugin won't create any mappings
                mappings = {
                    ---Operator-pending mapping; `gcc` `gbc` `gc[count]{motion}` `gb[count]{motion}`
                    basic = true,
                    ---Extra mapping; `gco`, `gcO`, `gcA`
                    extra = true,
                },
                ---Function to call before (un)comment
                pre_hook = nil,
                ---Function to call after (un)comment
                post_hook = nil,
            })
        end,
    },
    {
        "Shatur/neovim-session-manager",
        dir = gen.session_manager,
        name = "neovim-session-manager",
        lazy = true,
        cmd = { "SessionManager" },
        module = "session_manager",
        event = "VeryLazy",
        -- dependencies = { "trailblazer" },
        config = function()
            local Path = require("plenary.path")
            require("session_manager").setup({
                sessions_dir = Path:new(vim.fn.stdpath("data"), "sessions"),             -- The directory where the session files will be saved.
                path_replacer = "__",                                                    -- The character to which the path separator will be replaced for session files.
                colon_replacer = "++",                                                   -- The character to which the colon symbol will be replaced for session files.
                autoload_mode = require("session_manager.config").AutoloadMode.Disabled, -- Define what to do when Neovim is started without arguments. Possible values: Disabled, CurrentDir, LastSession
                autosave_last_session = true,                                            -- Automatically save last session on exit and on session switch.
                autosave_ignore_not_normal = true,                                       -- Plugin will not save a session when no buffers are opened, or all of them aren't writable or listed.
                autosave_ignore_filetypes = {                                            -- All buffers of these file types will be closed before the session is saved.
                    "gitcommit",
                    "FTerm",
                    "NvimTree",
                },
                autosave_only_in_session = false, -- Always autosaves session. If true, only autosaves after a session is active.
            })

            local save_time = 3 * 3600 * 1000;
            local function save()
                vim.cmd [[SessionManager save_current_session]]
                vim.defer_fn(save, save_time)
            end
            vim.defer_fn(save, save_time)

            require("core.utils").load_nvim_lua_file(vim.fn.getcwd())
        end,
    },
    {
        "echasnovski/mini.nvim",
        dir = gen.mini,
        name = "mini",
        lazy = true,
        config = function()
            -- require('mini.animate').setup()
            require("mini.surround").setup({
                n_lines = 255,
                highlight_duration = 16,
                mappings = {
                    add = "S",             -- Add surrounding
                    delete = "ds",         -- Delete surrounding
                    find = "sf",           -- Find surrounding (to the right)
                    find_left = "sF",      -- Find surrounding (to the left)
                    highlight = "sh",      -- Highlight surrounding
                    replace = "cs",        -- Replace surrounding
                    update_n_lines = "sn", -- Update `n_lines`
                },
            })
            -- require('mini.bracketed').setup({
            --     -- First-level elements are tables describing behavior of a target:
            --     --
            --     -- - <suffix> - single character suffix. Used after `[` / `]` in mappings.
            --     --   For example, with `b` creates `[B`, `[b`, `]b`, `]B` mappings.
            --     --   Supply empty string `''` to not create mappings.
            --     --
            --     -- - <options> - table overriding target options.
            --     --
            --     -- See `:h MiniBracketed.config` for more info.
            --
            --     buffer     = { suffix = 'b', options = {} },
            --     comment    = { suffix = 'c', options = {} },
            --     conflict   = { suffix = 'x', options = {} },
            --     diagnostic = { suffix = 'D', options = {} },
            --     file       = { suffix = 'f', options = {} },
            --     indent     = { suffix = 'i', options = {} },
            --     jump       = { suffix = 'j', options = {} },
            --     location   = { suffix = 'l', options = {} },
            --     oldfile    = { suffix = 'o', options = {} },
            --     quickfix   = { suffix = 'q', options = {} },
            --     treesitter = { suffix = 't', options = {} },
            --     undo       = { suffix = 'u', options = {} },
            --     window     = { suffix = 'w', options = {} },
            --     yank       = { suffix = 'y', options = {} },
            -- })
        end,
        event = "VeryLazy",
    },
    {
        "pocco81/auto-save.nvim",
        dir = gen.auto_save,
        name = "auto_save",
        lazy = true,
        event = { "TextChanged", "InsertEnter" },
        cmd = "ASToggle",
        config = function()
            require("auto-save").setup({
                enabled = true,
                execution_message = {
                    message = function() -- message to print on save
                        return ""
                    end,
                    dim = 0.18,               -- dim the color of `message`
                    cleaning_interval = 1250, -- (milliseconds) automatically clean MsgArea after displaying `message`. See :h MsgArea
                },
                trigger_events = { "InsertLeave", "TextChanged" },
                conditions = {
                    exists = false,
                    filename_is_not = {},
                    filetype_is_not = {},
                    modifiable = true,
                },
                condition = function(buf)
                    local utils = require("auto-save.utils.data")

                    if
                        vim.fn.getbufvar(buf, "&modifiable") == 1 and
                        utils.not_in(vim.fn.getbufvar(buf, "&filetype"), { "neo-tree" }) then
                        return true -- met condition(s), can save
                    end
                    return false    -- can't save
                end,
                write_all_buffers = false,
                on_off_commands = true,
                clean_command_line_interval = 1000,
                debounce_delay = 135,
                callbacks = {                               -- functions to be executed at different intervals
                    enabling = nil,                         -- ran when enabling auto-save
                    disabling = nil,                        -- ran when disabling auto-save
                    before_asserting_save = function() end, -- ran before checking `condition`
                    before_saving = function() end,         -- ran before doing the actual save
                    after_saving = function()
                        vim.notify("AutoSave: saved at " .. vim.fn.strftime("%H:%M:%S"))
                    end, -- ran after doing the actual save
                },
            })
        end
    },
    {
        "nvim-neotest/neotest",
        dir = gen.neotest,
        name = "neotest",
        dependencies = {
            "plenary_nvim",
            "nvim_treesitter",
            "nvim_nio",
            "FixCursorHold",
        },
        module = "neotest",
        init = function()
            require("which-key").add({
                { "<leader>L", group = "Neotest" },
            })
        end,
        config = function()
            require("neotest").setup({
                adapters = {
                    -- require("neotest-plenary"),
                    -- require("neotest-vim-test")({
                    --     ignore_file_types = { "python", "vim", "lua" },
                    -- }),
                    require('rustaceanvim.neotest'),
                    -- require("neotest-gtest"),
                    -- require("neotest-jest"),
                },
                consumers = {
                    overseer = require("neotest.consumers.overseer"),
                },
            })
        end,
        keys = {
            {
                "<leader>Lt",
                function()
                    require("neotest").run.run()
                end,
                desc = "Neotest run"
            },
            {
                "<leader>Lc",
                function()
                    require("neotest").run.run(vim.fn.expand("%"))
                end,
                desc = "Neotest run current file"
            },
            {
                "<leader>Ld",
                function()
                    require("neotest").run.run({ strategy = "dap" })
                end,
                desc = "Neotest debug"
            },
            {
                "<leader>Ls",
                function()
                    require("neotest").summary.toggle()
                end,
                desc = "Neotest summary"
            },
        }
    },
    {
        "figsoda/nix-develop.nvim",
        dir = gen.nix_develop_nvim,
        name = "nix_develop_nvim",
        event = "LspAttach",
    },
    {
        "LunarVim/bigfile.nvim",
        name = "bigfile",
        dir = gen.bigfile,
        config = function()
            require("bigfile").setup({
            })
        end,
    },
    {
        'axkirillov/hbac.nvim',
        name = "hbac",
        dir = gen.hbac,
        event = "VeryLazy",
        module = { "hbac", "telescope._extensions.hbac" },
        cmd = "Hbac",
        config = function()
            require("hbac").setup({
                autoclose                  = true, -- set autoclose to false if you want to close manually
                threshold                  = 32,   -- hbac will start closing unedited buffers once that number is reached
                close_command              = function(bufnr)
                    vim.api.nvim_buf_delete(bufnr, {})
                end,
                close_buffers_with_windows = false, -- hbac will close buffers with associated windows if this option is `true`
                telescope                  = {
                    -- See #telescope-configuration below
                },

            })
        end,
    },
    {
        "nvimdev/template.nvim",
        name = "template",
        dir = gen.template,
        cmd = { 'Template', 'TemProject' },
        module = "telescope._extensions.find_template",
        config = function()
            local function setup()
                local Job = require("plenary.job")

                local username_job = Job:new({ command = "git", args = { "config", "--get", "user.name" }, enable_recording = true })
                    :sync()
                local username = username_job[1] and vim.trim(username_job[1]) or ""

                local email_job = Job:new({ command = "git", args = { "config", "--get", "user.email" }, enable_recording = true })
                    :sync()
                local email = email_job[1] and vim.trim(email_job[1]) or ""

                require('template').setup({
                    temp_dir = vim.fn.stdpath('config') .. "/skeleton",
                    author = username,
                    email = email,
                })
            end
            setup()
            -- vim.api.nvim_create_autocmd("DirChanged", {
            --     pattern = "*",
            --     callback = setup,
            -- })
        end
    }
}
