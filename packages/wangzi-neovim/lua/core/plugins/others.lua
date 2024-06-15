local n = require("core.gen")
local M = {}
M.comment = function()
    require("nvim_comment").setup({
        -- Linters prefer comment and line to have a space in between markers
        marker_padding = true,
        -- should comment out empty or whitespace only lines
        comment_empty = true,
        -- trim empty comment whitespace
        comment_empty_trim_whitespace = true,
        -- Should key mappings be created
        create_mappings = true,
        -- Normal mode mapping left hand side
        line_mapping = "gcc",
        -- Visual/Operator mapping left hand side
        operator_mapping = "gc",
        -- text object mapping, comment chunk,,
        comment_chunk_text_object = "ic",
        -- Hook function to call before commenting takes place
        hook = nil,
    })
    vim.cmd([[autocmd BufEnter *.cpp,*.h :lua vim.api.nvim_buf_set_option(0, "commentstring", "// %s")]])
    vim.cmd([[autocmd BufEnter *.nix :lua vim.api.nvim_buf_set_option(0, "commentstring", "# %s")]])
end
M.marks = function()
    require("marks").setup({
        default_mappings = true,
        builtin_marks = {},
        cyclic = true,
        force_write_shada = false,
        refresh_interval = 250,
        sign_priority = { lower = 10, upper = 15, builtin = 8, bookmark = 20 },
        excluded_filetypes = {},
        bookmark_0 = {
            sign = "⚑",
            virt_text = "hello world",
        },
        mappings = {},
    })
end
M.auto_save = function()
    require("auto-save").setup({
        enabled = true,
        execution_message = {
            message = function() -- message to print on save
                return ""
            end,
            dim = 0.18, -- dim the color of `message`
            cleaning_interval = 1250, -- (milliseconds) automatically clean MsgArea after displaying `message`. See :h MsgArea
        },
        trigger_events = { "InsertLeave", "TextChanged" },
        conditions = {
            exists = false,
            filename_is_not = {},
            filetype_is_not = {},
            modifiable = true,
        },
        write_all_buffers = false,
        on_off_commands = true,
        clean_command_line_interval = 1000,
        debounce_delay = 135,
        callbacks = { -- functions to be executed at different intervals
            enabling = nil, -- ran when enabling auto-save
            disabling = nil, -- ran when disabling auto-save
            before_asserting_save = function() end, -- ran before checking `condition`
            before_saving = function() end, -- ran before doing the actual save
            after_saving = function()
                vim.notify("AutoSave: saved at " .. vim.fn.strftime("%H:%M:%S"))
            end, -- ran after doing the actual save
        },
    })
end
M.scrollbar = function()
    require("scrollbar").setup()
    require("scrollbar.handlers.search").setup()
end
M.dap_install = function()
    local dap_install = require("dap-install")
    dap_install.setup({
        installation_path = vim.fn.stdpath("data") .. "/dapinstall/",
    })
end
M.mini = function()
    -- require('mini.animate').setup()
    require("mini.surround").setup({
        n_lines = 255,
        highlight_duration = 16,
        mappings = {
            add = "S", -- Add surrounding
            delete = "ds", -- Delete surrounding
            find = "sf", -- Find surrounding (to the right)
            find_left = "sF", -- Find surrounding (to the left)
            highlight = "sh", -- Highlight surrounding
            replace = "cs", -- Replace surrounding
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
end
M.session_manager = function()
    local Path = require("plenary.path")
    require("session_manager").setup({
        sessions_dir = Path:new(vim.fn.stdpath("data"), "sessions"), -- The directory where the session files will be saved.
        path_replacer = "__", -- The character to which the path separator will be replaced for session files.
        colon_replacer = "++", -- The character to which the colon symbol will be replaced for session files.
        autoload_mode = require("session_manager.config").AutoloadMode.Disabled, -- Define what to do when Neovim is started without arguments. Possible values: Disabled, CurrentDir, LastSession
        autosave_last_session = true, -- Automatically save last session on exit and on session switch.
        autosave_ignore_not_normal = true, -- Plugin will not save a session when no buffers are opened, or all of them aren't writable or listed.
        autosave_ignore_filetypes = { -- All buffers of these file types will be closed before the session is saved.
            "gitcommit",
            "FTerm",
            "NvimTree",
        },
        autosave_only_in_session = false, -- Always autosaves session. If true, only autosaves after a session is active.
    })
end
M.distant = function()
    local actions = require("distant.nav.actions")

    local settings = {
        distant = {
            args = { "--shutdown-after", "60" },
        },
        -- Settings that apply when editing a remote file
        file = {
            mappings = {
                ["-"] = actions.up,
            },
        },
        -- Settings that apply to the navigation interface
        dir = {
            mappings = {
                ["<Return>"] = actions.edit,
                ["-"] = actions.up,
                ["K"] = actions.mkdir,
                ["N"] = actions.newfile,
                ["R"] = actions.rename,
                ["D"] = actions.remove,
            },
        },
        -- Maximimum time to wait (in milliseconds) for requests to finish
        max_timeout = 15 * 1000,
        -- Time to wait (in milliseconds) inbetween checks to see
        -- if a request timed out
        timeout_interval = 250,
        -- Time to wait (in milliseconds) inbetween polling checks to
        -- see if an async function has completed
        poll_interval = 200,
        -- Settings to use to start LSP instances
        lsp = {},
    }
    require("distant").setup({
        -- Applies Chip's personal settings to every machine you connect to
        --
        -- 1. Ensures that distant servers terminate with no connections
        -- 2. Provides navigation bindings for remote directories
        -- 3. Provides keybinding to jump into a remote file's parent directory
        ["*"] = settings,
    })
end
return M
