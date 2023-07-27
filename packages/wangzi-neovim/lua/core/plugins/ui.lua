return {
    {
        "navarasu/onedark.nvim",
        dir = gen.onedark_nvim,
        name = "onedark_nvim",
        config = function()
            require("core.plugins.onedark")
        end,
    },
    {
        "folke/which-key.nvim",
        dir = gen.which_key,
        name = "which_key",
        module = "which-key",
        lazy = true,
        event = "VeryLazy",
        config = function()
            require("which-key").setup({})
        end,
    },
    {
        "folke/noice.nvim",
        dir = gen.noice_nvim,
        name = "noice",
        dependencies = {
            "notify_nvim",
            "nvim_treesitter",
            {
                "MunifTanjim/nui.nvim",
                dir = gen.nui_nvim,
            }
        },
        event = "VeryLazy",
        config = function()
            require("nvim-tree")
            vim.opt.lazyredraw = false
            require("noice").setup({
                background_colour = "#000000",
                cmdline = {
                    enabled = true,         -- enables the Noice cmdline UI
                    view = "cmdline_popup", -- view for rendering the cmdline. Change to `cmdline` to get a classic cmdline at the bottom
                    opts = {},              -- global options for the cmdline. See section on views
                    ---@type table<string, CmdlineFormat>
                    format = {
                        -- conceal: (default=true) This will hide the text in the cmdline that matches the pattern.
                        -- view: (default is cmdline view)
                        -- opts: any options passed to the view
                        -- icon_hl_group: optional hl_group for the icon
                        -- title: set to anything or empty string to hide
                        cmdline = { pattern = "^:", icon = "", lang = "vim" },
                        search_down = { kind = "search", pattern = "^/", icon = " ", lang = "regex" },
                        search_up = { kind = "search", pattern = "^%?", icon = " ", lang = "regex" },
                        filter = { pattern = "^:%s*!", icon = "$", lang = "bash" },
                        lua = { pattern = "^:%s*lua%s+", icon = "", lang = "lua" },
                        help = { pattern = "^:%s*he?l?p?%s+", icon = "" },
                        input = {}, -- Used by input()
                        -- lua = false, -- to disable a format, set to `false`
                    },
                },
                lsp = {
                    -- override markdown rendering so that **cmp** and other plugins use **Treesitter**
                    override = {
                        ["vim.lsp.util.convert_input_to_markdown_lines"] = false,
                        ["vim.lsp.util.stylize_markdown"] = false,
                        ["cmp.entry.get_documentation"] = false,
                        -- ["textDocument.signatureHelp"] = false,
                    },
                    progress = {
                        enabled = true,
                        -- Lsp Progress is formatted using the builtins for lsp_progress. See config.format.builtin
                        -- See the section on formatting for more details on how to customize.
                        --- @type NoiceFormat|string
                        format = "lsp_progress",
                        --- @type NoiceFormat|string
                        format_done = "lsp_progress_done",
                        throttle = 1000 / 30, -- frequency to update lsp progress message
                        view = "mini",
                    },
                    hover = { enabled = false, },
                    signature = { enabled = false, },
                    message = {
                        enabled = true,
                        view = "notify",
                        opts = {},
                    },
                    documentation = {
                        view = "hover",
                        ---@type NoiceViewOptions
                        opts = {
                            lang = "markdown",
                            replace = true,
                            render = "plain",
                            format = { "{message}" },
                            win_options = { concealcursor = "n", conceallevel = 3 },
                        },
                    },
                },
                presets = {
                    bottom_search = false,         -- use a classic bottom cmdline for search
                    command_palette = false,       -- position the cmdline and popupmenu together
                    long_message_to_split = false, -- long messages will be sent to a split
                    inc_rename = false,            -- enables an input dialog for inc-rename.nvim
                    lsp_doc_border = false,        -- add a border to hover docs and signature help
                },
                messages = {
                    -- NOTE: If you enable messages, then the cmdline is enabled automatically.
                    -- This is a current Neovim limitation.
                    enabled = true,              -- enables the Noice messages UI
                    view = "notify",             -- default view for messages
                    view_error = "notify",       -- view for errors
                    view_warn = "notify",        -- view for warnings
                    view_history = "messages",   -- view for :messages
                    view_search = "virtualtext", -- view for search count messages. Set to `false` to disable
                },
                popupmenu = {
                    enabled = true,  -- enables the Noice popupmenu UI
                    ---@type 'nui'|'cmp'
                    backend = "nui", -- backend to use to show regular cmdline completions
                    ---@type NoicePopupmenuItemKind|false
                    -- Icons for completion item kinds (see defaults at noice.config.icons.kinds)
                    kind_icons = {}, -- set to `false` to disable icons
                },
                -- default options for require('noice').redirect
                -- see the section on Command Redirection
                ---@type NoiceRouteConfig
                redirect = {
                    view = "popup",
                    filter = { event = "msg_show" },
                },
                -- You can add any custom commands below that will be available with `:Noice command`
                ---@type table<string, NoiceCommand>
                commands = {
                    history = {
                        -- options for the message history that you get with `:Noice`
                        view = "split",
                        opts = { enter = true, format = "details" },
                        filter = {
                            any = {
                                { event = "notify" },
                                { error = true },
                                { warning = true },
                                { event = "msg_show", kind = { "" } },
                                { event = "lsp",      kind = "message" },
                            },
                        },
                    },
                    -- :Noice last
                    last = {
                        view = "popup",
                        opts = { enter = true, format = "details" },
                        filter = {
                            any = {
                                { event = "notify" },
                                { error = true },
                                { warning = true },
                                { event = "msg_show", kind = { "" } },
                                { event = "lsp",      kind = "message" },
                            },
                        },
                        filter_opts = { count = 1 },
                    },
                    -- :Noice errors
                    errors = {
                        -- options for the message history that you get with `:Noice`
                        view = "popup",
                        opts = { enter = true, format = "details" },
                        filter = { error = true },
                        filter_opts = { reverse = true },
                    },
                },
                notify = {
                    enabled = false,
                    view = "notify",
                },
                markdown = {
                    highlights = {
                        ["|%S-|"] = "@text.reference",
                        ["@%S+"] = "@parameter",
                        ["^%s*(Parameters:)"] = "@text.title",
                        ["^%s*(Return:)"] = "@text.title",
                        ["^%s*(See also:)"] = "@text.title",
                        ["{%S-}"] = "@parameter",
                    },
                },
                health = {
                    checker = true, -- Disable if you don't want health checks to run
                },
                smart_move = {
                    -- noice tries to move out of the way of existing floating windows.
                    enabled = true, -- you can disable this behaviour here
                    -- add any filetypes here, that shouldn't trigger smart move.
                    excluded_filetypes = { "cmp_menu", "cmp_docs", "notify" },
                },
                ---@type NoicePresthrottle = 1000 / 30, -- how frequently does Noice need to check for ui updates? This has no effect when in blocking mode.
                ---@type NoiceConfigViews
                views = {}, ---@see section on views
                ---@type NoiceRouteConfig[]
                routes = {}, --- @see section on routes
                ---@type table<string, NoiceFilter>
                status = {}, --- @see section on statusline components
                ---@type NoiceFormatOptions
                format = {}, --- @see section on formatting
            })
            vim.opt.lazyredraw = true

            local notify = vim.notify
            vim.notify = function(msg, level, opt, ...)
                if msg:find("warning: multiple different client offset_encodings", 1, true) then
                    return
                end
                if msg:find("query: invalid node type at position ", 1, true) then
                    return
                end
                if msg:find("69_get_delim_multi", 1, true) then
                    return
                end
                if level == "error" then
                    if msg:find("处理 CursorMoved 自动命令", 1, true) then
                        return
                    end
                    if msg:find("自动命令", 1, true) then
                        return
                    end
                    opt = opt or {}
                    opt.timeout = 1000
                end

                notify(msg, level, opt, ...)
            end
        end
    },
    {
        "rcarriga/nvim-notify",
        dir = gen.notify_nvim,
        name = "notify_nvim",
        module = "notify",
        event = "VeryLazy",
        config = function()
            require("notify").setup({
                timeout = 2500,
                max_width = 64,
                max_height = 16,
            })
        end,
    },
    {
        "kyazdani42/nvim-web-devicons",
        dir = gen.nvim_web_devicons,
        name = "nvim_web_devicons",
        lazy = true,
        config = function()
            require("core.plugins.icons")
        end,
        event = "VeryLazy",
        build = ":TSUpdate",
    },
    {
        "goolord/alpha-nvim",
        dir = gen.alpha_nvim,
        name = "alpha_nvim",
        config = function()
            require("core.plugins.alpha")
        end,
    },
    -- {
    --     "feline-nvim/feline.nvim",
    --     dir = gen.feline_nvim,
    --     name = "feline_nvim",
    --     dependencies = { "onedark_nvim" },
    --     config = function()
    --         require("core.plugins.statusline")
    --     end,
    -- },
    {
        "nvim-lualine/lualine.nvim",
        dir = gen.lualine_nvim,
        name = "lualine_nvim",
        dependencies = { "nvim_web_devicons", "onedark_nvim" },
        config = function()
            require("core.plugins.lualine")
        end,
    },
    {
        "akinsho/bufferline.nvim",
        dir = gen.bufferline_nvim,
        name = "bufferline_nvim",
        dependencies = { "nvim_web_devicons", "onedark_nvim" },
        module = "bufferline",
        lazy = true,
        event = "BufReadPost",
        config = function()
            require("core.plugins.bufferline")
        end,
        keys = {
            { "<Tab>",   function() require("bufferline").cycle(1) end,  mode = "n", desc = "next tab" },
            { "<S-Tab>", function() require("bufferline").cycle(-1) end, mode = "n", desc = "prev tab" },
        }
    },
    {
        "lukas-reineke/indent-blankline.nvim",
        dir = gen.indent_blankline_nvim,
        name = "indent_blankline_nvim",
        event = "VeryLazy",
        lazy = true,
        config = function()
            require("indent_blankline").setup({
                indentLine_enabled = 1,
                char = "▏",
                filetype_exclude = {
                    "help",
                    "terminal",
                    "alpha",
                    "packer",
                    "lspinfo",
                    "TelescopePrompt",
                    "TelescopeResults",
                    "nvchad_cheatsheet",
                    "lsp-installer",
                    "",
                },
                buftype_exclude = { "terminal" },
                show_trailing_blankline_indent = false,
                show_first_indent_level = false,
                show_end_of_line = true,
                show_current_context = true,
                show_current_context_start = true,
            })
        end,
    },
    {
        "norcalli/nvim-colorizer.lua",
        dir = gen.nvim_colorizer_lua,
        name = "nvim_colorizer_lua",
        event = "VeryLazy",
        lazy = true,
        config = function()
            require("colorizer").setup({
            }, {
                RGB = true,      -- #RGB hex codes
                RRGGBB = true,   -- #RRGGBB hex codes
                names = true,    -- "Name" codes like Blue
                RRGGBBAA = true, -- #RRGGBBAA hex codes
                rgb_fn = true,   -- CSS rgb() and rgba() functions
                hsl_fn = true,   -- CSS hsl() and hsla() functions
                css = true,      -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
                css_fn = true,   -- Enable all CSS *functions*: rgb_fn, hsl_fn

                -- Available modes: foreground, background
                mode = "background", -- Set the display mode.
            })
            -- vim.cmd("ColorizerReloadAllBuffers")
        end,
    },
    {
        "m00qek/baleia.nvim",
        dir = gen.baleia_nvim,
        tag = "v1.2.0",
        name = "baleia_nvim",
        event = "VeryLazy",
        cmd = "BaleiaColorize",
        lazy = true,
        module = "baleia",
        config = function()
            vim.cmd [[
                command! BaleiaColorize call luaeval("require('baleia').setup { }").once(bufnr('%'))

                autocmd BufWinEnter my-buffer call luaeval("require('baleia').setup { }").automatically(bufnr('%'))
                autocmd BufWinEnter *.log call luaeval("require('baleia').setup { }").automatically(bufnr('%'))
            ]]
        end,
    },
    {
        "stevearc/dressing.nvim",
        dir = gen.dressing_nvim,
        name = "dressing_nvim",
        lazy = true,
        event = "VeryLazy",
        config = function()
            require("core.plugins.dressing")
        end,
    },
}
