return {
    -- require("core.plugins.onedark"),
    require("core.plugins.tokyonight"),
    require("core.plugins.heirline"),
    require("core.plugins.snacks"),
    {
        "folke/which-key.nvim",
        dir = gen.which_key,
        name = "which_key",
        module = "which-key",
        lazy = true,
        event = "VeryLazy",
        config = function()
            require("which-key").setup({
                notify = false,
                preset = "modern",
            })
        end,
    },
    {
        "s1n7ax/nvim-window-picker",
        dir = gen.nvim_window_picker,
        name = "nvim_window_picker",
        lazy = true,
        event = "VeryLazy",
        version = "2.*",
        config = function()
            require("window-picker").setup({
                filter_rules = {
                    include_current_win = false,
                    autoselect_one = true,
                    -- filter using buffer options
                    bo = {
                        -- if the file type is one of following, the window will be ignored
                        filetype = { "neo-tree", "neo-tree-popup", "notify" },
                        -- if the buffer type is one of following, the window will be ignored
                        buftype = { "terminal", "quickfix" },
                    },
                },
            })
        end,
    },
    {
        "MunifTanjim/nui.nvim",
        name = "nui_nvim",
        dir = gen.nui_nvim,
        lazy = true,
    },
    {
        "SmiteshP/nvim-navic",
        dir = gen.navic,
        name = "navic",
        lazy = true,
    },
    {
        "folke/noice.nvim",
        dir = gen.noice_nvim,
        name = "noice",
        dependencies = {
            "notify_nvim",
            "nvim_treesitter",
            "nui_nvim",
        },
        event = "VeryLazy",
        config = function()
            pcall(require, "nvim-tree")
            vim.opt.lazyredraw = false
            require("noice").setup({
                background_colour = "#000000",
                cmdline = {
                    enabled = true,         -- enables the Noice cmdline UI
                    view = "cmdline_popup", -- view for rendering the cmdline. Change to `cmdline` to get a classic cmdline at the bottom
                    opts = {},              -- global options for the cmdline. See section on views
                    ---@type table<string, CmdlineFormat>
                    format = {
                        conceal = false,
                        -- conceal: (default=true) This will hide the text in the cmdline that matches the pattern.
                        -- view: (default is cmdline view)
                        -- opts: any options passed to the view
                        -- icon_hl_group: optional hl_group for the icon
                        -- title: set to anything or empty string to hide
                        cmdline = { pattern = "^:", icon = "", lang = "vim" },
                        search_down = { kind = "search", pattern = "^/", icon = " ", lang = "regex" },
                        search_up = { kind = "search", pattern = "^%?", icon = " ", lang = "regex" },
                        filter = { pattern = "^:%s*!", icon = "$", lang = "bash" },
                        lua = { pattern = "^:%s*lua%s+", icon = " ", lang = "lua" },
                        help = { pattern = "^:%s*he?l?p?%s+", icon = "󰘥 " },
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
                        enabled = false,
                        -- Lsp Progress is formatted using the builtins for lsp_progress. See config.format.builtin
                        -- See the section on formatting for more details on how to customize.
                        --- @type NoiceFormat|string
                        format = "lsp_progress",
                        --- @type NoiceFormat|string
                        format_done = "lsp_progress_done",
                        throttle = 8, -- frequency to update lsp progress message
                        view = "mini",
                    },
                    hover = { enabled = false },
                    signature = { enabled = false },
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
                            win_options = { concealcursor = "n", conceallevel = 1 },
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
                views = {
                    mini = { enable = false },
                    cmdline_popup = { border = { style = "rounded" }, },
                }, ---@see section on views
                ---@type NoiceRouteConfig[]
                routes = {}, --- @see section on routes
                ---@type table<string, NoiceFilter>
                status = {}, --- @see section on statusline components
                ---@type NoiceFormatOptions
                format = {}, --- @see section on formatting
            })
            vim.opt.lazyredraw = true
        end,
    },
    {
        "rcarriga/nvim-notify",
        dir = gen.notify_nvim,
        name = "notify_nvim",
        module = "notify",
        config = function()
            require("notify").setup({
                timeout = 2500,
                max_width = 64,
                max_height = 16,
                stages = "static",
                render = "compact",
            })
        end,
    },
    {
        "kyazdani42/nvim-web-devicons",
        dir = gen.nvim_web_devicons,
        name = "nvim_web_devicons",
        lazy = false,
        config = function()
            require("core.plugins.icons")
        end,
        event = "VeryLazy",
        build = ":TSUpdate",
    },
    {
        "folke/edgy.nvim",
        name = "edgy",
        dir = gen.edgy,
        event = "VeryLazy",
        config = function()
            local function isFloadWindow(win)
                return vim.api.nvim_win_get_config(win).relative == "win"
            end
            require("edgy").setup({
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
                            return vim.b[buf].neo_tree_source == "filesystem" and not isFloadWindow(win)
                        end,
                    },
                    {
                        title = "Neo-Tree Git",
                        ft = "neo-tree",
                        filter = function(buf, win)
                            return vim.b[buf].neo_tree_source == "git_status" and not isFloadWindow(win)
                        end,
                        open = "Neotree position=left git_status",
                    },
                    {
                        title = "Neo-Tree Buffers",
                        ft = "neo-tree",
                        filter = function(buf, win)
                            return vim.b[buf].neo_tree_source == "buffers" and not isFloadWindow(win)
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
            })
        end,
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
    },
    {
        "goolord/alpha-nvim",
        dir = gen.alpha_nvim,
        name = "alpha_nvim",
        enabled = false,
        config = function()
            require("core.plugins.alpha")
            require("core.theme").setup()
        end,
    },
    -- {
    --     "feline-nvim/feline.nvim",
    --     dir = gen.feline_nvim,
    --     name = "feline_nvim",
    --     dependencies = {},
    --     config = function()
    --         require("core.plugins.statusline")
    --     end,
    -- },
    {
        "nvim-lualine/lualine.nvim",
        dir = gen.lualine_nvim,
        name = "lualine_nvim",
        enabled = false,
        dependencies = {
            "nvim_web_devicons",
            "trouble_nvim",
        },
        config = function()
            require("core.plugins.lualine")
        end,
    },
    {
        "j-hui/fidget.nvim",
        dir = gen.fidget_nvim,
        name = "fidget_nvim",
        module = "fidget",
        event = "VeryLazy",
        config = function()
            require("fidget").setup({
                notification = {
                    window = {
                    }
                }
            })
            vim.api.nvim_create_autocmd("LspProgress", {
                pattern = "end",
                callback = function(ev)
                    local token = ev.data.params.token
                    local client_id = ev.data.client_id
                    local client = client_id and vim.lsp.get_client_by_id(client_id)
                    if client and token then
                        require("fidget").notification.remove(client.name, token)
                    end
                end,
            })
        end
    },
    {
        "akinsho/bufferline.nvim",
        dir = gen.bufferline_nvim,
        name = "bufferline_nvim",
        dependencies = { "nvim_web_devicons", },
        module = "bufferline",
        lazy = true,
        enabled = false,
        event = "BufReadPost",
        config = function()
            require("core.plugins.bufferline")
        end,
        keys = {
            {
                "<Tab>",
                function()
                    require("bufferline").cycle(1)
                end,
                mode = "n",
                desc = "next tab",
            },
            {
                "<S-Tab>",
                function()
                    require("bufferline").cycle(-1)
                end,
                mode = "n",
                desc = "prev tab",
            },
        },
    },
    {
        "lukas-reineke/indent-blankline.nvim",
        dir = gen.indent_blankline_nvim,
        name = "indent_blankline_nvim",
        event = "VeryLazy",
        lazy = true,
        main = "ibl",
        dependencies = { "tokyonight" },
        config = function()
            local hooks = require("ibl.hooks")
            local highlight = {
                "RainbowRed",
                "RainbowYellow",
                "RainbowBlue",
                "RainbowOrange",
                "RainbowGreen",
                "RainbowViolet",
                "RainbowCyan",
            }

            require("ibl").setup({
                scope = {
                    highlight = highlight,
                },
                indent = {
                    char = "▏",
                },
                whitespace = {
                    highlight = highlight,
                    remove_blankline_trail = false,
                },
                exclude = {
                    filetypes = {
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
                    buftypes = { "terminal" },
                },
            })

            hooks.register(hooks.type.SCOPE_HIGHLIGHT, hooks.builtin.scope_highlight_from_extmark)
        end,
    },
    {
        "catgoose/nvim-colorizer.lua",
        dir = gen.nvim_colorizer_lua,
        name = "nvim_colorizer_lua_fork",
        event = "VeryLazy",
        lazy = true,
        config = function()
            require("colorizer").setup({
                filetypes = { "*" },
                buftypes = {},
                lazy_load = true,
                user_default_options = {
                    RGB = true,      -- #RGB hex codes
                    RRGGBB = true,   -- #RRGGBB hex codes
                    names = true,    -- "Name" codes like Blue
                    RRGGBBAA = true, -- #RRGGBBAA hex codes
                    rgb_fn = true,   -- CSS rgb() and rgba() functions
                    hsl_fn = true,   -- CSS hsl() and hsla() functions
                    css = true,      -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
                    css_fn = true,   -- Enable all CSS *functions*: rgb_fn, hsl_fn

                    -- Available modes: foreground, background
                    mode = "virtualtext", -- Set the display mode.
                    virtualtext = "",
                }
            })
            -- vim.cmd("ColorizerReloadAllBuffers")
        end,
    },
    -- {
    --     "m00qek/baleia.nvim",
    --     dir = gen.baleia_nvim,
    --     tag = "v1.2.0",
    --     name = "baleia_nvim",
    --     -- event = "VeryLazy",
    --     cmd = "BaleiaColorize",
    --     lazy = true,
    --     module = "baleia",
    --     config = function()
    --         vim.cmd([[
    --             command! BaleiaColorize call luaeval("require('baleia').setup { }").once(bufnr('%'))
    --
    --             autocmd BufWinEnter my-buffer call luaeval("require('baleia').setup { }").automatically(bufnr('%'))
    --             autocmd BufWinEnter *.log call luaeval("require('baleia').setup { }").automatically(bufnr('%'))
    --         ]])
    --     end,
    -- },
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
    {
        "petertriho/nvim-scrollbar",
        dir = gen.scrollbar,
        name = "scrollbar",
        module = { "scrollbar", "scrollbar.handlers.search", "scrollbar.handlers.gitsigns" },
        lazy = true,
        event = { "BufNewFile", "BufReadPost" },
        config = function()
            require("scrollbar").setup({
                marks = {
                    Search = {
                        text = { "-", "=" },
                        priority = 1,
                        gui = nil,
                        color = nil,
                        cterm = nil,
                        color_nr = nil, -- cterm
                        highlight = "ScrollbarSearch",
                    },
                },
                excluded_buftypes = {
                    "terminal",
                },
                excluded_filetypes = {
                    "cmp_docs",
                    "cmp_menu",
                    "noice",
                    "prompt",
                    "TelescopePrompt",
                    "neo-tree",
                },
            })
        end,
    },
    {
        "RRethy/vim-illuminate",
        dir = gen.illuminate,
        name = "illuminate",
        module = "illuminate",
        lazy = true,
        event = { "BufNewFile", "BufReadPost" },
    },
    {
        "antoinemadec/FixCursorHold.nvim",
        dir = gen.FixCursorHold_nvim,
        event = "VeryLazy",
        name = "FixCursorHold",
        lazy = true,
        config = function()
            vim.g.cursorhold_updatetime = 100
        end
    },
    {
        "nvim-neotest/nvim-nio",
        dir = gen.nvim_nio,
        name = "nvim_nio",
        lazy = true,
        module = "nio",
    },
    {
        "rmagatti/goto-preview",
        dir = gen.goto_preview,
        name = "goto_preview",
        event = "LspAttach",
        module = "goto-preview",
        config = function()
            require('goto-preview').setup {
                resizing_mappings = false,
                vim_ui_input = false,
                border = { "↖", "─", "╮", "│", "╯", "─", "╰", "│" },
                post_open_hook = function(buff, win)
                    local close = function()
                        if vim.api.nvim_win_is_valid(win) then
                            vim.api.nvim_win_close(win, true)
                        end
                    end
                    vim.keymap.set('n', '<Esc>', close, { buffer = buff })
                    vim.keymap.set('n', 'q', close, { buffer = buff })
                end,
            }
        end,
        keys = {
            {
                "gd",
                function()
                    require('goto-preview').goto_preview_definition()
                end,
                mode = "n",
                desc = "preview definition",
            },
            {
                "gt",
                function()
                    require('goto-preview').goto_preview_type_definition()
                end,
                mode = "n",
                desc = "preview type definition",
            },
            {
                "gr",
                function()
                    require('goto-preview').goto_preview_references()
                end,
                mode = "n",
                desc = "preview references",
            },
        },
        dependencies = {
            {
                "rmagatti/logger.nvim",
                name = "logger_nvim",
                dir = gen.logger_nvim,
                module = "logger",
            }
        },
    },
}
