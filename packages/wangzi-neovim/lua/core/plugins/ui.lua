local function isFloadWindow(win)
    return vim.api.nvim_win_get_config(win).relative == "win"
end

return {
    -- require("core.plugins.onedark"),
    require("core.plugins.tokyonight"),
    require("core.plugins.heirline"),
    require("core.plugins.snacks"),
    require("core.plugins.dressing"),
    require("core.plugins.noice"),
    require("core.plugins.edgy"),
    {
        "folke/which-key.nvim",
        dir = gen.which_key,
        name = "which_key",
        module = "which-key",
        lazy = true,
        event = "VeryLazy",
        opts = {
            notify = false,
            preset = "modern",
        }
    },
    {
        "s1n7ax/nvim-window-picker",
        dir = gen.nvim_window_picker,
        name = "nvim_window_picker",
        lazy = true,
        event = "VeryLazy",
        version = "2.*",
        opts = {
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
        },
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
        "rcarriga/nvim-notify",
        dir = gen.notify_nvim,
        name = "notify",
        module = "notify",
        opts = {
            timeout = 2500,
            max_width = 64,
            max_height = 16,
            stages = "static",
            render = "compact",
        },
    },
    {
        "kyazdani42/nvim-web-devicons",
        dir = gen.nvim_web_devicons,
        name = "nvim_web_devicons",
        lazy = false,
        event = "VeryLazy",
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
        opts = {
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
        }
    },
    {
        "petertriho/nvim-scrollbar",
        dir = gen.scrollbar,
        name = "scrollbar",
        module = { "scrollbar", "scrollbar.handlers.search", "scrollbar.handlers.gitsigns" },
        lazy = true,
        event = { "BufNewFile", "BufReadPost" },
        opts = {
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
        },
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
        opts = {
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
        },
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
