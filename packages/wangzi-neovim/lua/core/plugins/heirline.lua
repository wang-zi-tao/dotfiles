local function config()
    local heirline            = require "heirline"
    local heirline_components = require "heirline-components.all"
    local conditions          = require("heirline.conditions")
    local utils               = require("heirline.utils")
    local symbols             = require("core.theme").symbols
    local components          = heirline_components.component

    local tokyonight_colors   = require("tokyonight.colors").setup()

    local function quit()
        return {
            provider = " " .. symbols.close .. " ",
            on_click = {
                callback = function()
                    vim.cmd [[quitall]]
                end,
                name = "quit",
            }
        }
    end

    local function close_buffer()
        return {
            provider = " " .. symbols.close .. " ",
            on_click = {
                callback = function()
                    vim.cmd [[quit]]
                end,
                name = "close_buffer",
            }
        }
    end

    local function tabline_buffers()
        return components.tabline_buffers({
            hl = function(self)
                local tab_type = self.tab_type

                if self._show_picker and self.tab_type ~= "buffer_active" then
                    tab_type = "buffer_visible"
                end

                if tab_type == "buffer_active" then
                    return { fg = "blue", bold = true }
                else
                    return { fg = "white", }
                end
            end,
        })
    end

    local function mode_color(self)
        if conditions.is_active() then
            return self.mode_colors[vim.fn.mode():sub(1, 1)]
        else
            return "fg_gutter"
        end
    end

    local function mode_hl(self)
        return {
            bg = self:mode_color(),
            fg = "white",
        }
    end

    local function bg2_hl(self)
        return {
            bg = "fg_gutter",
        }
    end

    local function set_hl_mode(opts)
        opts.hl = mode_hl
        opts.surround = { color = mode_color }
        return opts
    end

    local function set_hl_bg2(opts)
        opts.hl = bg2_hl
        opts.surround = { color = "fg_gutter" }
        return opts
    end

    local colors = vim.tbl_extend(
        "force",
        tokyonight_colors,
        heirline_components.hl.get_colors(),
        {
            fg = "#ffffff",
            vim_mode = tokyonight_colors.blue,
            vim_mode_sep = tokyonight_colors.blue,
            bg2 = tokyonight_colors.fg_gutter,
        }
    )

    local separator1 = { ' ', ' ' }
    local separator1_empty = { '', ' ' }
    local separator2 = { "", "" }
    local separator3 = { '', '' }
    local mode_colors = {
        n = "blue",
        i = "green",
        v = "red",
        V = "red",
        ["\22"] = "cyan",
        c = "blue",
        s = "yellow",
        S = "yellow",
        ["\19"] = "purple",
        R = "orange",
        r = "orange",
        ["!"] = "red",
        t = "orange",

    }

    vim.api.nvim_create_autocmd("ModeChanged", {
        pattern = "*:*",
        callback = vim.schedule_wrap(function()
            vim.cmd("redrawstatus")
        end),
    })

    local Align = { provider = "%=" }
    local Space = { provider = " " }

    local function WindowNumber()
        return {
            provider = function()
                return vim.api.nvim_win_get_number(0)
            end,
        }
    end

    local function MacroRec()
        return {
            condition = function()
                return vim.fn.reg_recording() ~= "" and vim.o.cmdheight == 0
            end,
            provider = " ",
            hl = { fg = "orange", bold = true },
            utils.surround({ "[", "]" }, nil, {
                provider = function()
                    return vim.fn.reg_recording()
                end,
                hl = { fg = "green", bold = true },
            }),
            update = {
                "RecordingEnter",
                "RecordingLeave",
            }
        }
    end
    local function separator(char, opts)
        return {
            provider = char,
            hl = function(self)
                return {
                    fg = opts.bg or self:mode_color(),
                }
            end,
        }
    end
    local function surround(children, opts)
        return {
            separator(opts.separator[1], opts),
            {
                children,
                hl = function(self)
                    return {
                        bg = opts.bg or self:mode_color(),
                        fg = opts.fg or "white",
                        -- force = true
                    }
                end,
            },
            separator(opts.separator[2], opts),
        }
    end

    local function trouble()
        local trouble = require("trouble")
        local trouble_symbols = trouble.statusline({
            mode = "lsp_document_symbols",
            groups = {},
            title = false,
            filter = { range = true },
            format = "❱{kind_icon}{symbol.name:Normal}",
            -- The following line is needed to fix the background color
            -- Set it to the lualine section you want to use
            -- hl_group = "lualine_c_normal",
        })
        return {
            provider = trouble_symbols.get,
            condition = conditions.is_active,
        }
    end

    local function ViMode()
        return {
            provider = "󰕷 ",
            hl = function(self)
                return { bg = self:mode_color(), bold = true, }
            end,
        }
    end
    local function pwd()
        return {
            provider = function()
                return vim.fn.getcwd()
            end,
            update = "DirChanged",
        }
    end
    local function file_info(opts)
        opts = opts or {}
        opts.filename = {}
        opts.filetype = false
        return components.file_info(opts)
    end
    local StatusLine = {
        surround({
            surround({
                Space,
                ViMode(),
                WindowNumber(),
                Space,
            }, { separator = { "", separator2[2] } }),
            Space,
            file_info(set_hl_bg2({})),
            components.git_diff(set_hl_bg2({})),
        }, { separator = { "", separator2[2] }, bg = "bg2" }),
        components.fill(),
        {
            components.cmd_info(),
            MacroRec(),
        },
        components.fill(),
        components.lsp(),
        surround({
            Space,
            components.diagnostics(set_hl_bg2({})),
            surround({
                components.nav({
                    scrollbar = false,
                    hl = mode_hl,
                    surround = { color = mode_color }
                }),
                Space,
            }, { separator = { separator2[1], "" } })
        }, { separator = { separator2[1], "" }, bg = "bg2" }),
        static = {
            mode_colors = mode_colors,
            mode_color = mode_color,
        },
    }
    local WinBar = {
        init = function(self) self.bufnr = vim.api.nvim_get_current_buf() end,
        Space,
        -- components.breadcrumbs(),
        trouble(),
        components.fill(),
        components.cmd_info(),
        surround({
            file_info(set_hl_bg2({})),
            surround({
                Space,
                WindowNumber(),
                Space,
                ViMode(),
                close_buffer(),
                Space,
            }, { separator = { separator3[1], "" } }),
        }, { separator = { separator3[1], "" }, bg = "bg2" }),
        static = {
            mode_colors = mode_colors,
            mode_color = mode_color,
        },
    }
    local TabLine = {
        surround({
            surround({
                Space,
                ViMode(),
                components.neotree({ fg = "white" }),
                Space,
            }, { separator = { "", separator1[2] } }),
            pwd(),
            { provider = separator1_empty[2] },
            file_info(set_hl_bg2({})),
        }, { separator = { "", separator1[2] }, bg = "bg2" }),
        components.fill(),
        tabline_buffers(),
        components.fill(),
        components.virtual_env(),
        components.file_encoding(),
        surround({
                Space,
                components.git_diff(set_hl_bg2({})),
                Space,
                components.diagnostics(set_hl_bg2({})),
                Space,
                components.git_branch(set_hl_bg2({})),
                surround({
                        Space,
                        quit(),
                        Space,
                    },
                    { separator = { separator1[1], "" } }),
            },
            { separator = { separator1[1], "" }, bg = "bg2" }),
        static = {
            mode_colors = mode_colors,
            mode_color = mode_color,
        },
    }
    local StatusColumn = {
        components.signcolumn(),
        components.numbercolumn(),
    }
    local function is_disabled(args)
        return not require("heirline-components.buffer").is_valid(args.buf) or
            conditions.buffer_matches({
                buftype = { "terminal", "prompt", "nofile", "help", "quickfix" },
                filetype = { "NvimTree", "neo%-tree", "dashboard", "Outline", "aerial", "trouble", "codecompanion", "sagaoutline", "Trouble", "qf" },
            }, args.buf)
    end
    heirline_components.init.subscribe_to_events()
    require("heirline").setup({
        statusline = StatusLine,
        winbar = WinBar,
        tabline = TabLine,
        statuscolumn = StatusColumn,
        opts = {
            disable_winbar_cb = is_disabled,
            colors = colors,
        }
    })
end

return {
    {
        "rebelot/heirline.nvim",
        name = "heirline",
        package = "heirline",
        event = "VeryLazy",
        dependencies = "heirline-components",
        config = config
    },
    {
        "Zeioth/heirline-components.nvim",
        name = "heirline-components",
        package = "heirline-components.all",
        lazy = true
    }
}
