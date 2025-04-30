local function config()
    local heirline                 = require "heirline"
    local heirline_components      = require "heirline-components.all"
    local conditions               = require("heirline.conditions")
    local utils                    = require("heirline.utils")
    local symbols                  = require("core.theme").symbols
    local components               = heirline_components.component

    local tokyonight_colors        = require("tokyonight.colors").setup({ style = "moon" })

    local found_vectorcode_command = vim.fn.executable("vectorcode") ~= 0


    local separator1 = { '', '' }
    local separator1_empty = { '', '' }
    local separator2 = { "", "" }
    local separator3 = { '', '' }
    local separator4_right = { '', '' }
    local separator4_right_empty = { '', '' }
    local lsp_symbol = {
        ["rust-analyzer"] = "",
        clangd = " ",
        pyright = "",
        rnix = " ",
        lua_ls = "",
        ["GitHub Copilot"] = "",
        vectorcode_server = "",
    }

    vim.api.nvim_create_autocmd("ModeChanged", {
        pattern = "*:*",
        callback = vim.schedule_wrap(function()
            vim.cmd("redrawstatus")
            vim.cmd("redrawtabline")
        end),
    })

    local colors = vim.tbl_extend(
        "force",
        heirline_components.hl.get_colors(),
        require("core.theme").colors,
        tokyonight_colors,
        {
            fg = "#ffffff",
            vim_mode = tokyonight_colors.blue,
            vim_mode_sep = tokyonight_colors.blue,
            bg2 = tokyonight_colors.fg_gutter,
        }
    )

    local fg_gutter = colors.fg_gutter
    local bg2 = colors.fg_gutter
    local ui_main = colors.ui_main

    local mode_colors = {
        n = ui_main,
        i = "green",
        v = "red",
        V = "red",
        ["\22"] = "cyan",
        c = "blue",
        s = "yellow",
        S = "yellow",
        ["\19"] = ui_main, -- telescope
        R = "orange",
        r = "orange",
        ["!"] = "red",
        t = "green1",

    }


    local function mode_color(self)
        if conditions.is_active() then
            return self.mode_colors[vim.fn.mode():sub(1, 1)]
        else
            return fg_gutter
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
            bg = fg_gutter,
        }
    end

    local function set_hl_mode(opts)
        opts.hl = mode_hl
        opts.surround = { color = mode_color }
        return opts
    end

    local function set_hl_bg2(opts)
        opts.hl = opts.hl or {}
        opts.hl.bg = opts.hl.bg or fg_gutter
        opts.surround = { color = fg_gutter }
        return opts
    end

    local Align = { provider = "%=" }
    local Space = { provider = " " }

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
            provider = symbols.close,
            on_click = {
                callback = function()
                    vim.cmd [[quit]]
                end,
                name = "close_buffer",
            }
        }
    end

    local function tabline_buffers()
        local buf_utils = require("heirline-components.buffer")

        local TablineBufnr = {
            provider = function(self)
                return tostring(self.bufnr .. ". ")
            end,
            hl = function(self)
                if self.is_active then
                    return { fg = "white", bold = true }
                else
                    return "Comment"
                end
            end,
        }
        local TablineFileName = {
            provider = function(self)
                local filename = self.filename
                filename = filename == "" and "[No Name]" or vim.fn.fnamemodify(filename, ":t")
                return filename
            end,
            hl = function(self)
                if self.is_active then
                    return { fg = "white", bold = true }
                else
                    return { fg = "white", }
                end
            end,
        }
        local FileIcon = {
            init = function(self)
                local filename = self.filename
                local extension = vim.fn.fnamemodify(filename, ":e")
                self.icon, self.icon_color = require("nvim-web-devicons").get_icon_color(filename, extension,
                    { default = true })
            end,
            provider = function(self)
                return self.icon and (self.icon .. " ")
            end,
            hl = function(self)
                if self.is_active then
                    return { fg = "white" }
                else
                    return { fg = self.icon_color }
                end
            end
        }
        local TablineFileFlags = {
            {
                condition = function(self)
                    return vim.api.nvim_get_option_value("modified", { buf = self.bufnr })
                end,
                provider = "󰆓",
                hl = { fg = "green" },
            },
            {
                condition = function(self)
                    local status_dict = vim.b[self.bufnr].gitsigns_status_dict
                    if not status_dict then
                        return false
                    end

                    local has_changes = status_dict.added ~= 0 or status_dict.removed ~= 0 or
                        status_dict.changed ~= 0
                    return has_changes
                end,
                provider = "",
                hl = function(self)
                    if self.is_active then
                        return { fg = "white" }
                    else
                        return { fg = "blue" }
                    end
                end
            },
            {
                update = { "DiagnosticChanged", "BufEnter" },
                condition = function(self)
                    local errors = #vim.diagnostic.get(self.bufnr, { severity = vim.diagnostic.severity.ERROR })
                    return errors > 0
                end,
                hl = { fg = "red" },
                provider = symbols.error,
            },
            {
                condition = function(self)
                    return not vim.api.nvim_get_option_value("modifiable", { buf = self.bufnr })
                        or vim.api.nvim_get_option_value("readonly", { buf = self.bufnr })
                end,
                provider = function(self)
                    if vim.api.nvim_get_option_value("buftype", { buf = self.bufnr }) == "terminal" then
                        return ""
                    else
                        return ""
                    end
                end,
                hl = { fg = "orange" },
            },
        }
        local TablineFileNameBlock = {
            init = function(self)
                self.filename = vim.api.nvim_buf_get_name(self.bufnr)
            end,
            hl = function(self)
                if self.is_active then
                    return { bg = self:mode_color() }
                else
                    return { bg = fg_gutter }
                end
            end,
            on_click = {
                callback = function(_, minwid, _, button)
                    if (button == "m") then -- close on mouse middle click
                        vim.schedule(function()
                            vim.api.nvim_buf_delete(minwid, { force = false })
                        end)
                    else
                        vim.api.nvim_win_set_buf(0, minwid)
                    end
                end,
                minwid = function(self)
                    return self.bufnr
                end,
                name = "heirline_tabline_buffer_callback",
            },
            -- TablineBufnr,
            FileIcon, -- turns out the version defined in #crash-course-part-ii-filename-and-friends can be reutilized as is here!
            TablineFileName,
            { provider = " " },
            TablineFileFlags,
        }

        local TablineCloseButton = {
            {
                provider = symbols.close,
                hl = { fg = "white", },
                on_click = {
                    callback = function(_, minwid)
                        vim.schedule(function()
                            vim.api.nvim_buf_delete(minwid, { force = false })
                            vim.cmd.redrawtabline()
                        end)
                    end,
                    minwid = function(self)
                        return self.bufnr
                    end,
                    name = "heirline_tabline_close_buffer_callback",
                },
            },
        }

        local TablineBufferBlock = utils.surround(separator4_right, function(self)
            if self.is_active then
                return self:mode_color()
            else
                return fg_gutter
            end
        end, {
            TablineFileNameBlock,
            TablineCloseButton,
        })

        local BufferLine = utils.make_buflist(
            TablineBufferBlock,
            { provider = "", hl = "Comment" },
            { provider = "", hl = "Comment" }
        )

        return {
            flexible = -4,
            BufferLine
        }
        -- return components.tabline_buffers()
    end

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
            flexible = opts.flexible,
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

    local function Trouble()
        return {
            provider = function()
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
                value, success = pcall(trouble_symbols.get)
                return value or ""
            end,
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

    local function git_diff(opts)
        return vim.tbl_deep_extend("keep", opts or {}, {
            condition = conditions.is_git_repo,
            init = function(self)
                self.status_dict = vim.b.gitsigns_status_dict

                self.has_changes = self.status_dict.added ~= 0 or self.status_dict.removed ~= 0 or
                    self.status_dict.changed ~= 0
            end,
            {
                provider = function(self)
                    local count = self.status_dict.added or 0
                    return count > 0 and (symbols.git.added .. count)
                end,
                hl = { fg = "green" },
            },
            {
                provider = function(self)
                    local count = self.status_dict.changed or 0
                    return count > 0 and (symbols.git.modified .. count)
                end,
                hl = { fg = "blue" },
            },
            {
                provider = function(self)
                    local count = self.status_dict.removed or 0
                    return count > 0 and (symbols.git.removed .. count)
                end,
                hl = { fg = "red" },
            },
        })
    end

    local function pwd()
        return {
            provider = function()
                local icon = "  "
                local cwd = vim.fn.getcwd(0)
                cwd = vim.fn.fnamemodify(cwd, ":~")
                if not conditions.width_percent_below(#cwd, 0.25) then
                    cwd = vim.fn.pathshorten(cwd)
                end
                return icon .. cwd
            end,
            update = "DirChanged",
            on_click = {
                name = "copy_pwd",
                callback = function()
                    vim.cmd [[let @+ = getcwd()]]
                end
            },
        }
    end

    local function file_info(opts)
        opts = opts or {}
        opts.filename = {}
        opts.filetype = false
        opts.hl = opts.hl or {}
        opts.hl.fg = "white"
        opts.on_click = {
            name = "copy_file_path",
            callback = function()
                vim.cmd [[let @+ = expand("%:p")]]
            end,
        }
        return components.file_info(opts)
    end

    local function file_relative_path(opts)
        return vim.tbl_deep_extend("keep", opts or {}, {
            provider = function()
                return vim.fn.expand("%:~:.")
            end,
            on_click = {
                name     = "copy_relative_file_path",
                callback = function()
                    vim.cmd [[let @+ = expand("%:~:.")]]
                end,
            },
            update   = "BufEnter"
        })
    end

    local function Lsp(opts)
        opts = opts or {}
        return vim.tbl_deep_extend('keep', opts, {
            condition = conditions.lsp_attached,
            update = { 'LspAttach', 'LspDetach' },
            provider = function()
                local icons = {}
                for i, server in pairs(vim.lsp.get_clients({ bufnr = 0 })) do
                    if server.name ~= "null-ls" and server.name ~= "vectorcode-server" then
                        table.insert(icons, lsp_symbol[server.name] or server.name)
                    end
                end
                return " [" .. table.concat(icons, " ") .. "]"
            end,
            hl = { fg = "green1", bold = true },
            on_click = {
                name = "heirline_lsp",
                callback = function() vim.schedule(vim.cmd.LspInfo) end,
            },
        })
    end

    local DAPMessages = {
        condition = function()
            local session = require("dap").session()
            return session ~= nil and conditions.is_active()
        end,
        provider = function()
            return " "
        end,
        hl = "Debug",
        {
            provider = " ",
            hl = "DapUIRestartNC",
            on_click = {
                callback = function()
                    require("dap").continue()
                end,
                name = "heirline_dap_continue",
            },
        },
        {
            provider = " 󰆹",
            hl = "DapUIStepIntoNC",
            on_click = {
                callback = function()
                    require("dap").step_into()
                end,
                name = "heirline_dap_step_into",
            },
        },
        {
            provider = " ",
            hl = "DapUIStepOverNC",
            on_click = {
                callback = function()
                    require("dap").step_over()
                end,
                name = "heirline_dap_step_over",
            },
        },
        {
            provider = " 󰆸",
            hl = "DapUIStepOutNC",
            on_click = {
                callback = function()
                    require("dap").step_out()
                end,
                name = "heirline_dap_step_out",
            },
        },
        {
            provider = " ",
            hl = "DapUIRestartNC",
            on_click = {
                callback = function()
                    require("dap").run_last()
                end,
                name = "heirline_dap_run_last",
            },
        },
        {
            provider = " ",
            hl = "DapUIStopNC",
            on_click = {
                callback = function()
                    require("dap").terminate()
                    require("dapui").close({})
                end,
                name = "heirline_dap_close",
            },
        },
        { 
            flexible = 1,
            {
                provider = function()
                    return " " .. require("dap").status() .. " "
                end,
                hl = "Debug",
                on_click = {
                    callback = function()
                        require("dapui").toggle()
                    end,
                    name = "heirline_dapui",
                },
            }, 
            { provider = "" },
        }
    }

    local function FileSize(opts)
        return vim.tbl_deep_extend("keep", opts or {}, {
            provider = function()
                -- stackoverflow, compute human readable file size
                local suffix = { 'B', 'K', 'M', 'G', 'T', 'P', 'E' }
                local fsize = vim.fn.getfsize(vim.api.nvim_buf_get_name(0))
                fsize = (fsize < 0 and 0) or fsize
                if fsize < 1024 then
                    return fsize .. suffix[1]
                end
                local i = math.floor((math.log(fsize) / math.log(1024)))
                return string.format("%.2f%s", fsize / math.pow(1024, i), suffix[i + 1])
            end
        })
    end

    local FileEncoding = {
        provider = function()
            local enc = (vim.bo.fenc ~= '' and vim.bo.fenc) or vim.o.enc -- :h 'enc'
            return enc:upper()
        end
    }

    local function VectorCode(opts)
        if found_vectorcode_command then
            local lualine_component = require("vectorcode.integrations").lualine({})
            return vim.tbl_deep_extend("keep", opts or {}, {
                provider = lualine_component[1],
                condition = lualine_component.condition,
            })
        else
            return {}
        end
    end


    local function Nav(opts)
        local component = components.nav(opts)
        table.insert(component[2].update, "ModeChanged")
        return component
    end

    local StatusLine = {
        surround({
            surround({
                Space,
                WindowNumber(),
            }, { separator = { "", separator2[2] } }),
            file_info(set_hl_bg2({})),
            git_diff(set_hl_bg2({})),
        }, { separator = { "", separator2[2] }, bg = bg2 }),
        DAPMessages,
        components.fill(),
        {
            components.cmd_info(),
            MacroRec(),
        },
        components.fill(),
        Lsp(),
        surround({
            components.diagnostics(set_hl_bg2({})),
            surround({
                Nav(set_hl_mode { scrollbar = false, }),
                Space,
            }, { separator = { separator2[1], "" } })
        }, { separator = { separator2[1], "" }, bg = bg2 }),
        static = {
            mode_colors = mode_colors,
            mode_color = mode_color,
        },
    }
    local WinBar = {
        init = function(self) self.bufnr = vim.api.nvim_get_current_buf() end,
        {
            Space,
            components.breadcrumbs(),
            -- Trouble(),
        },
        components.fill(),
        {
            components.cmd_info(),
        },
        surround({
            file_info(set_hl_bg2({})),
            surround({
                Space,
                WindowNumber(),
                Space,
                close_buffer(),
                Space,
            }, { separator = { separator2[1], "" } }),
        }, { separator = { separator2[1], "" }, bg = bg2 }),
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
            }, { separator = { "", separator4_right[2] } }),
            pwd(),
            Space,
            { provider = separator4_right_empty[2] },
            Space,
            file_relative_path(set_hl_bg2({})),
            components.file_info(set_hl_bg2({ file_name = false, })),
        }, { separator = { "", separator4_right[2] }, bg = bg2, }),
        components.fill(),
        tabline_buffers(),
        components.fill(),
        {
            VectorCode({ hl = { fg = "gray" } }),
            components.virtual_env(),
            FileSize({ hl = { fg = "gray" } }),
            components.file_encoding({ hl = { fg = "gray" } }),
        },
        Space,
        surround({
                Space,
                components.diagnostics(set_hl_bg2({})),
                Space,
                components.git_branch(set_hl_bg2({ hl = { fg = "orange" } })),
                git_diff(set_hl_bg2({})),
                surround({
                        quit(),
                    },
                    { separator = { separator4_right[1], "" }, }),
            },
            { separator = { separator4_right[1], "" }, bg = bg2 }),
        static = {
            mode_colors = mode_colors,
            mode_color = mode_color,
        },
    }
    local StatusColumn = {
        components.foldcolumn(),
        components.signcolumn(),
        components.numbercolumn(),
    }
    local function is_disabled(args)
        if not require("heirline-components.buffer").is_valid(args.buf)
        then
            return true
        end
        if
            conditions.buffer_matches({
                buftype = { "terminal", "prompt", "nofile", "help", "quickfix" },
                filetype = {
                    "NvimTree",
                    "neo%-tree",
                    "dashboard",
                    "Outline",
                    "aerial",
                    "trouble",
                    "codecompanion",
                    "sagaoutline",
                    "Trouble",
                    "qf",
                    "toggleterm"
                },
            }, args.buf) then
            return true
        end
        return false
    end
    heirline_components.init.subscribe_to_events()
    heirline.load_colors(colors)
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
        dir = gen.heirline,
        name = "heirline",
        package = "heirline",
        event = "LazyFile",
        dependencies = "heirline_components",
        config = config
    },
    {
        "Zeioth/heirline-components.nvim",
        dir = gen.heirline_components,
        name = "heirline_components",
        package = "heirline-components.all",
        lazy = true
    }
}
