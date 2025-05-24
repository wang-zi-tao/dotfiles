local lualine           = require("lualine")
local lualine_highlight = require("lualine.highlight")
-- local noice = require("noice")
local colors            = require("core.theme").colors
local symbols           = require("core.theme").symbols
local utils             = require("core.utils")

local conditions        = {
    buffer_not_empty = function()
        return vim.fn.empty(vim.fn.expand("%:t")) ~= 1
    end,
    hide_in_width = function()
        return vim.fn.winwidth(0) > 80
    end,
    check_git_workspace = function()
        local filepath = vim.fn.expand("%:p:h")
        local gitdir = vim.fn.finddir(".git", filepath .. ";")
        return gitdir and #gitdir > 0 and #gitdir < #filepath
    end,
}

local mode              = {
    -- mode component
    function()
        return "󰕷 "
    end,
    color = { fg = "#ffffff" },
    -- color = function()
    --     return { fg = mode_color[vim.fn.mode()] }
    -- end,
    on_click = function()
        vim.cmd [[WhichKey]]
    end,
}

local filesize          = {
    -- filesize component
    "filesize",
    icon = "",
    cond = conditions.buffer_not_empty,
    color = { fg = "#ffffff", gui = "bold" },
}

local filetype          = {
    "filetype",
    colored = true,             -- Displays filetype icon in color if set to true
    icon_only = true,           -- Display only an icon for filetype
    icon = { align = "right" }, -- Display filetype icon on the right hand side
    align = "right",
    -- icon =    {'X', align='right'}
    -- Icon string ^ in table is ignored in filetype component
}

local full_filename     = {
    "filename",
    symbols = {
        modified = "", -- Text to show when the file is modified.
        readonly = "[-]", -- Text to show when the file is non-modifiable or readonly.
        unnamed = "[No Name]", -- Text to show for unnamed buffers.
        newfile = " ", -- Text to show for newly created file before first write
    },
    path = 1,
    padding = { left = 0, right = 1 },
    cond = conditions.buffer_not_empty,
    color = { fg = colors.white, gui = "bold" },
}

local filename          = {
    "filename",
    symbols = {
        modified = "", -- Text to show when the file is modified.
        readonly = "[-]", -- Text to show when the file is non-modifiable or readonly.
        unnamed = "[No Name]", -- Text to show for unnamed buffers.
        newfile = " ", -- Text to show for newly created file before first write
    },
    path = 0,
    padding = { left = 0, right = 1 },
    cond = conditions.buffer_not_empty,
    color = { fg = colors.fg, gui = "bold" },
}

local branch            = {
    "b:gitsigns_head",
    icon = "",
    color = {
        fg = colors.blue,
        separator = { right = "" },
    },
}

local branch_bufferline = {
    "b:gitsigns_head",
    icon = "",
    color = {
        fg = "#ffffff",
        separator = { right = "" },
    },
}

local function diff_source()
    local gitsigns = vim.b.gitsigns_status_dict
    if gitsigns then
        return {
            added = gitsigns.added,
            modified = gitsigns.changed,
            removed = gitsigns.removed,
        }
    end
end

local diff = {
    "diff",
    icon = "",
    -- Is it me or the symbol for modified us really weird
    symbols = symbols.git,
    diff_color = {
        added = { fg = colors.green },
        modified = { fg = colors.orange },
        removed = { fg = colors.red },
    },
    source = diff_source,
    cond = conditions.hide_in_width,
    color = {
        fg = colors.blue,
        separator = { left = "" },
    },
}
local diagnostics = {
    "diagnostics",
    sources = { "nvim_diagnostic" },
    symbols = symbols,
    diagnostics_color = {
        color_error = { fg = colors.red },
        color_warn = { fg = colors.yellow },
        color_info = { fg = colors.cyan },
    },
}

local midle = {
    "%=",
    color = {
        separator = { left = "", right = "" },
    },
}

-- local message = {
--     noice.api.status.message.get_hl,
--     cond = noice.api.status.message.has,
-- }
-- local command = {
--     noice.api.status.command.get,
--     cond = noice.api.status.command.has,
--     color = { fg = colors.grey_fg2 },
-- }
-- local noice_mode = {
--     noice.api.status.mode.get,
--     cond = noice.api.status.mode.has,
--     color = { fg = colors.grey_fg2 },
-- }
-- local search = {
--     noice.api.status.search.get,
--     cond = noice.api.status.search.has,
--     color = { fg = colors.grey_fg2 },
-- }
local lsp_symbol = {
    ["rust-analyzer"] = "",
    clangd = " ",
    pyright = "",
    rnix = " ",
    lua_ls = "",
}
local lsp = {
    -- Lsp server name .
    function()
        local msg = ""
        local buf_ft = vim.api.nvim_buf_get_option(0, "filetype")
        local clients = vim.lsp.get_clients({ bufnr = 0 })
        if next(clients) == nil then
            return msg .. " No Active Lsp"
        end
        for _, client in ipairs(clients) do
            local filetypes = client.config.filetypes
            if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
                msg = msg .. " " .. (lsp_symbol[client.name] or client.name)
            end
        end
        return msg
    end,
    color = { fg = colors.fg, gui = "bold" },
}

-- Add components to right sections
local encoding = {
    "encoding",         -- option component same as &encoding in viml
    fmt = string.upper, -- I'm not sure why it's upper case either ;)
    cond = conditions.hide_in_width,
    color = { fg = colors.white, gui = "bold" },
}

local file_format = {
    "fileformat",
    symbols = {
        unix = " ", -- e712
        dos = "", -- e70f
        mac = "", -- e711
    },
    color = { fg = colors.white, gui = "bold" },
}

local location = {
    "location",
    icon = "",
    color = {
        fg = "#ffffff",
    },
}

local progress = {
    "progress",
    icon = " ",
    padding = 0,
    color = {
        fg = "#ffffff",
        gui = "bold",
    },
}

local lsp_count = {
    function()
        local ft = vim.bo.filetype
        if ft == "rust" or ft == "python" then
            return ""
        end
        local data = require("dr-lsp").lspCountTable()
        return string.format("%dD >> %dR", data.workspace.definitions, data.workspace.references)
    end,
    color = { fg = colors.blue },
}

local datatime = {
    "datetime",
    -- options: default, us, uk, iso, or your own format string ("%H:%M", etc..)
    style = "default",
}

local windows = {
    "windows",
    mdoe = 0,
    windows_color = {
        -- Same values as the general color option can be used here.
        active = "lualine_b_normal",     -- Color for active window.
        inactive = "lualine_b_inactive", -- Color for inactive window.
    },
}

local Buffer = require('lualine.components.buffers.buffer')
local origin_apply_mode = Buffer.apply_mode
function Buffer:apply_mode(name)
    if self.options.mode == 5 then
        local str = string.format(
            '%s %s%s%s',
            self.alternate_file_icon,
            self.icon,
            name,
            self.modified_icon
        )
        local prefix = lualine_highlight.component_format_highlight({
            name = "LualineBufferPrefix" }) .. "▌"
        return prefix .. str
    end
    return origin_apply_mode(self, name)
end

local origin_get_props = Buffer.get_props
function Buffer:get_props()
    self.diagnostics = ""
    return origin_get_props(self)
end

local buffers = {
    "buffers",
    mode = 5,
    use_mode_colors = true,
    buffers_color = {
        -- Same values as the general color option can be used here.
        active = "LualineBufferActive",     -- Color for active buffer.
        inactive = "LualineBufferInactive", -- Color for inactive buffer.
    },
    symbols = {
        modified = "● ", -- Text to show when the buffer is modified
        alternate_file = "", -- Text to show to identify the alternate file
        directory = "", -- Text to show when the buffer is a directory
    },
}

local tabs = {
    "tabs",
    symbols = {
        modified = "", -- Text to show when the file is modified.
    },
    tabs_color = {
        -- Same values as the general color option can be used here.
        active = "lualine_y_normal",     -- Color for active tab.
        inactive = "lualine_y_inactive", -- Color for inactive tab.
    },
    show_modified_status = true,
    use_mode_colors = true,
    fmt = function(name, context)
        -- Show + if buffer is modified in tab
        local buflist = vim.fn.tabpagebuflist(context.tabnr)
        local winnr = vim.fn.tabpagewinnr(context.tabnr)
        local bufnr = buflist[winnr]
        local mod = vim.fn.getbufvar(bufnr, "&mod")

        return name .. (mod == 1 and " +" or "")
    end,
}

local window_number = {
    function()
        return " " .. vim.api.nvim_win_get_number(0)
    end
}

local dap_state = {
    function()
        return "  " .. require("dap").status()
    end,
    cond = function()
        return package.loaded["dap"] and require("dap").status() ~= ""
    end,
    color = { fg = colors.orange },
}

local dap_state_bufferline = {
    function()
        return "  " .. require("dap").status()
    end,
    cond = function()
        return package.loaded["dap"] and require("dap").status() ~= ""
    end,
    color = { fg = "#ffffff" },
}

local trouble = require("trouble")
local trouble_symbols = trouble.statusline({
    mode = "lsp_document_symbols",
    groups = {},
    title = false,
    filter = { range = true },
    format = "❱{kind_icon}{symbol.name:Normal}",
    -- The following line is needed to fix the background color
    -- Set it to the lualine section you want to use
    hl_group = "lualine_c_normal",
})
local trouble_lsp_symbol = {
    trouble_symbols.get,
    cond = trouble_symbols.has,
}

local tabline_split_left = {
    function()
        return " "
    end
}

local split_left = {
    function()
        return ""
    end
}

local split_right = {
    function()
        return ""
    end
}
local dr_lsp = {
    require("dr-lsp").lspProgress,
    color = { fg = colors.blue },
}

local quit = {
    function()
        return symbols.close
    end,
    on_click = function()
        vim.cmd [[quitall]]
    end
}

local close_buffer = {
    function()
        return symbols.close
    end,
    on_click = function()
        vim.cmd [[quit]]
    end
}

local arrow = {
    function()
        local statusline = require('arrow.statusline')
        return statusline.text_for_statusline_with_icons(0)
    end,
}

local pwd = {
    function()
        return utils.project_dir()
    end,
    on_click = function()
        vim.cmd [[let @+ = expand('%:p')]]
    end
}

local function components_add_option(components, option)
    local r = {}
    for k, v in pairs(components) do
        r[k] = vim.tbl_extend("keep", components[k], option)
    end
    return r
end

local function line_add_option(line, left_option, right_option)
    return {
        lualine_a = components_add_option(line.lualine_a, left_option),
        lualine_b = components_add_option(line.lualine_b, left_option),
        lualine_c = components_add_option(line.lualine_c, left_option),
        lualine_x = components_add_option(line.lualine_x, right_option),
        lualine_y = components_add_option(line.lualine_y, right_option),
        lualine_z = components_add_option(line.lualine_z, right_option),
    }
end

lualine.setup({
    options = {
        section_separators = { left = "", right = "" },
        component_separators = { left = "", right = "" },
        -- component_separators = { left = '', right = '' },
        theme = 'tokyonight',
        disabled_filetypes = {
            "NvimTree",
            "Outline",
            "lspsagaoutline",
            "dapui_scopes",
            "dapui_watches",
            "dapui_stacks",
            "dapui_breakpoints",
            "sagaoutline",
            "neo-tree",
            "trouble",
            "qf",
            "array",
        },
        refresh = {
            statusline = 1000,
            tabline = 1000,
            winbar = 1000,
        }
    },
    sections = {
        lualine_a = { mode },
        lualine_b = { filetype, filename },
        lualine_c = {
            diff,
        },
        lualine_x = {
            -- location,
            -- progress,
            -- filesize,
            dap_state,
            split_right,
            diagnostics,
        },
        lualine_y = { lsp, location, progress, },
        lualine_z = { filesize, encoding, file_format, },
    },
    inactive_sections = {
        -- these are to remove the defaults
        lualine_a = { window_number },
        lualine_b = { filetype, filename },
        lualine_c = {
            diff,
        },
        lualine_x = {
            diagnostics,
        },
        lualine_y = { lsp, location, progress, },
        lualine_z = { filesize, encoding, file_format, },
    },
    winbar = line_add_option({
        lualine_a = {
        },
        lualine_b = {
        },
        lualine_c = {
            trouble_lsp_symbol,
        },
        lualine_x = {
            lsp_count,
            { "searchcount" },
            { "selectioncount" },
        },
        lualine_y = {
            vim.tbl_extend("keep", filetype, { colored = false }),
            vim.tbl_extend("keep", filename, { color = { fg = "#ffffff" } }),
            arrow,
        },
        lualine_z = {
            window_number,
            close_buffer,
        },
    }, {
        separator = { left = '', right = '' }
    }, {
        separator = { left = '', right = '' }
    }),
    inactive_winbar = {
        lualine_a = {
        },
        lualine_b = {
        },
        lualine_c = {
        },
        lualine_y = {
            vim.tbl_extend("keep", filetype, { colored = false }),
            vim.tbl_extend("keep", filename, {}),
            arrow,
        },
        lualine_z = {
            window_number,
            close_buffer,
        },
    },
    extensions = {
        "quickfix",
        "nvim-tree",
        -- "symbols-outline",
        "nvim-dap-ui",
        "neo-tree",
        "toggleterm",
        "nvim-dap-ui",
        "lazy",
    },
    tabline = line_add_option({
        lualine_a = {
            mode,
        },
        lualine_b = {
            pwd,
            tabline_split_left,
            full_filename,
            -- windows,
        },
        lualine_c = {
            -- buffers,
        },
        lualine_x = {
        },
        lualine_y = {
            dap_state_bufferline,
            diff,
            branch_bufferline,
            -- tabs,
            diagnostics, lsp,
        },
        lualine_z = {
            close_buffer,
        },
    }, {
        separator = { left = '', right = ' ' }
    }, {
        separator = { left = ' ', right = '' }
    }),
})
