local M = {}
M.bg = function(group, col)
    cmd("hi " .. group .. " guibg=" .. col)
end
M.fg = function(group, col)
    cmd("hi " .. group .. " guifg=" .. col)
end
M.fg_bg = function(group, fgcol, bgcol)
    cmd("hi " .. group .. " guifg=" .. fgcol .. " guibg=" .. bgcol)
end

M.colors = {
    darker_black = "#1b1f27",
    black = "#1e222a",  --  nvim bg
    black2 = "#252931",
    one_bg = "#282c34", -- real bg of onedark
    one_bg2 = "#353b45",
    one_bg3 = "#30343c",
    grey = "#42464e",
    grey_fg = "#565c64",
    grey_fg2 = "#6f737b",
    light_grey = "#6f737b",
    red = "#ff757f",
    baby_pink = "#DE8C92",
    pink = "#ff75a0",
    line = "#2a2e36", -- for lines like vertsplit
    green = "#c3e88d",
    vibrant_green = "#7eca9c",
    nord_blue = "#81A1C1",
    blue = "#61afef",
    yellow = "#e7c787",
    sun = "#EBCB8B",
    purple = "#b4bbc8",
    dark_purple = "#c882e7",
    teal = "#519ABA",
    orange = "#FF8800",
    cyan = "#86e1fc",
    violet = '#a9a1e1',
    magenta = '#c678dd',
    white = "#ffffff",
    statusline_bg = "#22262e",
    lightbg = "#2d3139",
    lightbg2 = "#262a32",
    pmenu_bg = "#A3BE8C",
    folder_bg = "#61afef",
    ui_main = "#82aaff",

    bg = '#202328',
    fg = '#bbc2cf',

    error = "#db4b4b",
    warning = "#EFB839",
    info = "#0db9d7",
    hint = "#4fd6be",
}
M.symbols = {
    error = "󰅙 ",
    warning = " ",
    warn = " ",
    info = " ",
    hint = " ",

    ui_split_left = "",
    git_branch = "",

    close = "󰖭",
    vim = "",

    checkbox_true = " ",
    checkbox_false = " ",

    file = {
        file = " ",
    },

    git = {
        added = "+",
        modified = "~",
        removed = "-",
        deleted = "✖ ",
        renamed = " ",
        untracked = "",
        ignored = " ",
        unstaged = "",
        staged = " ",
        conflict = "",
    },


    lsp = {
        Unknown = { icon = "?", hl = "" },
        Root = { icon = "󰙅", hl = "NeoTreeRootName" },
        File = { icon = "󰈙 ", hl = "Tag" },
        Module = { icon = " ", hl = "Exception" },
        Namespace = { icon = "󰌗 ", hl = "Include" },
        Package = { icon = " ", hl = "Label" },
        Class = { icon = " ", hl = "Include" },
        Method = { icon = "󰆧 ", hl = "Function" },
        Property = { icon = " ", hl = "@property" },
        Field = { icon = " ", hl = "@field" },
        Constructor = { icon = " ", hl = "@constructor" },
        Enum = { icon = "󰕘 ", hl = "@number" },
        Interface = { icon = "󰕘 ", hl = "Type" },
        Function = { icon = "󰊕 ", hl = "Function" },
        Variable = { icon = "󰆧 ", hl = "@variable" },
        Constant = { icon = "󰏿 ", hl = "Constant" },
        String = { icon = " ", hl = "String" },
        Number = { icon = "󰎠 ", hl = "Number" },
        Boolean = { icon = "◩ ", hl = "Boolean" },
        Array = { icon = "󰅪 ", hl = "Type" },
        Object = { icon = "󰅩 ", hl = "Type" },
        Key = { icon = "󰌋 ", hl = "" },
        Null = { icon = "󰟢 ", hl = "Constant" },
        EnumMember = { icon = "", hl = "Number" },
        Struct = { icon = "", hl = "Type" },
        Event = { icon = "", hl = "Constant" },
        Operator = { icon = "󰆕", hl = "Operator" },
        TypeParameter = { icon = "𝙏", hl = "Type" },

        TypeAlias = { icon = ' ', hl = 'Type' },
        Parameter = { icon = ' ', hl = '@parameter' },
        StaticMethod = { icon = '󰡱 ', hl = 'Function' },
        Macro = { icon = ' ', hl = 'Macro' },

        Text = { icon = " ", hl = "@text" },
        Unit = { icon = " ", hl = "@text" },
        Value = { icon = " ", hl = "@text" },
        Keyword = { icon = "󰌋 ", hl = "@keyword" },
        Reference = { icon = "󰌷 ", hl = "@text" },
        Folder = { icon = " ", hl = "@text" },

        icons = function(self)
            local icons = {}
            for k, v in ipairs(self) do
                if type(v) == "table" then
                    icons[k] = v.icon
                end
            end
            return icons
        end
    },
}
function M.define_sign()
    vim.fn.sign_define("DiagnosticSignError", { text = M.symbols.error, texthl = "DiagnosticSignError" })
    vim.fn.sign_define("DiagnosticSignWarn", { text = M.symbols.warning, texthl = "DiagnosticSignWarn" })
    vim.fn.sign_define("DiagnosticSignInfo", { text = M.symbols.info, texthl = "DiagnosticSignInfo" })
    vim.fn.sign_define("DiagnosticSignHint", { text = M.symbols.hint, texthl = "DiagnosticSignHint" })
end

function M.setup()
    vim.cmd [[colorscheme tokyonight-moon]]

    vim.fn.sign_define("DapBreakpoint", { text = "", texthl = "DapBreakpoint", numhl = "DapBreakpoint" })
    vim.fn.sign_define("DapBreakpointCondition", { text = "", texthl = "DapBreakpoint", numhl = "DapBreakpoint" })
    vim.fn.sign_define("DapBreakpointRejected", { text = "", texthl = "DapBreakpoint", numhl = "DapBreakpoint" })
    vim.fn.sign_define("DapLogPoint", { text = "", texthl = "DapLogPoint", numhl = "DapLogPoint" })
    vim.fn.sign_define(
        "DapStopped",
        { text = "", texthl = "DapStoppedIcon", linehl = "DapStopped", numhl = "DapStopped" }
    )
    vim.fn.sign_define("GitAdd", { text = M.symbols.git.added, texthl = "GitSignsAdd" })
    vim.fn.sign_define("GitChange", { text = M.symbols.git.modified, texthl = "GitSignsChange" })
    vim.fn.sign_define("GitDelete", { text = M.symbols.git.removed, texthl = "GitSignsDelete" })
end

return M
