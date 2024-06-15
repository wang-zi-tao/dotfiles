local function config()
    local opt = {
        highlight_hovered_item = true,
        show_guides = true,
        auto_preview = true,
        position = "right",
        relative_width = false,
        width = 25,
        auto_close = false,
        show_numbers = false,
        show_relative_numbers = true,
        show_symbol_details = true,
        preview_bg_highlight = "Pmenu",
        keymaps = { -- These keymaps can be a string or a table for multiple keys
            close = { "<Esc>", "q" },
            goto_location = "<Cr>",
            focus_location = "o",
            hover_symbol = "<C-space>",
            toggle_preview = "K",
            rename_symbol = "r",
            code_actions = "a",
        },
        lsp_blacklist = {},
        symbol_blacklist = {},
        symbols = {
            File = { icon = "", hl = "SymbolOutlineURI" },
            Module = { icon = "", hl = "SymbolOutlineNamespace" },
            Namespace = { icon = "", hl = "SymbolOutlineNamespace" },
            Package = { icon = "", hl = "SymbolOutlineNamespace" },
            Class = { icon = "", hl = "SymbolOutlineType" },
            Method = { icon = "", hl = "SymbolOutlineMethod" },
            Property = { icon = "", hl = "SymbolOutlineMethod" },
            Field = { icon = "", hl = "SymbolOutlineField" },
            Constructor = { icon = "", hl = "SymbolOutlineConstructor" },
            Enum = { icon = "", hl = "SymbolOutlineType" },
            Interface = { icon = "", hl = "SymbolOutlineType" },
            Function = { icon = "", hl = "SymbolOutlineFunction" },
            Variable = { icon = "", hl = "SymbolOutlineConstant" },
            Constant = { icon = "", hl = "SymbolOutlineConstant" },
            String = { icon = "", hl = "SymbolOutlineString" },
            Number = { icon = "#", hl = "SymbolOutlineNumber" },
            Boolean = { icon = "", hl = "SymbolOutlineBoolean" },
            Array = { icon = "", hl = "SymbolOutlineConstant" },
            Object = { icon = "", hl = "SymbolOutlineType" },
            Key = { icon = "", hl = "SymbolOutlineType" },
            Null = { icon = "ﳠ", hl = "SymbolOutlineType" },
            EnumMember = { icon = "", hl = "SymbolOutlineField" },
            Struct = { icon = "", hl = "SymbolOutlineType" },
            Event = { icon = "", hl = "SymbolOutlineType" },
            Operator = { icon = "+", hl = "SymbolOutlineOperator" },
            TypeParameter = { icon = "𝙏", hl = "SymbolOutlineParameter" },
        },
    }
    require("symbols-outline").setup(opt)
    local colors = require("core.colors").get()
    local blue = colors.blue
    for k, v in pairs(opt.symbols) do
        vim.cmd("hi " .. v.hl .. " guifg=" .. blue)
    end
end

return {
    "simrat39/symbols-outline.nvim",
    dir = gen.symbols_outline,
    name = "symbols_outline",
    module = "symbols-outline",
    cmd = {
        "SymbolsOutline",
        "SymbolsOutlineOpen",
        "SymbolsOutlineClose",
    },
    lazy = true,
    config = config,
    keys = {
        {
            "<leader>o",
            function()
                require("symbols-outline").toggle_outline()
            end,
            desc = "Outline",
        },
    },
}
