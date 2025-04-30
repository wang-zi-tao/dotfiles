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
        symbols = require("core.theme").symbols.lsp,
    }
    -- require("symbols-outline").setup(opt)
    local colors = require("core.theme").colors
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
