local onedark = require("onedark")
onedark.setup({
    -- Main options --
    style = "deep",                                                                    -- Default theme style. Choose between 'dark', 'darker', 'cool', 'deep', 'warm', 'warmer' and 'light'
    transparent = false,                                                               -- Show/hide background
    term_colors = false,                                                               -- Change terminal color as per the selected theme style
    ending_tildes = false,                                                             -- Show the end-of-buffer tildes. By default they are hidden
    -- toggle theme style ---
    toggle_style_key = "<leader>wT",                                                   -- Default keybinding to toggle
    toggle_style_list = { "dark", "light", "darker", "cool", "deep", "warm", "warmer" }, -- List of styles to toggle between

    -- Change code style ---
    -- Options are italic, bold, underline, none
    -- You can configure multiple style with comma seperated, For e.g., keywords = 'italic,bold'
    code_style = {
        comments = "italic",
        keywords = "none",
        functions = "none",
        strings = "none",
        variables = "none",
    },

    -- Custom Highlights --
    colors = {}, -- Override default colors
    highlights = {
        HoverBorder = { fg = "$blue", bg = "$bg0" },
        SageBorder = { fg = "$blue", bg = "$bg0" },
        FloatBorder = { fg = "$blue", bg = "$bg0" },
        RenameBorder = { fg = "$blue", bg = "$bg0" },
        DiagnosticBorder = { fg = "$blue", bg = "$bg0" },
        TerminalBorder = { fg = "$blue", bg = "$bg0" },
        NormalBorder = { fg = "$blue", bg = "$bg0" },
        VertSplit = { fg = "$blue", bg = "$bg0" },
        TelescopeBorder = { fg = "$bg1", bg = "$bg1" },
        TelescopePromptBorder = { fg = "$bg2", bg = "$bg2" },
        TelescopeResultsBorder = { fg = "$bg1", bg = "$bg1" },
        TelescopePreviewBorder = { fg = "$bg1", bg = "$bg1" },
        TelescopePromptNormal = { fg = "$fg", bg = "$bg2" },
        TelescopePreviewNormal = { fg = "$fg", bg = "$bg1" },
        TelescopeResultsNormal = { fg = "$fg", bg = "$bg1" },
        TelescopePromptPrefix = { fg = "$blue" },
        TelescopeNormal = { bg = "$bg0" },
        TelescopePreviewTitle = { fg = "$black", bg = "$blue" },
        TelescopePromptTitle = { fg = "$black", bg = "$blue" },
        TelescopeResultsTitle = { fg = "$bg1", bg = "$bg1" },
        TelescopeSelection = { fg = "$fg", bg = "$bg0" },
        TelescopeResultsDiffAdd = { fg = "$green" },
        TelescopeResultsDiffChange = { fg = "$yellow" },
        TelescopeResultsDiffDelete = { fg = "$red" },
        NotifyERRORBorder = { fg = "$red" },
        NotifyWARNBorder = { fg = "$yellow" },
        NotifyINFOBorder = { fg = "$green" },
        NotifyDEBUGBorder = { fg = "$cyan" },
        NotifyTRACEBorder = { fg = "$grey" },
        NotifyERRORIcon = { fg = "$red" },
        NotifyWARNIcon = { fg = "$yellow" },
        NotifyINFOIcon = { fg = "$green" },
        NotifyDEBUGIcon = { fg = "$grey" },
        NotifyTRACEIcon = { fg = "$purple" },
        NotifyERRORTitle = { fg = "$red" },
        NotifyWARNTitle = { fg = "$yellow" },
        NotifyINFOTitle = { fg = "$green" },
        NotifyDEBUGTitle = { fg = "$grey" },
        NotifyTRACETitle = { fg = "$purple" },
        NotifyBackground = { bg = "$bg0" },
        WhichKey = { fg = "$cyan" },
        NeoTreeTabInactive = { bg = "$black" },
        NeoTreeTabSeparatorActive = { fg = "$blue", bg = "$bg1" },
        NeoTreeTabSeparatorInactive = { fg = "$black", bg = "$black" },
        NeoTreeGitUnstaged = { bg = "$bg0", fg = "$red" },
        NeoTreeGitUntracked = { bg = "$bg0", fg = "$red" },
        ScrollbarSearch = { fg = "$yellow" },
        DiagnosticVirtualTextWarn = { fg = "$orange" },
        DiagnosticVirtualTextInfo = { fg = "$blue" },
        DiagnosticVirtualTextHint = { fg = "$green" },
        DiagnosticVirtualTextError = { fg = "$red" },
    }, -- Override highlight groups

    -- Plugins Config --
    diagnostics = {
        darker = true,  -- darker colors for diagnostic
        undercurl = true, -- use undercurl instead of underline for diagnostics
        background = false, -- use background color for virtual text
    },
})
onedark.load()
