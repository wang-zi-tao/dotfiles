return {
    {
        "folke/tokyonight.nvim",
        name = "tokyonight",
        dir = gen.tokyonight_nvim,
        module = "tokyonight",
        priority = 90,
        config = function()
            require("tokyonight").setup({
                -- your configuration comes here
                -- or leave it empty to use the default settings
                style = "moon",         -- The theme comes in three styles, `storm`, `moon`, a darker variant `night` and `day`
                light_style = "day",    -- The theme is used when the background is set to light
                transparent = false,    -- Enable this to disable setting the background color
                terminal_colors = true, -- Configure the colors used when opening a `:terminal` in [Neovim](https://github.com/neovim/neovim)
                styles = {
                    -- Style to be applied to different syntax groups
                    -- Value is any valid attr-list value for `:help nvim_set_hl`
                    comments = { italic = true },
                    keywords = { italic = true },
                    functions = {},
                    variables = {},
                    -- Background styles. Can be "dark", "transparent" or "normal"
                    sidebars = "dark",            -- style for sidebars, see below
                    floats = "dark",              -- style for floating windows
                },
                sidebars = { "qf", "help" },      -- Set a darker background on sidebar-like windows. For example: `["qf", "vista_kind", "terminal", "packer"]`
                day_brightness = 0.3,             -- Adjusts the brightness of the colors of the **Day** style. Number between 0 and 1, from dull to vibrant colors
                hide_inactive_statusline = false, -- Enabling this option, will hide inactive statuslines and replace them with a thin border instead. Should work with the standard **StatusLine** and **LuaLine**.
                dim_inactive = false,             -- dims inactive windows
                lualine_bold = false,             -- When `true`, section headers in the lualine theme will be bold

                --- You can override specific color groups to use other groups or a hex color
                --- function will be called with a ColorScheme table
                ---@param colors ColorScheme
                on_colors = function(colors) end,

                --- You can override specific highlights to use other groups or a hex color
                --- function will be called with a Highlights and ColorScheme table
                on_highlights = function(h, c)
                    local bg = ""
                    h.HoverBorder = { fg = c.blue, bg = c.bg_dark }
                    h.SageBorder = { fg = c.blue, bg = c.bg_dark }
                    h.FloatBorder = { fg = c.blue, bg = c.bg_dark }
                    h.RenameBorder = { fg = c.blue, bg = c.bg_dark }
                    h.DiagnosticBorder = { fg = c.blue, bg = c.bg_dark }
                    h.TerminalBorder = { fg = c.blue, bg = c.bg_dark }
                    h.NormalBorder = { fg = c.blue, bg = c.bg_dark }
                    h.VertSplit = { fg = c.blue, bg = c.bg_dark }
                    h.NotifyERRORBorder = { fg = c.red }
                    h.NotifyWARNBorder = { fg = c.yellow }
                    h.NotifyINFOBorder = { fg = c.green }
                    h.NotifyDEBUGBorder = { fg = c.cyan }
                    h.NotifyTRACEBorder = { fg = c.grey }
                    h.NotifyERRORIcon = { fg = c.red }
                    h.NotifyWARNIcon = { fg = c.yellow }
                    h.NotifyINFOIcon = { fg = c.green }
                    h.NotifyDEBUGIcon = { fg = c.grey }
                    h.NotifyTRACEIcon = { fg = c.purple }
                    h.NotifyERRORTitle = { fg = c.red }
                    h.NotifyWARNTitle = { fg = c.yellow }
                    h.NotifyINFOTitle = { fg = c.green }
                    h.NotifyDEBUGTitle = { fg = c.grey }
                    h.NotifyTRACETitle = { fg = c.purple }
                    h.NotifyBackground = { bg = c.bg_dark }
                    h.WhichKey = { fg = c.cyan }
                    h.NeoTreeTabInactive = { bg = c.black }
                    h.NeoTreeTabSeparatorActive = { fg = c.blue, bg = c.bg }
                    h.NeoTreeTabSeparatorInactive = { fg = c.black, bg = c.black }
                    h.NeoTreeGitUnstaged = { bg = c.bg_dark, fg = c.red }
                    h.NeoTreeGitUntracked = { bg = c.bg_dark, fg = c.red }
                    h.ScrollbarSearch = { fg = c.yellow }
                    h.DiagnosticVirtualTextWarn = { fg = c.orange }
                    h.DiagnosticVirtualTextInfo = { fg = c.blue }
                    h.DiagnosticVirtualTextHint = { fg = c.green }
                    h.DiagnosticVirtualTextError = { fg = c.red }
                    h.DapUIPlayPauseNC = { bg = c.bg_dark, fg = c.green }
                    h.DapUIStopNC = { bg = c.bg_dark, fg = "#f70067" }
                    h.DapUIRestartNC = { bg = c.bg_dark, fg = c.green }
                    h.DapUIUnavailableNC = { bg = c.bg_dark, fg = "#424242" }
                    h.DapUIStepOverNC = { bg = c.bg_dark, fg = c.cyan }
                    h.DapUIStepIntoNC = { bg = c.bg_dark, fg = c.cyan }
                    h.DapUIStepBackNC = { bg = c.bg_dark, fg = c.cyan }
                    h.DapUIStepOutNC = { bg = c.bg_dark, fg = c.cyan }
                    h.DapBreakpoint = { fg = c.red }
                    h.DapLogPoint = { fg = c.orange }
                    h.DapStopped = { bg = c.green, fg = c.black }
                    h.DapStoppedIcon = { fg = c.cyan }
                    h.BufferLineBufferSelected = { bg = c.bg_dark, fg = "#ffffff" }
                    h.BufferLineDuplicate = { bg = "#131820", fg = c.grey }
                    h.BufferLineDuplicateVisible = { bg = c.bg_dark, fg = c.grey }
                    h.BufferLineDuplicateSelected = { bg = c.bg_dark, fg = c.grey }
                    h.BufferLineHintSelected = { bg = c.bg_dark, fg = c.cyan }
                    h.BufferLineInfoSelected = { bg = c.bg_dark, fg = c.green }
                    h.BufferLineWarningSelected = { bg = c.bg_dark, fg = c.orange }
                    h.BufferLineErrorSelected = { bg = c.bg_dark, fg = c.red }
                    h.BufferLineHintDiagnosticSelected = { bg = c.bg_dark, fg = c.cyan }
                    h.BufferLineInfoDiagnosticSelected = { bg = c.bg_dark, fg = c.green }
                    h.BufferLineWarningDiagnosticSelected = { bg = c.bg_dark, fg = c.orange }
                    h.BufferLineErrorDiagnosticSelected = { bg = c.bg_dark, fg = c.red }
                    h.SagaWinbarFileName = { bg = c.bg_dark, fg = c.grey }
                    h.SagaWinbarFolderName = { bg = c.bg_dark, fg = c.grey }
                    h.EdgyWinBar = { bg = c.bg_dark, fg = c.fg }
                    h.WinBar = { bg = c.bg_dark, fg = c.fg }
                    h.WinBarNC = { bg = c.bg_dark, fg = c.fg }

                    h.GitSignsAdd = { fg = "#8bcd5b" }
                    h.GitSignsChange = { fg = "#41a7fc" }
                    h.GitSignsDeletexxx = { fg = "#f65866" }

                    h.RainbowRed = { fg = c.red }
                    h.RainbowYellow = { fg = c.yellow }
                    h.RainbowBlue = { fg = c.blue }
                    h.RainbowOrange = { fg = c.orange }
                    h.RainbowGreen = { fg = c.green }
                    h.RainbowViolet = { fg = c.purple }
                    h.RainbowCyan = { fg = c.cyan }

                    h.LualineBufferPrefix = { fg = c.blue }
                    h.LualineBufferInactive = { fg = c.grey }
                    h.LualineBufferActive = { fg = c.blue }
                end,
                cache = true,
                plugins = {
                    auto = false,
                    all = true,
                    rainbow = true,
                    telescope = true,
                    dap = false,
                    diffview = false,
                },
            })
        end
    }
}
