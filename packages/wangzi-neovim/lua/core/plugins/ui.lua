return {
    {
        "navarasu/onedark.nvim",
        dir = gen.onedark_nvim,
        name = "onedark_nvim",
        config = function()
            require("core.plugins.onedark")
        end,
    },
    {
        "folke/which-key.nvim",
        dir = gen.which_key,
        name = "which_key",
        module = "which-key",
        lazy = true,
        event = "VeryLazy",
        config = function()
            require("which-key").setup({})
        end,
    },
    {
        "rcarriga/nvim-notify",
        dir = gen.notify_nvim,
        name = "notify_nvim",
        module = "notify",
        event = "VeryLazy",
        config = function()

        end,
    },
    {
        "kyazdani42/nvim-web-devicons",
        dir = gen.nvim_web_devicons,
        name = "nvim_web_devicons",
        lazy = true,
        config = function()
            require("core.plugins.icons")
        end,
        event = "VeryLazy",
        build = ":TSUpdate",
    },
    {
        "goolord/alpha-nvim",
        dir = gen.alpha_nvim,
        name = "alpha_nvim",
        config = function()
            require("core.plugins.alpha")
        end,
    },
    {
        "feline-nvim/feline.nvim",
        dir = gen.feline_nvim,
        name = "feline_nvim",
        dependencies = { "onedark_nvim" },
        config = function()
            require("core.plugins.statusline")
        end,
    },
    {
        "akinsho/bufferline.nvim",
        dir = gen.bufferline_nvim,
        name = "bufferline_nvim",
        dependencies = { "nvim_web_devicons", "onedark_nvim" },
        module = "bufferline",
        lazy = true,
        event = "BufReadPost",
        config = function()
            require("core.plugins.bufferline")
        end,
        keys = {
            { "<Tab>", function() require("bufferline").cycle(1) end, mode = "n", desc = "next tab" },
            { "<S-Tab>", function() require("bufferline").cycle(-1) end, mode = "n", desc = "prev tab" },
        }
    },
    {
        "lukas-reineke/indent-blankline.nvim",
        dir = gen.indent_blankline_nvim,
        name = "indent_blankline_nvim",
        event = "VeryLazy",
        lazy = true,
        config = function()
            require("indent_blankline").setup({
                indentLine_enabled = 1,
                char = "▏",
                filetype_exclude = {
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
                buftype_exclude = { "terminal" },
                show_trailing_blankline_indent = false,
                show_first_indent_level = false,
            })
        end,
    },
    {
        "norcalli/nvim-colorizer.lua",
        dir = gen.nvim_colorizer_lua,
        name = "nvim_colorizer_lua",
        event = "VeryLazy",
        lazy = true,
        config = function()
            require("colorizer").setup({
            }, {
                RGB = true, -- #RGB hex codes
                RRGGBB = true, -- #RRGGBB hex codes
                names = true, -- "Name" codes like Blue
                RRGGBBAA = true, -- #RRGGBBAA hex codes
                rgb_fn = true, -- CSS rgb() and rgba() functions
                hsl_fn = true, -- CSS hsl() and hsla() functions
                css = true, -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
                css_fn = true, -- Enable all CSS *functions*: rgb_fn, hsl_fn

                -- Available modes: foreground, background
                mode = "background", -- Set the display mode.
            })
            -- vim.cmd("ColorizerReloadAllBuffers")
        end,
    },
    {
        "m00qek/baleia.nvim",
        dir = gen.baleia_nvim,
        tag = "v1.2.0",
        name = "baleia_nvim",
        event = "VeryLazy",
        cmd = "BaleiaColorize",
        lazy = true,
        module = "baleia",
        config = function()
            vim.cmd [[
                command! BaleiaColorize call luaeval("require('baleia').setup { }").once(bufnr('%'))

                autocmd BufWinEnter my-buffer call luaeval("require('baleia').setup { }").automatically(bufnr('%'))
                autocmd BufWinEnter *.log call luaeval("require('baleia').setup { }").automatically(bufnr('%'))
            ]]
        end,
    },
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
}
