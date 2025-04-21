return {
    {
        "numToStr/FTerm.nvim",
        dir = gen.fterm,
        name = "fterm",
        module = "FTerm",
        lazy = true,
        disable = true,
        config = function()
            require("FTerm").setup({
                border = "rounded",
                dimensions = {
                    height = 0.9,
                    width = 0.9,
                },
            })
        end,
        -- keys = {
        --     { "\\'", function() require("FTerm").toggle() end, mode = "n", desc = "Float Terminal" },
        --     {
        --         "<C-\\>",
        --         function()
        --             if (1 == vim.fn.has("win32")) then
        --                 vim.cmd [[Lspsaga term_toggle]]
        --             else
        --                 require("FTerm").toggle()
        --             end
        --         end,
        --         mode = { "n", "t" },
        --         { "n", "t" },
        --         desc = "Float Terminal"
        --     }
        -- }
    },
    {
        "akinsho/toggleterm.nvim",
        name = "toggleterm",
        dir = gen.toggleterm_nvim,
        config = function()
            vim.api.nvim_create_autocmd({ "TermOpen" }, {
                callback = function(args)
                    local opts = { buffer = args.buf }
                    vim.keymap.set('t', '<C-\\>', [[<cmd>q<CR>]], opts)
                end,
            })
            local shell = vim.o.shell
            if 1 == vim.fn.has("win32") and vim.fn.executable("nu") == 1 then
                shell = "nu"
            end
            local toggleterm_nvim = require("core.utils").toggleterm_nvim
            require("toggleterm").setup({
                direction = "float",
                shell = shell,
                float_opts = {
                    -- The border key is *almost* the same as 'nvim_open_win'
                    -- see :h nvim_open_win for details on borders however
                    -- the 'curved' border is a custom border type
                    -- not natively supported but implemented in this plugin.
                    border = "rounded",
                },
                highlights = {
                    -- highlights which map to a highlight group name and a table of it's values
                    -- NOTE: this is only a subset of values, any group placed here will be set for the terminal window split
                    NormalFloat = {
                        link = "Normal",
                    },
                    FloatBorder = {
                        link = "TerminalBorder",
                    },
                },
            })
            local Terminal = require("toggleterm.terminal").Terminal
            toggleterm_nvim.gitui = Terminal:new({ cmd = "lazygit", hidden = true })
            toggleterm_nvim.rg = Terminal:new({ cmd = "nu", hidden = true })
            for i = 0, 9 do
                toggleterm_nvim[i] = Terminal:new({ display_name = "terminal " .. i, cmd = "nu", hidden = true })
            end
        end,
        cmd = { "ToggleTerm" },
        init = function()
            require("which-key").add({
                { "<leader>t", group = "TrailBlazer / Terminal" },
            })
        end,
        module = "toggleterm",
        keys = {
            {
                "\\'",
                function()
                    vim.cmd.ToggleTerm()
                end,
                mode = { "n", "i", "v", "t" },
                desc = "Float Terminal",
            },
            {
                "<C-\\>",
                function()
                    vim.cmd.ToggleTerm()
                end,
                mode = { "n", "i", "v", "t" },
                desc = "Float Terminal",
            },
            {
                "<leader>tg",
                function()
                    require("core.utils"):toggle_term("gitui")
                end,
                desc = "GitUI",
            },
            {
                "<leader>tw",
                function()
                    require("core.utils"):toggle_term("rg")
                end,
                desc = "rg",
            },
            { "<leader>tf", "<cmd>ToggleTerm direction=float<CR>",      desc = "Terminal float" },
            { "<leader>tb", "<cmd>ToggleTerm direction=tab<CR>",        desc = "Terminal tab" },
            { "<leader>th", "<cmd>ToggleTerm direction=horizontal<CR>", desc = "Terminal horizontal" },
            { "<leader>tv", "<cmd>ToggleTerm direction=vertical<CR>",   desc = "Terminal vertical" },
            {
                "<leader>t1",
                function()
                    require("core.utils").toggle_term(1)
                end,
                desc = "Terminal 1",
            },
            {
                "<leader>t2",
                function()
                    require("core.utils").toggle_term(2)
                end,
                desc = "Terminal 2",
            },
            {
                "<leader>t3",
                function()
                    require("core.utils").toggle_term(3)
                end,
                desc = "Terminal 3",
            },
            {
                "<leader>t4",
                function()
                    require("core.utils").toggle_term(4)
                end,
                desc = "Terminal 4",
            },
            {
                "<leader>t5",
                function()
                    require("core.utils").toggle_term(5)
                end,
                desc = "Terminal 5",
            },
            {
                "<leader>t6",
                function()
                    require("core.utils").toggle_term(6)
                end,
                desc = "Terminal 6",
            },
            {
                "<leader>t7",
                function()
                    require("core.utils").toggle_term(7)
                end,
                desc = "Terminal 7",
            },
            {
                "<leader>t8",
                function()
                    require("core.utils").toggle_term(8)
                end,
                desc = "Terminal 8",
            },
            {
                "<leader>t9",
                function()
                    require("core.utils").toggle_term(9)
                end,
                desc = "Terminal 9",
            },
            {
                "<leader>t0",
                function()
                    require("core.utils").toggle_term(0)
                end,
                desc = "Terminal 0",
            },
        },
    },
}
