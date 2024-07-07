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
            global.toggleterm_nvim = {
                gitui = Terminal:new({ cmd = "lazygit", hidden = true }),
                rg = Terminal:new({ cmd = "nu", hidden = true }),
            }
            for i = 0, 9 do
                global.toggleterm_nvim[i] = Terminal:new({ cmd = "nu", hidden = true })
            end
        end,
        cmd = { "ToggleTerm" },
        init = function()
            require("which-key").register({
                t = { name = "TrailBlazer / Terminal" },
            }, { prefix = "<leader>" })
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
                    global.toggleterm_nvim.gitui:toggle()
                end,
                desc = "GitUI",
            },
            {
                "<leader>tw",
                function()
                    global.toggleterm_nvim.rg:toggle()
                end,
                desc = "rg",
            },
            { "<leader>tf", ":ToggleTerm direction=float<CR>",      desc = "Terminal float" },
            { "<leader>tb", ":ToggleTerm direction=tab<CR>",        desc = "Terminal tab" },
            { "<leader>th", ":ToggleTerm direction=horizontal<CR>", desc = "Terminal horizontal" },
            { "<leader>tv", ":ToggleTerm direction=vertical<CR>",   desc = "Terminal vertical" },
            {
                "<leader>t1",
                function()
                    global:toggle_term(1)
                end,
                desc = "Terminal 1",
            },
            {
                "<leader>t2",
                function()
                    global:toggle_term(2)
                end,
                desc = "Terminal 2",
            },
            {
                "<leader>t3",
                function()
                    global:toggle_term(3)
                end,
                desc = "Terminal 3",
            },
            {
                "<leader>t4",
                function()
                    global:toggle_term(4)
                end,
                desc = "Terminal 4",
            },
            {
                "<leader>t5",
                function()
                    global:toggle_term(5)
                end,
                desc = "Terminal 5",
            },
            {
                "<leader>t6",
                function()
                    global:toggle_term(6)
                end,
                desc = "Terminal 6",
            },
            {
                "<leader>t7",
                function()
                    global:toggle_term(7)
                end,
                desc = "Terminal 7",
            },
            {
                "<leader>t8",
                function()
                    global:toggle_term(8)
                end,
                desc = "Terminal 8",
            },
            {
                "<leader>t9",
                function()
                    global:toggle_term(9)
                end,
                desc = "Terminal 9",
            },
            {
                "<leader>t0",
                function()
                    global:toggle_term(0)
                end,
                desc = "Terminal 0",
            },
        },
    },
}
