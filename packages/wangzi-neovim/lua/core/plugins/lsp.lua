local symbols = require("core.theme").symbols

local function setup_lspsaga()
    require("lspsaga").setup({
        outline = {
            win_position = "right",
            win_with = "",
            win_width = 30,
            show_detail = true,
            auto_preview = true,
            auto_refresh = true,
            auto_close = true,
            custom_sort = nil,
            keys = {
                jump = "<CR>",
                expand_collapse = "u",
                quit = "q",
            },
        },
        finder = {
            --percentage
            max_height = 1.0,
            force_max_height = false,
            keys = {
                jump_to = "l",
                edit = { "o", "<CR>" },
                vsplit = "s",
                split = "i",
                tabe = "t",
                tabnew = "r",
                quit = { "q", "<ESC>" },
                close_in_preview = "<ESC>",
            },
        },
        ui = {
            -- This option only works in Neovim 0.9
            title = true,
            -- Border type can be single, double, rounded, solid, shadow.
            border = "rounded",
            winblend = 0,
            expand = "",
            collapse = "",
            code_action = "",
            incoming = "󰋺 ",
            outgoing = "󰈇 ",
            hover = " ",
            kind = {},
            button = { "", "" },
            imp_sign = " ",
        },
        floaterm = {
            height = 0.9,
            width = 0.9,
        },
    })
    vim.api.nvim_create_autocmd({ "VimResized" }, {
        pattern = "lspsagaoutline",
        callback = function(args)
            vim.notify("resize")
            local alltabpages = vim.api.nvim_list_tabpages()
            local window = nil
            for _, tabpage in ipairs(alltabpages) do
                local winlist = vim.api.nvim_tabpage_list_wins(tabpage)
                for _, win in ipairs(winlist) do
                    local buf = vim.api.nvim_win_get_buf(win)
                    if buf == args.buf then
                        window = win
                        vim.api.nvim_set_current_win(win)
                        break
                    end
                end
                if window then
                    break
                end
            end
            local width = vim.api.nvim_win_get_width(window)
            if width > 32 then
                vim.api.nvim_win_set_width(32)
            end
        end,
    })
end

local function lsp_signature_on_attach(bufnr)
    require("lsp_signature").on_attach({
        bind = true,
        doc_lines = 0,
        floating_window = true,
        fix_pos = true,
        hint_enable = true,
        hint_prefix = symbols.hint,
        hint_scheme = "String",
        hi_parameter = "Search",
        max_height = 22,
        max_width = 120,        -- max_width of signature floating_window, line will be wrapped if exceed max_width
        handler_opts = {
            border = "rounded", -- double, single, shadow, none
        },
        zindex = 200,           -- by default it will be on top of all floating windows, set to 50 send it to bottom
        padding = "",           -- character to pad on left and right of signature can be ' ', or '|'  etc
    }, bufnr)
end

return {
    lsp_signature_on_attach = lsp_signature_on_attach,
    {
        "ray-x/lsp_signature.nvim",
        dir = gen.lsp_signature_nvim,
        name = "lsp_signature_nvim",
        event = "LspAttach",
        config = function()
        end,
    },
    {
        "glepnir/lspsaga.nvim",
        dir = gen.lspsaga,
        name = "lspsaga",
        event = "LspAttach",
        dependencies = { "nvim_web_devicons" },
        config = setup_lspsaga,
        cmd = "Lspsaga",
        module = "lspsaga",
        keys = {
            { "[d", "<cmd>Lspsaga diagnostic_jump_prev<CR>", mode = "n", desc = "Previous Diagnostic" },
            { "]d", "<cmd>Lspsaga diagnostic_jump_next<CR>", mode = "n", desc = "Next Diagnostic" },
            {
                "[e",
                function()
                    require("lspsaga.diagnostic"):goto_prev({ severity = vim.diagnostic.severity.ERROR })
                end,
                mode = "n",
                desc = "Previous Error",
            },
            {
                "]e",
                function()
                    require("lspsaga.diagnostic"):goto_next({ severity = vim.diagnostic.severity.ERROR })
                end,
                mode = "n",
                desc = "Next Error",
            },
            {
                "ge",
                function()
                    require("lspsaga.diagnostic"):goto_next({ severity = vim.diagnostic.severity.ERROR })
                end,
                mode = "n",
                desc = "Diagnostic",
            },
            { "gd", "<cmd>Lspsaga peek_definition<CR>",      mode = "n", desc = "Peek Definition" },
            {
                "gD",
                function()
                    require("trailblazer").new_trail_mark()
                    vim.cmd([[Lspsaga goto_definition]])
                end,
                mode = "n",
                desc = "Goto Definition",
            },
            { "gr", "<cmd>Lspsaga rename<CR>",               mode = "n", desc = "Rename" },
            { "gt", "<cmd>Lspsaga peek_type_definition<CR>", mode = "n", desc = "Peek Type Definition" },
            { "gT", "<cmd>Lspsaga goto_type_definition<CR>", mode = "n", desc = "Goto Type Definition" },
            {
                "gh",
                function()
                    require("trailblazer").new_trail_mark()
                    vim.cmd([[Lspsaga finder]])
                end,
                mode = "n",
                desc = "LSP Finder",
            },
            -- { "K", "<cmd>Lspsaga hover_doc<CR>", mode = "n", desc = "Hover" },
            {
                "<A-d>",
                "<cmd>Lspsaga term_toggle<CR>",
                mode = { "n", "t" },
                desc = "Lspsaga Terminal",
            },

            { "<leader>la", "<cmd>Lspsaga code_action<CR>",          desc = "CodeActions" },
            { "<leader>lr", "<cmd>Lspsaga rename<CR>",               desc = "Rename" },
            { "<leader>ld", "<cmd>Lspsaga peek_definition<CR>",      desc = "PreviewDefinition" },
            { "<leader>lD", "<cmd>Lspsaga peek_type_definition<CR>", desc = "PreviewDefinition" },
            { "<leader>lo", "<cmd>Lspsaga outline<CR>",              desc = "Outline" },
            { "<leader>lc", "<cmd>Lspsaga incoming_calls<CR>",       desc = "Incoming call" },
            { "<leader>lC", "<cmd>Lspsaga outgoing_calls<CR>",       desc = "Outgoing call" },
            { "<leader>lt", "<cmd>Lspsaga term_toggle<CR>",          desc = "Terminal" },
            { "<leader>lh", "<cmd>Lspsaga finder<CR>",               desc = "finder" },
        },
    },
    {
        "lukas-reineke/lsp-format.nvim",
        dir = gen.lsp_format_nvim,
        name = "lsp_format_nvim",
        module = "lsp-format",
        config = function()
            require("lsp-format").setup({
                cpp = { exclude = { "astyle", "clang_format", "uncrustify", "clangd" } },
                rust = { exclude = { "dxfmt", "leptosfmt" } }
            })
        end,
    },
    {
        "ThePrimeagen/refactoring.nvim",
        dir = gen.refactoring_nvim,
        name = "refactoring_nvim",
        module = "refactoring",
        event = "LspAttach",
        dependencies = {
            "plenary_nvim",
            "nvim_treesitter",
        },
        config = function()
            require("refactoring").setup()
        end,
    },
    {
        "mrcjkb/haskell-tools.nvim",
        dir = gen.haskell_tools_nvim,
        version = "^3", -- Recommended
        ft = { "hs", "haskell", "lhaskell", "cabal", "cabalproject" },
    },
    {
        "Vigemus/iron.nvim",
        dir = gen.iron_nvim,
        name = "iron",
        module = "iron",
        cmd = { "IronRepl", "IronRestart", "IronFocus", "IronHide" },
        keys = {
            { "<leader>Rr", function() vim.cmd.IronRepl() end, desc = "Iron Repl", },
        },
        config = function()
            local iron = require("iron.core")

            iron.setup({
                config = {
                    -- Whether a repl should be discarded or not
                    scratch_repl = true,
                    -- Your repl definitions come here
                    repl_definition = {
                        sh = {
                            -- Can be a table or a function that
                            -- returns a table (see below)
                            command = { "zsh" },
                        },
                    },
                    -- How the repl window will be displayed
                    -- See below for more information
                    repl_open_cmd = require("iron.view").bottom(40),
                },
                -- Iron doesn't set keymaps by default anymore.
                -- You can set them here or manually add keymaps to the functions in iron.core
                keymaps = {
                    send_motion = "<space>Rc",
                    visual_send = "<space>Rc",
                    send_file = "<space>Rf",
                    send_line = "<space>Rl",
                    send_until_cursor = "<space>Ru",
                    send_mark = "<space>Rm",
                    mark_motion = "<space>Rc",
                    mark_visual = "<space>Rc",
                    remove_mark = "<space>Rd",
                    cr = "<space>s<cr>",
                    interrupt = "<space>s<space>",
                    exit = "<space>Rq",
                    clear = "<space>Rl",
                },
                -- If the highlight is on, you can change how it looks
                -- For the available options, check nvim_set_hl
                highlight = {
                    italic = true,
                },
                ignore_blank_lines = true, -- ignore blank lines when sending visual select lines
            })
        end,
    },
    {
        "chrisgrieser/nvim-dr-lsp",
        dir = gen.dr_lsp,
        lazy = true,
        name = "dr_lsp",
        config = function() end,
        module = "dr-lsp",
    },
    {
        "SmiteshP/nvim-navbuddy",
        dir = gen.navbuddy,
        name = "navbuddy",
        dependencies = {
            "nui_nvim",
            "navic",
            "nvim_lspconfig",
            "telescope_nvim",
        },
        cmd = "Navbuddy",
        keys = {
            {
                "<leader>ln",
                function()
                    require("nvim-navbuddy").open()
                end,
                desc = "navbuddy",
            },
        },
        config = function()
            local navbuddy = require("nvim-navbuddy")
            navbuddy.setup({
                window = {
                    border = "rounded", -- "rounded", "double", "solid", "none"
                    -- or an array with eight chars building up the border in a clockwise fashion
                    -- starting with the top-left corner. eg: { "╔", "═" ,"╗", "║", "╝", "═", "╚", "║" }.
                    size = "80%",     -- Or table format example: { height = "40%", width = "100%"}
                    position = "50%", -- Or table format example: { row = "100%", col = "0%"}
                    scrolloff = nil,  -- scrolloff value within navbuddy window
                    sections = {
                        left = {
                            size = "20%",
                            border = nil, -- You can set border style for each section individually as well.
                        },
                        mid = {
                            size = "40%",
                            border = nil,
                        },
                        right = {
                            -- No size option for right most section. It fills to
                            -- remaining area.
                            border = nil,
                            preview = "leaf", -- Right section can show previews too.
                            -- Options: "leaf", "always" or "never"
                        },
                    },
                },
                node_markers = {
                    enabled = true,
                    icons = {
                        leaf = "  ",
                        leaf_selected = "  ",
                        branch = "󰇘 ",
                    },
                },
                icons = require("core.theme").symbols.lsp:icons(),
                use_default_mappings = true, -- If set to false, only mappings set
                -- by user are set. Else default
                -- mappings are used for keys
                -- that are not set by user
                lsp = {
                    auto_attach = true, -- If set to true, you don't need to manually use attach function
                    preference = nil,   -- list of lsp server names in order of preference
                },
                source_buffer = {
                    follow_node = true, -- Keep the current node in focus on the source buffer
                    highlight = true,   -- Highlight the currently focused node
                    reorient = "smart", -- "smart", "top", "mid" or "none"
                    scrolloff = nil,    -- scrolloff value when navbuddy is open
                },
            })
        end,
    },
    {
        "zeioth/garbage-day.nvim",
        dir = gen.garbage_day,
        name = "garbage_day",
        dependencies = "nvim_lspconfig",
        lazy = true,
        module = "garbage-day",
        config = function()
            require("garbage-day").setup({
                grace_period = 60 * 60,
                notifications = true,
                timeout = 1000 * 16,
            })
        end
    },
    {
        "epwalsh/obsidian.nvim",
        dir = gen.obsidian_nvim,
        name = "obsidian_nvim",
        cmd = {
            "ObsidianOpen",
            "ObsidianNew",
            "ObsidianQuickSwitch",
            "ObsidianFollowLink",
            "ObsidianBacklinks",
            "ObsidianTags",
            "ObsidianToday",
            "ObsidianYesterday",
            "ObsidianTomorrow",
            "ObsidianDailies",
            "ObsidianTemplate",
            "ObsidianSearch",
            "ObsidianLink",
            "ObsidianLinkNew",
            "ObsidianLinks",
            "ObsidianExtractNote",
            "ObsidianWorkspace",
            "ObsidianPasteImg",
            "ObsidianRename",
            "ObsidianToggleCheckbox",
        },
        ft = "markdown",
        dependencies = { "plenary_nvim" },
        module = "obsidian",
        config = function()
            local async = require "plenary.async"
            local path_lib = require "plenary.path"
            local home = vim.loop.os_homedir()
            async.run(function()
                local workspaces
                if vim.fn.has("win32") == 0 then
                    workspaces = {
                        {
                            name = "personal",
                            path = home .. "/文档/Obsidian",
                        },
                        {
                            name = "work",
                            path = home .. "/文档/Obsidian-work",
                        },
                    }
                else
                    workspaces = {
                        {
                            name = "work",
                            path = home .. "/Documents/Obsidian-work",
                        },
                        {
                            name = "personal",
                            path = home .. "/Documents/Obsidian",
                        },
                    }
                end

                for _, workspace in pairs(workspaces) do
                    local path = path_lib:new(workspace.path)
                    path:mkdir({ parents = true, exists_ok = true })
                end
                require("obsidian").setup({ workspaces = workspaces })
            end)
        end,
    },
}
