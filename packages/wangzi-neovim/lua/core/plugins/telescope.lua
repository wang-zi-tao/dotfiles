local function config()
    local telescope = require("telescope")
    local add_to_trouble = require("trouble.sources.telescope").add
    local open_with_trouble = require("trouble.sources.telescope").open
    telescope.setup({
        defaults = {
            vimgrep_arguments = {
                "rg",
                "--color=never",
                "--no-heading",
                "--with-filename",
                "--line-number",
                "--column",
                "--smart-case",
                "--no-ignore",
            },
            prompt_prefix = "❱ ",
            selection_caret = "  ",
            entry_prefix = "  ",
            initial_mode = "insert",
            selection_strategy = "reset",
            sorting_strategy = "ascending",
            layout_strategy = "horizontal",
            layout_config = {
                horizontal = {
                    prompt_position = "top",
                    preview_width = 0.45,
                    results_width = 0.55,
                },
                vertical = {
                    mirror = true,
                },
                width = 0.95,
                height = 0.90,
                preview_cutoff = 120,
            },
            file_sorter = require("telescope.sorters").get_fuzzy_file,
            file_ignore_patterns = { "node_modules", "target", "build" },
            generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
            path_display = { "truncate" },
            winblend = 0,
            border = {},
            borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
            color_devicons = true,
            use_less = true,
            set_env = { ["COLORTERM"] = "truecolor" }, -- default = nil,
            file_previewer = require("telescope.previewers").vim_buffer_cat.new,
            grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
            qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
            -- Developer configurations: Not meant for general override
            buffer_previewer_maker = require("telescope.previewers").buffer_previewer_maker,
            mappings = {
                i = { ["<c-t>"] = open_with_trouble },
                n = { ["<c-t>"] = open_with_trouble },
            },
        },
        extensions = {
            ["ui-select"] = {
                require("telescope.themes").get_dropdown({
                    -- even more opts
                }),

                -- pseudo code / specification for writing custom displays, like the one
                -- for "codeactions"
                -- specific_opts = {
                --   [kind] = {
                --     make_indexed = function(items) -> indexed_items, width,
                --     make_displayer = function(widths) -> displayer
                --     make_display = function(displayer) -> function(e)
                --     make_ordinal = function(e) -> string
                --   },
                --   -- for example to disable the custom builtin "codeactions" display
                --      do the following
                --   codeactions = false,
                -- }
            },
            file_browser = {
                -- disables netrw and use telescope-file-browser in its place
                hijack_netrw = true,
                mappings = {
                    ["i"] = {
                        -- your custom insert mode mappings
                    },
                    ["n"] = {
                        -- your custom normal mode mappings
                    },
                },
            },
            fzf = {
                fuzzy = true,                   -- false will only do exact matching
                override_generic_sorter = true, -- override the generic sorter
                override_file_sorter = true,    -- override the file sorter
                case_mode = "smart_case",       -- or "ignore_case" or "respect_case"
                -- the default case_mode is "smart_case"
            },
            project = {
                hidden_files = true, -- default: false
                theme = "dropdown",
                order_by = "asc",
                sync_with_nvim_tree = true, -- default false
            },
            frecency = {
                -- db_root = vim.fn.stdpath("cache").."/telescope_frecency.sqlite",
                workspace = "CWD",
                path_display = { "shorten" },
                show_scores = false,
                show_unindexed = true,
                ignore_patterns = { "*.git/*", "*/tmp/*" },
                disable_devicons = false,
            },
            ast_grep = {
                command = {
                    "sg",
                    "--json=stream",
                },                      -- must have --json=stream
                grep_open_files = true, -- search in opened files
                lang = nil,             -- string value, specify language for ast-grep `nil` for default
            }
        },
    })

    local extensions = {
        "ui-select",
        "dap",
        "file_browser",
        -- "fzf",
        "frecency",
        "project",
        "live_grep_args",
        "notify",
        "noice",
        "smart_open",
        "hbac",
    }
    for _, ext in ipairs(extensions) do
        if not pcall(telescope.load_extension, ext) then
            vim.notify("failed to load telescope extension: " .. ext, "error")
        end
    end
end

local function telescope()
    return require("telescope.builtin")
end

return {
    {
        "nvim-telescope/telescope.nvim",
        dir = gen.telescope_nvim,
        name = "telescope_nvim",
        module = "telescope",
        cmd = "Telescope",
        lazy = true,
        config = config,
        init = function()
            require("which-key").add({
                { "<leader>f",  group = "Telescope" },
                { "<leader>fm", group = "Telescope In Module" },
            })
        end,
        keys = {
            {
                "<leader>fk",
                function()
                    telescope().keymaps()
                end,
                desc = "Keymaps",
            },
            {
                "<leader>fw",
                function()
                    pcall(require("core.utils").add_mark)
                    require("telescope.builtin").live_grep({
                        search_dirs = { global.pwd or "." },
                        default_text = require("core.utils").get_selection(),
                    })
                end,
                mode = { "n", "v" },
                desc = "Grep",
            },
            {
                "<leader>fW",
                function()
                    pcall(require("core.utils").add_mark)
                    require("telescope.builtin").live_grep({
                        grep_open_files = true,
                        default_text = require("core.utils").get_selection(),
                    })
                end,
                mode = { "n", "v" },
                desc = "Grep Buffers",
            },
            {
                "<leader>ff",
                function()
                    pcall(require("core.utils").add_mark)
                    require("telescope.builtin").fd({
                        search_dirs = { global.pwd or "." },
                        default_text = require("core.utils").get_selection(),
                        no_ignore = true,
                    })
                end,
                mode = { "n", "v" },
                desc = "Files",
            },
            {
                "<leader>fF",
                function()
                    vim.cmd("Telescope frecency")
                end,
                desc = "frecency",
            },
            {
                "<leader>fr",
                function()
                    telescope().registers()
                end,
                desc = "Registers",
            },
            {
                "<leader>fo",
                function()
                    pcall(require("core.utils").add_mark)
                    require("telescope.builtin").lsp_dynamic_workspace_symbols({
                        default_text = require("core.utils").get_selection(),
                    })
                end,
                mode = { "n", "v" },
                desc = "WorkspaceSymbols",
            },
            {
                "<leader>fa",
                function()
                    vim.lsp.buf.code_action()
                end,
                desc = "Actions",
            },
            {
                "<leader>fi",
                function()
                    pcall(require("core.utils").add_mark)
                    telescope().lsp_references()
                end,
                desc = "LSP Reference",
            },
            {
                "<leader>fb",
                function()
                    telescope().buffers()
                end,
                desc = "Buffers",
            },
            {
                "<leader>fs",
                function()
                    telescope().git_status()
                end,
                desc = "GitStatus",
            },
            {
                "<leader>ft",
                function()
                    telescope().tags()
                end,
                desc = "Tags",
            },
            {
                "<leader>fc",
                function()
                    telescope().git_commits()
                end,
                desc = "GitCommits",
            },
            {
                "<leader>fB",
                function()
                    telescope().git_branches()
                end,
                desc = "GitBranches",
            },
            {
                "<leader>fM",
                function()
                    telescope().marks()
                end,
                desc = "Marks",
            },
            {
                "<leader>fD",
                function()
                    telescope().lsp_definitions()
                end,
                desc = "lsp definitions",
            },
            {
                "<leader>fd",
                function()
                    telescope().lsp_document_symbols()
                end,
                desc = "Lsp_document_symbols",
            },
            {
                "<leader>fp",
                function()
                    require("telescope").extensions.project.project({})
                end,
                desc = "Projects",
            },
            {
                "<leader>fr",
                function()
                    pcall(require("core.utils").add_mark)
                    telescope().lsp_references()
                end,
                desc = "lsp incomint calls",
            },
            {
                "<leader>fR",
                function()
                    telescope().lsp_outgoing_calls()
                end,
                desc = "lsp outgoing calls",
            },
            {
                "<leader>fI",
                function()
                    pcall(require("core.utils").add_mark)
                    require("telescope.builtin").lsp_implementations()
                end,
                desc = "LSP Implementation",
            },
            {
                "<leader>ft",
                function()
                    pcall(require("core.utils").add_mark)
                    require("telescope.builtin").lsp_definitions()
                end,
                desc = "LSP Define",
            },
            {
                "<leader>fT",
                function()
                    require("telescope.builtin").lsp_type_definitions()
                end,
                desc = "LSP TypeDefinition",
            },
            {
                "<leader>fg",
                function()
                    require("core.utils").cachedinput(
                        "telescope_grep_by_filetype",
                        "filetype: ",
                        "*.cpp",
                        "filetype",
                        function(glob)
                            require("telescope.builtin").live_grep({ glob_pattern = glob })
                        end
                    )
                end,
                desc = "Grep by type",
            },
            {
                "<leader>fS",
                function()
                    vim.cmd.Telescope("ast_grep")
                end,
                desc = "AST grep",
            },
            {
                "<leader>fmf",
                function()
                    pcall(require("core.utils").add_mark)
                    local module_dir = require("core.utils").module_dir()
                    require("telescope.builtin").fd({
                        search_dirs = module_dir,
                        default_text = require("core.utils").get_selection(),
                        no_ignore = true,
                    })
                end,
                mode = { "n", "v" },
                desc = "Files",
            },
            {
                "<leader>fmw",
                function()
                    pcall(require("core.utils").add_mark)
                    local module_dir = require("core.utils").module_dir()
                    require("telescope.builtin").live_grep({
                        search_dirs = module_dir,
                        default_text = require("core.utils").get_selection(),
                    })
                end,
                mode = { "n", "v" },
                desc = "Files",
            },
        },
        dependencies = {
            "nvim_web_devicons",
            "noice",
            {
                "nvim-telescope/telescope-project.nvim",
                dir = gen.telescope_project_nvim,
                name = "telescope_project_nvim",
                module = "telescope._extensions.project",
            },
            {
                "nvim-telescope/telescope-ui-select.nvim",
                dir = gen.telescope_ui_select,
                name = "telescope_ui_select",
                module = "telescope._extensions.ui-select",
                lazy = true,
            },
            {
                "nvim-telescope/telescope-dap.nvim",
                dir = gen.telescope_dap_nvim,
                name = "telescope_dap_nvim",
                module = "telescope._extensions.dap",
                keys = {
                    {
                        "<leader>dua",
                        function()
                            require("telescope").extensions.dap.commands({})
                        end,
                        desc = "Commands",
                    },
                    {
                        "<leader>duC",
                        function()
                            require("telescope").extensions.dap.configurations({})
                        end,
                        desc = "Configurations",
                    },
                },
                lazy = true,
            },
            {
                "nvim-telescope/telescope-live-grep-args.nvim",
                dir = gen.telescope_live_grep_args_nvim,
                name = "telescope_live_grep_args_nvim",
                module = "telescope._extensions.live_grep_args",
                lazy = true,
            },
            {
                "nvim-telescope/telescope-fzf-native.nvim",
                dir = gen.telescope_fzf_native_nvim,
                name = "telescope_fzf_native_nvim",
                module = "telescope._extensions.fzf",
                build = "make",
                lazy = true,
            },
            {
                "nvim-telescope/telescope-frecency.nvim",
                dir = gen.telescope_frecency_nvim,
                name = "telescope_frecency_nvim",
                module = "telescope._extensions.frecency",
                lazy = true,
            },
            {
                "nvim-telescope/telescope-file-browser.nvim",
                dir = gen.telescope_file_browser_nvim,
                name = "telescope_file_browser_nvim",
                module = "telescope._extensions.file_browser",
                lazy = true,
            },
            {
                "Marskey/telescope-sg",
                dir = gen.telescope_sg,
                name = "telescope_sg",
                module = "telescope._extensions.ast_grep",
                lazy = true,
            },
            {
                "danielfalk/smart-open.nvim",
                dir = gen.telescope_smart_open,
                name = "telescope_smart_open",
                module = "telescope._extensions.smart_open",
                dependencies = { "sqlite" },
                lazy = true,
            },
        },
    },
}
