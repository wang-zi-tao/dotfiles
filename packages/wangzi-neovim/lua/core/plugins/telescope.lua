local function config()
    local telescope = require("telescope")
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
                base_dirs = {
                    { '~/Code',      max_depth = 1 },
                    { '~/workspace', max_depth = 4 },
                },
                hidden_files = true, -- default: false
                theme = "dropdown",
                order_by = "asc",
                sync_with_nvim_tree = true, -- default false
            },
            frecency = {
                -- db_root = vim.fn.stdpath("cache").."/telescope_frecency.sqlite",
                show_scores = false,
                show_unindexed = true,
                ignore_patterns = { "*.git/*", "*/tmp/*" },
                disable_devicons = false,
                workspaces = {
                    ["workspaces"] = "~/workspace",
                    ["codes"]      = "~/Code"
                }
            },
        },
    })

    local extensions = {
        "ui-select",
        "dap",
        "file_browser",
        "fzf",
        "frecency",
        "project",
        "live_grep_args",
        "notify",
        "noice",
    }
    for _, ext in ipairs(extensions) do
        pcall(telescope.load_extension, telescope, ext)
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
            require("which-key").register({
                    f = { name = "Telescope" },
                },
                { prefix = "<leader>" })
        end,
        keys = {
            { "<leader>fk", function() telescope().keymaps() end,               desc = "Keymaps" },
            {
                "<leader>fw",
                function()
                    require("trailblazer").new_trail_mark()
                    vim.cmd("Telescope live_grep search_dirs="..( global.pwd or "." ))
                end,
                desc = "Grep",
            },
            {
                "<leader>fW",
                function() require('telescope.builtin').live_grep({ grep_open_files = true }) end,
                desc = "Grep Buffers",
            },
            { "<leader>ff", function()
                vim.cmd("Telescope fd search_dirs="..( global.pwd or "." ))
            end, desc = "Files", },
            { "<leader>fr", function() telescope().registers() end,             desc = "Registers", },
            { "<leader>fo", function() telescope().lsp_workspace_symbols() end, desc = "WorkspaceSymbols", },
            { "<leader>fa", function() vim.lsp.buf.code_action() end,           desc = "Actions", },
            {
                "<leader>fi",
                function()
                    require("trailblazer").new_trail_mark()
                    telescope().lsp_references()
                end,
                desc = "LSP Reference",
            },
            { "<leader>fb", function() telescope().buffers() end,      desc = "Buffers", },
            { "<leader>fs", function() telescope().git_status() end,   desc = "GitStatus", },
            { "<leader>ft", function() telescope().tags() end,         desc = "Tags", },
            { "<leader>fc", function() telescope().git_commits() end,  desc = "GitCommits", },
            { "<leader>fB", function() telescope().git_branches() end, desc = "GitBranches", },
            { "<leader>fm", function() telescope().marks() end, desc = "Marks",
            },
            { "<leader>fd", function() telescope().lsp_document_symbols() end,        desc = "Lsp_document_symbols", },
            { "<leader>fp", function() telescope().extensions.project.project {} end, desc = "Projects", },
            { "<leader>fr", function() telescope().lsp_references() end,              desc = "LspReferences", },
            {
                "<leader>fI",
                function()
                    require("trailblazer").new_trail_mark()
                    require("telescope.builtin").lsp_implementations()
                end,
                desc = "LSP Implementation",
            },
            { "<leader>ft", function() require("telescope.builtin").lsp_definitions() end, desc = "LSP Define", },
            {
                "<leader>fT",
                function() require("telescope.builtin").lsp_type_definitions() end,
                desc = "LSP TypeDefinition",
            },
            {
                "<leader>fg",
                function()
                    local glob = require("core.utils").cachedinput("telescope_grep_by_filetype", "filetype: ", ".cpp",
                        "filetype")
                    require('telescope.builtin').live_grep({ glob_pattern = glob })
                end,
                desc = "Grep by type",
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
                    { "<leader>dua", function() require("telescope").extensions.dap.commands({}) end, desc = "Commands", },
                    {
                        "<leader>duC",
                        function() require("telescope").extensions.dap.configurations({}) end,
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
                build = 'make',
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
        },
    },
}
