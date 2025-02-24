local function config()
    if 1 == vim.fn.has("win32") then
        require("nvim-treesitter.install").prefer_git = false
    end

    vim.filetype.add({ extension = { wgsl = "wgsl" } })
    local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
    parser_config.wgsl = {
        install_info = {
            url = "https://github.com/szebniok/tree-sitter-wgsl",
            files = { "src/parser.c" },
        },
    }

    local parser_dir = vim.fn.stdpath("cache") .. "/treesitter"
    vim.opt.runtimepath:append(parser_dir)

    require("nvim-treesitter.configs").setup({
        -- A list of parser names, or "all"
        --
        -- ensure_installed = { "c", "cpp", "rust", "java", "typescript", "haskell", "lua", "python", "vim", "vimdoc", "query" },

        -- Install parsers synchronously (only applied to `ensure_installed`)
        sync_install = false,

        -- Automatically install missing parsers when entering buffer
        -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
        auto_install = false,

        -- List of parsers to ignore installing (for "all")
        ignore_install = {},

        ---- If you need to change the installation directory of the parsers (see -> Advanced Setup)

        highlight = {
            -- `false` will disable the whole extension
            enable = true,
            -- NOTE: these are the names of the parsers and not the filetype. (for example if you want to
            -- disable highlighting for the `tex` filetype, you need to include `latex` in this list as this is
            -- the name of the parser)
            -- list of language that will be disabled
            -- Or use a function for more flexibility, e.g. to disable slow treesitter highlight for large files
            disable = function(lang, buf)
                local max_filesize = 100 * 1024 -- 100 KB
                local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
                if ok and stats and stats.size > max_filesize then
                    return true
                end
            end,
            -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
            -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
            -- Using this option may slow down your editor, and you may see some duplicate highlights.
            -- Instead of true it can also be a list of languages
            additional_vim_regex_highlighting = true,
        },

        parser_install_dir = parser_dir,

        incremental_selection = {
            enable = true,
            keymaps = {
                init_selection = "gnn",
                node_incremental = "grn",
                scope_incremental = "grc",
                node_decremental = "grm",
            },
        },

        indent = {
            enable = true,
        },
    })
end

return {
    "nvim-treesitter/nvim-treesitter",
    dir = gen.nvim_treesitter,
    name = "nvim_treesitter",
    event = { "BufNewFile", "BufReadPost" },
    lazy = true,
    enabled = false,
    dependencies = {
        "nvim_web_devicons",
        {
            "HiPhish/rainbow-delimiters.nvim",
            dir = gen.rainbow_delimiters,
            name = "rainbow_delimiters",
            lazy = true,
            module = "rainbow-delimiters",
            config = function()
                local rainbow_delimiters = require("rainbow-delimiters")
                require("rainbow-delimiters.setup").setup({
                    strategy = {
                        [""] = rainbow_delimiters.strategy["global"],
                        vim = rainbow_delimiters.strategy["local"],
                    },
                    query = {
                        [""] = "rainbow-delimiters",
                        lua = "rainbow-blocks",
                    },
                    priority = {
                        [""] = 110,
                        lua = 210,
                    },
                    highlight = {
                        "RainbowDelimiterRed",
                        "RainbowDelimiterYellow",
                        "RainbowDelimiterBlue",
                        "RainbowDelimiterOrange",
                        "RainbowDelimiterGreen",
                        "RainbowDelimiterViolet",
                        "RainbowDelimiterCyan",
                    },
                })
            end,
        },
        {
            "windwp/nvim-ts-autotag",
            dir = gen.ts_autotag,
            name = "ts_autotag",
            lazy = true,
            config = function() end,
        },
        -- {
        --     "anuvyklack/pretty-fold.nvim",
        --     dir = gen.pretty_fold,
        --     name = "pretty_fold",
        --     enabled = false,
        --     lazy = true,
        --     config = function()
        --         require("core.plugins.pretty_fold")
        --     end,
        -- },
        -- {
        --     "anuvyklack/fold-preview.nvim",
        --     dir = gen.pretty_fold_preview,
        --     name = "pretty_fold_preview",
        --     enabled = false,
        --     -- dependencies = "keymap_amend",
        --     dependencies = "pretty_fold",
        --     lazy = true,
        --     config = function()
        --         require("fold-preview").setup({ border = "rounded" })
        --     end,
        -- },
        {
            "kevinhwang91/nvim-hlslens",
            dir = gen.hlslens,
            name = "hlslens",
            lazy = true,
            config = function()
                require("scrollbar.handlers.search").setup({
                    -- hlslens config overrides
                })
            end,
        },
        {
            "nvim-treesitter/nvim-treesitter-textobjects",
            dir = gen.nvim_treesitter_textobjects,
            name = "nvim_treesitter_textobjects",
            config = function()
                require("nvim-treesitter.configs").setup({
                    textobjects = {
                        select = {
                            enable = true,
                            lookahead = true,
                            keymaps = {
                                ["af"] = "@function.outer",
                                ["if"] = "@function.inner",
                                ["iif"] = "@function.inner",
                                ["iF"] = "@frame.inner",
                                ["aF"] = "@frame.outer",
                                ["ib"] = "@block.inner",
                                ["ab"] = "@block.outer",
                                ["ic"] = "@call.inner",
                                ["ac"] = "@call.outer",
                                ["io"] = "@loop.inner",
                                ["ao"] = "@loop.outer",
                                ["ip"] = "@parameter.inner",
                                ["ap"] = "@parameter.outer",
                                ["iP"] = "@parameter.inner",
                                ["aP"] = "@parameter.outer",
                                ["im"] = "@comment.inner",
                                ["am"] = "@comment.outer",
                                ["in"] = "@assignment.inner",
                                ["an"] = "@assignment.outer",
                                ["iN"] = "@assignment.lhs",
                                ["aN"] = "@assignment.rhs",
                                ["id"] = "@conditional.inner",
                                ["ad"] = "@conditional.outer",
                                ["iC"] = { query = "@class.inner", desc = "Select inner part of a class region" },
                                ["aC"] = "@class.outer",
                                ["as"] = { query = "@scope", query_group = "locals", desc = "Select language scope" },
                            },
                            selection_modes = {
                                ["@parameter.outer"] = "v", -- charwise
                                ["@function.outer"] = "V",  -- linewise
                                ["@class.outer"] = "<c-v>", -- blockwise
                            },
                            include_surrounding_whitespace = true,
                        },
                        swap = {
                            enable = true,
                            swap_next = {
                                ["<leader>sa"] = "@parameter.inner",
                            },
                            swap_previous = {
                                ["<leader>sA"] = "@parameter.inner",
                            },
                        },
                        move = {
                            enable = true,
                            set_jumps = true, -- whether to set jumps in the jumplist
                            goto_next_start = {
                                ["]m"] = "@function.outer",
                                ["]]m"] = "@function.outer",
                                ["]c"] = { query = "@class.outer", desc = "Next class start" },
                                ["]]c"] = { query = "@class.outer", desc = "Next class start" },
                                ["]o"] = "@loop.*",
                                ["]]o"] = "@loop.*",
                                ["]b"] = { query = "@scope", query_group = "locals", desc = "Next scope" },
                                ["]]b"] = { query = "@scope", query_group = "locals", desc = "Next scope" },
                                ["]z"] = { query = "@fold", query_group = "folds", desc = "Next fold" },
                                ["]]z"] = { query = "@fold", query_group = "folds", desc = "Next fold" },
                            },
                            goto_next_end = {
                                ["]M"] = "@function.outer",
                                ["]["] = "@class.outer",
                            },
                            goto_previous_start = {
                                ["[m"] = "@function.outer",
                                ["[["] = "@class.outer",
                            },
                            goto_previous_end = {
                                ["[m"] = "@function.outer",
                                ["[[m"] = "@function.outer",
                                ["[c"] = { query = "@class.outer", desc = "Next class start" },
                                ["[[c"] = { query = "@class.outer", desc = "Next class start" },
                                ["[o"] = "@loop.*",
                                ["[[o"] = "@loop.*",
                                ["[b"] = { query = "@scope", query_group = "locals", desc = "Next scope" },
                                ["[[b"] = { query = "@scope", query_group = "locals", desc = "Next scope" },
                                ["[z"] = { query = "@fold", query_group = "folds", desc = "Next fold" },
                                ["[[z"] = { query = "@fold", query_group = "folds", desc = "Next fold" },
                            },
                            -- Below will go to either the start or the end, whichever is closer.
                            -- Use if you want more granular movements
                            -- Make it even more gradual by adding multiple queries and regex.
                            goto_next = {
                                ["]i"] = "@conditional.outer",
                            },
                            goto_previous = {
                                ["[i"] = "@conditional.outer",
                            },
                        },
                        lsp_interop = {
                            enable = true,
                            border = "none",
                            floating_preview_opts = {},
                            peek_definition_code = {
                                ["<leader>df"] = "@function.outer",
                                ["<leader>dF"] = "@class.outer",
                            },
                        },
                    },
                })
            end,
        },
        {
            "nvim-treesitter/nvim-treesitter-context",
            dir = gen.nvim_treesitter_context,
            name = "nvim_treesitter_context",
            lazy = true,
            config = function()
                require 'treesitter-context'.setup {
                    enable = true,           -- Enable this plugin (Can be enabled/disabled later via commands)
                    multiwindow = false,     -- Enable multiwindow support.
                    max_lines = 8,           -- How many lines the window should span. Values <= 0 mean no limit.
                    min_window_height = 0,   -- Minimum editor window height to enable context. Values <= 0 mean no limit.
                    line_numbers = true,
                    multiline_threshold = 1, -- Maximum number of lines to show for a single context
                    trim_scope = 'outer',    -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
                    mode = 'cursor',         -- Line used to calculate context. Choices: 'cursor', 'topline'
                    -- Separator between context and content. Should be a single character string, like '-'.
                    -- When separator is set, the context will only show up when there are at least 2 lines above cursorline.
                    separator = nil,
                    zindex = 20,     -- The Z-index of the context window
                    on_attach = nil, -- (fun(buf: integer): boolean) return false to disable attaching
                }
            end
        },
    },
    config = config,
    -- build = ":TSUpdate",
}
