local function config()
    require('nvim-treesitter.configs').setup {
        -- A list of parser names, or "all"
        -- ensure_installed = {
        --     "lua",
        --     "vim",
        --     "rust",
        --     "nix",
        --     "c",
        --     "cpp",
        --     "java",
        --     "javascript",
        --     "typescript",
        --     "python",
        --     "cmake",
        --     "diff",
        --     "git_rebase",
        --     "gitcommit",
        --     "gitattributes",
        --     "gitignore",
        --     "json",
        --     "llvm",
        --     "regex",
        --     "vue",
        --     "markdown",
        --     "markdown_inline",
        -- },

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

        parser_install_dir = vim.fn.stdpath("cache") .. "/treesitter",

        rainbow = {
            enable = true,
            -- disable = { "jsx", "cpp" }, list of languages you want to disable the plugin for
            extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
            max_file_lines = nil, -- Do not enable for files with more than n lines, int
            -- colors = {}, -- table of hex strings
            -- termcolors = {} -- table of colour name strings
        },

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
    }
end

return {
    "nvim-treesitter/nvim-treesitter",
    dir = gen.nvim_treesitter,
    name = "nvim_treesitter",
    event = "VeryLazy",
    lazy = true,
    dependencies = {
        "nvim_web_devicons",
        {
            "p00f/nvim-ts-rainbow",
            dir = gen.ts_rainbow,
            name = "ts_rainbow",
            lazy = true,
        },
        {
            "windwp/nvim-ts-autotag",
            dir = gen.ts_autotag,
            name = "ts_autotag",
            lazy = true,
            config = function()

            end,
        },
        {
            "anuvyklack/pretty-fold.nvim",
            dir = gen.pretty_fold,
            name = "pretty_fold",
            lazy = true,
            config = function()
                require("core.plugins.pretty_fold")
            end,
        },
        {
            "anuvyklack/fold-preview.nvim",
            dir = gen.pretty_fold_preview,
            name = "pretty_fold_preview",
            -- dependencies = "keymap_amend",
            dependencies = "pretty_fold",
            lazy = true,
            config = function()
                require('fold-preview').setup { border = "rounded" }
            end,
        },
        {
            "kevinhwang91/nvim-hlslens",
            dir = gen.hlslens,
            name = "hlslens",
            lazy = true,
            config = function()
                require("core.plugins.hlslens")
            end,
        },
        {
            "nvim-treesitter/nvim-treesitter-textobjects",
            dir = gen.nvim_treesitter_textobjects,
            name = "nvim_treesitter_textobjects",
            config = function()
                require 'nvim-treesitter.configs'.setup {
                    textobjects = {
                        select = {
                            enable = true,
                            lookahead = true,
                            keymaps = {
                                ["af"] = "@function.outer",
                                ["if"] = "@function.inner",
                                ["ac"] = "@class.outer",
                                ["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
                                ["as"] = { query = "@scope", query_group = "locals", desc = "Select language scope" },
                            },
                            selection_modes = {
                                ['@parameter.outer'] = 'v', -- charwise
                                ['@function.outer'] = 'V', -- linewise
                                ['@class.outer'] = '<c-v>', -- blockwise
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
                                ["]]"] = { query = "@class.outer", desc = "Next class start" },
                                ["]o"] = "@loop.*",
                                ["]s"] = { query = "@scope", query_group = "locals", desc = "Next scope" },
                                ["]z"] = { query = "@fold", query_group = "folds", desc = "Next fold" },
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
                                ["[M"] = "@function.outer",
                                ["[]"] = "@class.outer",
                            },
                            -- Below will go to either the start or the end, whichever is closer.
                            -- Use if you want more granular movements
                            -- Make it even more gradual by adding multiple queries and regex.
                            goto_next = {
                                ["]i"] = "@conditional.outer",
                            },
                            goto_previous = {
                                ["[i"] = "@conditional.outer",
                            }
                        },
                        lsp_interop = {
                            enable = true,
                            border = 'none',
                            floating_preview_opts = {},
                            peek_definition_code = {
                                ["<leader>df"] = "@function.outer",
                                ["<leader>dF"] = "@class.outer",
                            },
                        },
                    },
                }
            end
        }
    },
    config = config,
    -- build = ":TSUpdate",
}
