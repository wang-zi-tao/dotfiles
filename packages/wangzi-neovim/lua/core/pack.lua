local n = require("core.gen") or {}
local packer = require("packer")
packer.init({
    package_root = n.core and n.core .. "/site/pack",
    display = {
        open_fn = function()
            return require("packer.util").float({ border = "single" })
        end,
        prompt_border = "single",
    },
    git = {
        clone_timeout = 6000, -- seconds
    },
    compile_path = n.compile_path,
    auto_clean = true,
    compile_on_sync = true,
})
packer.startup(function(use)
    use({
        n.core or vim.fn.stdpath("config"),
        as = "core",
        config = function()
            require("core")
        end,
    })
    use({
        n.packer or "wbthomason/packer.nvim",
        as = "packer.nvim",
        opt = true,
    })
    use({
        n.plenary_nvim or "nvim-lua/plenary.nvim",
        module = "plenary",
        requires = "core",
        as = "plenary_nvim",
    })
    use({
        n.impatient_nvim or "lewis6991/impatient.nvim",
        as = "impatient_nvim",
        disable = true,
        config = function()
            require("impatient")
        end,
    })
    -- use({
    --     n.filetype or "nathom/filetype.nvim",
    --     as = "filetype"
    -- })

    use({
        n.onedark_nvim or "navarasu/onedark.nvim",
        as = "onedark_nvim",
        requires = "core",
        config = function()
            require("core.plugins.onedark")
        end,
    })

    use({
        n.nvim_web_devicons or "kyazdani42/nvim-web-devicons",
        as = "nvim_web_devicons",
        after = "nvim_treesitter",
        config = function()
            require("core.plugins.icons")
        end,
        run = ":TSUpdate",
    })

    use({
        n.feline_nvim or "feline-nvim/feline.nvim",
        as = "feline_nvim",
        after = "nvim_web_devicons",
        requires = "nvim_web_devicons",
        config = function()
            require("core.plugins.statusline")
        end,
    })

    use({
        n.bufferline_nvim or "akinsho/bufferline.nvim",
        as = "bufferline_nvim",
        after = "nvim_web_devicons",
        requires = "nvim_web_devicons",
        module = "bufferline",
        config = function()
            require("core.plugins.bufferline")
        end,
    })

    use({
        n.indent_blankline_nvim or "lukas-reineke/indent-blankline.nvim",
        as = "indent_blankline_nvim",
        event = "BufRead",
        config = function()
            require("core.plugins.others").blankline()
        end,
    })

    use({
        n.nvim_colorizer_lua or "norcalli/nvim-colorizer.lua",
        as = "nvim_colorizer_lua",
        event = "BufRead",
        config = function()
            require("core.plugins.others").colorizer()
        end,
    })

    use({
        n.nvim_treesitter or "nvim-treesitter/nvim-treesitter",
        as = "nvim_treesitter",
        event = "BufRead",
        config = function()
            require("core.plugins.treesitter")
        end,
        -- run = ":TSUpdate",
    })

    use({
        n.gitsigns_nvim or "lewis6991/gitsigns.nvim",
        opt = true,
        as = "gitsigns.nvim",
        module = "gitsigns",
        config = function()
            require("core.plugins.gitsigns")
        end,
        setup = function()
            vim.defer_fn(function()
                require("packer").loader("gitsigns.nvim")
            end, 0)
        end,
    })
    use({
        n.vgit_nvim or "tanvirtin/vgit.nvim",
        as = "vgit_nvim",
        requires = "plenary_nvim",
        cmd = "VGit",
        config = function()
            require("core.plugins.vgit")
        end,
    })

    use({
        n.nvim_lspconfig or "neovim/nvim-lspconfig",
        as = "nvim_lspconfig",
        module = "lspconfig",
        opt = true,
        setup = function()
            vim.defer_fn(function()
                require("packer").loader("nvim_lspconfig")
                vim.cmd('if &ft == "packer" | echo "" | else | silent! e %')
            end, 0)
        end,
        config = function()
            require("core.plugins.lspconfig")
        end,
    })

    use({
        n.lsp_signature_nvim or "ray-x/lsp_signature.nvim",
        as = "lsp_signature_nvim",
        after = "nvim_lspconfig",
        config = function()
            require("core.plugins.others").signature()
        end,
    })

    use({
        n.vim_matchup or "andymass/vim-matchup",
        as = "vim_matchup",
        opt = true,
        requires = "core",
        setup = function()
            vim.defer_fn(function()
                require("packer").loader("vim_matchup")
            end, 0)
        end,
    })

    use({
        n.better_escape_nvim or "max397574/better-escape.nvim",
        as = "better_escape_nvim",
        disable = true,
        config = function()
            require("core.plugins.others").better_escape()
        end,
    })

    use({
        n.friendly_snippets or "rafamadriz/friendly-snippets",
        as = "friendly_snippets",
        module = "cmp_nvim_lsp",
    })

    use({
        n.nvim_cmp or "hrsh7th/nvim-cmp",
        as = "nvim_cmp",
        after = "friendly_snippets",
        module = "cmp",
        config = function()
            require("core.plugins.cmp")
        end,
        event = "BufEnter",
    })

    use({
        n.luasnip or "l3mon4d3/luasnip",
        as = "luasnip",
        module = "luasnip",
        after = "friendly_snippets",
        config = function()
            require("core.plugins.others").luasnip()
        end,
    })

    use({
        n.cmp_luasnip or "saadparwaiz1/cmp_luasnip",
        as = "cmp_luasnip",
        after = "luasnip",
        config = function()
            require("cmp").register_source("luasnip", require("cmp_luasnip").new())
        end,
    })

    use({
        n.cmp_nvim_lua or "hrsh7th/cmp-nvim-lua",
        as = "cmp_nvim_lua",
        after = "cmp_luasnip",
        config = function()
            require("cmp").register_source("nvim_lua", require("cmp_nvim_lua").new())
        end,
    })

    use({
        n.cmp_nvim_lsp or "hrsh7th/cmp-nvim-lsp",
        as = "cmp_nvim_lsp",
        after = "cmp_nvim_lua",
        module = "cmp_nvim_lsp",
        config = function()
            require("cmp_nvim_lsp").setup()
        end,
    })

    use({
        n.cmp_buffer or "hrsh7th/cmp-buffer",
        as = "cmp_buffer",
        after = "nvim_cmp",
        requires = "nvim_cmp",
        module = "cmp_buffer",
        config = function()
            require("cmp").register_source("buffer", require("cmp_buffer").new())
        end,
    })
    if n.cmp_tabnine ~= false then
        local run
        if (0== vim.fn.has("win32")) then
            run='./install.sh'
        else
            run='powershell ./install.ps1'
        end
        use({
            n.cmp_tabnine or "tzachar/cmp-tabnine",
            as = "cmp_tabnine",
            run = run,
            requires = "nvim_cmp",
            after = "nvim_cmp",
            module = "cmp_tabnine",
            config = function()
                require("core.plugins.others").cmp_tabnine()
            end,
        })
    end
    use({
        n.cmp_spell or "f3fora/cmp-spell",
        as = "cmp_spell",
        after = "nvim_cmp",
        requires = "nvim_cmp",
        config = function()
            require("cmp").register_source("spell", require("cmp-spell").new())
        end,
    })
    use({
        n.cmp_path or "hrsh7th/cmp-path",
        as = "cmp_path",
        after = "cmp_buffer",
        requires = "cmp_buffer",
        config = function()
            require("cmp").register_source("path", require("cmp_path").new())
        end,
    })
    use({
        n.cmp_zsh or "tamago324/cmp-zsh",
        as = "cmp_zsh",
        after = "cmp_buffer",
        requires = "cmp_buffer",
        config = function()
            require("cmp").register_source("zsh", require("cmp_zsh").new())
            require("cmp_zsh").setup({
                zshrc = true, -- Source the zshrc (adding all custom completions). default: false
                filetypes = { "deoledit", "zsh" }, -- Filetypes to enable cmp_zsh source. default: {"*"}
            })
        end,
    })
    use({
        n.cmp_git or "petertriho/cmp-git",
        as = "cmp_git",
        after = "cmp_buffer",
        requires = "cmp_buffer",
        config = function()
            require("core.plugins.cmp_git")
        end,
    })

    use({
        n.nvim_autopairs or "windwp/nvim-autopairs",
        as = "nvim_autopairs",
        config = function()
            require("core.plugins.others").autopairs()
        end,
    })

    use({
        n.alpha_nvim or "goolord/alpha-nvim",
        as = "alpha_nvim",
        config = function()
            require("core.plugins.alpha")
        end,
    })

    use({
        n.nvim_comment or "terrortylor/nvim-comment",
        as = "nvim_comment",
        requires = "core",
        module = "nvim_comment",
        keys = { "gcc", "gc" },
        config = function()
            require("core.plugins.others").comment()
        end,
    })

    use({
        n.nvim_tree_lua or "kyazdani42/nvim-tree.lua",
        as = "nvim_tree_lua",
        wants = "nvim_web_devicons",
        requires = "onedark_nvim",
        module = "nvim-tree",
        cmd = { "NvimTreeToggle", "NvimTreeFocus" },
        config = function()
            require("core.plugins.nvimtree")
        end,
    })

    use({
        n.telescope_nvim or "nvim-telescope/telescope.nvim",
        as = "telescope_nvim",
        module = "telescope",
        cmd = "Telescope",
        config = function()
            require("core.plugins.telescope")
        end,
    })
    use({
        n.telescope_ui_select or "nvim-telescope/telescope-ui-select.nvim",
        as = "telescope_ui_select",
        module = "telescope._extensions.ui-select",
    })
    use({
        n.telescope_dap_nvim or "nvim-telescope/telescope-dap.nvim",
        as = "telescope_dap_nvim",
        module = "telescope._extensions.dap",
    })
    use({
        n.telescope_project_nvim or "nvim-telescope/telescope-project.nvim",
        as = "telescope_project_nvim",
        module = "telescope._extensions.project",
    })
    use({
        n.telescope_live_grep_args_nvim or "nvim-telescope/telescope-live-grep-args.nvim",
        as = "telescope_live_grep_args_nvim",
        module = "telescope._extensions.live_grep_args",
    })
    use({
        n.telescope_fzf_native_nvim or "nvim-telescope/telescope-fzf-native.nvim",
        as = "telescope_fzf_native_nvim",
        module = "telescope._extensions.fzf",
        run = 'make',
    })
    use({
        n.telescope_frecency_nvim or "nvim-telescope/telescope-frecency.nvim",
        as = "telescope_frecency_nvim",
        module = "telescope._extensions.frecency",
    })
    use({
        n.sqlite or "kkharji/sqlite.lua",
        as = "sqlite",
        module = "sqlite",
        setup = function()
            require("core.plugins.others").sqlite()
        end,
    })
    use({
        n.telescope_file_browser_nvim or "nvim-telescope/telescope-file-browser.nvim",
        as = "telescope_file_browser_nvim",
        module = "telescope._extensions.file_browser",
    })
    use({
        n.dressing_nvim or "stevearc/dressing.nvim",
        as = "dressing_nvim",
        config = function()
            require("core.plugins.dressing")
        end,
    })
    use({
        n.trouble_nvim or "folke/trouble.nvim",
        as = "trouble_nvim",
        requires = "nvim_web_devicons",
        config = function()
            require("core.plugins.trouble")
        end,
    })
    use({
        n.null_ls or "jose-elias-alvarez/null-ls.nvim",
        as = "null_ls",
        after = "nvim_lspconfig",
        config = function()
            require("core.plugins.null_ls")
        end,
    })
    use({
        n.which_key or "folke/which-key.nvim",
        as = "which_key",
        requires = "onedark_nvim",
        module = "which-key",
        config = function()
            require("core.plugins.others").which_key()
        end,
    })
    use({
        n.symbols_outline or "simrat39/symbols-outline.nvim",
        as = "symbols_outline",
        module = "symbols-outline",
        cmd = {
            "SymbolsOutline",
            "SymbolsOutlineOpen",
            "SymbolsOutlineClose",
        },
        config = function()
            require("core.plugins.symbols_outline_pre")
        end,
    })
    use({
        n.rust_tools or "simrat39/rust-tools.nvim",
        as = "rust_tools",
        cmd = {
            "RustSetInlayHints",
            "RustDisableInlayHints",
            "RustToggleInlayHints",
            "RustRunnables",
            "RustExpandMacro",
            "RustOpenCargo",
            "RustParentModule",
            "RustJoinLines",
            "RustHoverActions",
            "RustHoverRange",
            "RustMoveItemDown",
            "RustMoveItemUp",
            "RustStartStandaloneServerForBuffer",
            "RustDebuggables",
            "RustViewCrateGraph",
            "RustReloadWorkspace",
            "RustSSR",
        },
        config = function()
            require("core.plugins.others").rust_tools()
        end,
        ft = { "rs", "rust", "toml" },
    })
    if n.markdown_preview then
        use({
            n.markdown_preview or "davidgranstrom/nvim-markdown-preview",
            as = "markdown_preview",
            ft = "markdown",
            cmd = "MarkdownPreview",
        })
    end
    use({
        n.marks or "chentoast/marks.nvim",
        as = "marks",
        requires = "core",
        config = function()
            require("core.plugins.others").marks()
        end,
    })
    use({
        n.auto_save or "Pocco81/AutoSave.nvim",
        as = "auto_save",
        requires = "core",
        config = function()
            require("core.plugins.others").auto_save()
        end,
    })
    use({
        n.undotree or "mbbill/undotree",
        requires = "core",
        as = "undotree",
        cmd = "UndotreeToggle",
    })
    use({
        n.ts_rainbow or "p00f/nvim-ts-rainbow",
        as = "ts_rainbow",
        after = "nvim_treesitter",
        requires = "nvim_treesitter",
    })
    use({
        n.diffview or "sindrets/diffview.nvim",
        as = "diffview",
        requires = "plenary_nvim",
        module = "diffview",
        cmd = {
            "DiffviewOpen",
            "DiffviewClose",
            "DiffviewClose",
            "DiffviewFocusFiles",
            "DiffviewRefresh",
        },
        config = function()
            require("core.plugins.others").diffview()
        end,
    })
    use({
        n.navigator or 'numToStr/Navigator.nvim',
        as = "navigator",
        requires = "core",
        module = "Navigator",
        config = function()
            require("core.plugins.others").navigator()
        end,
    })
    use({
        n.illuminate or "RRethy/vim-illuminate",
        as = "illuminate",
        module = "illuminate",
        after = "nvim_treesitter",
        requires = "onedark_nvim",
    })
    use({
        n.hlslens or "kevinhwang91/nvim-hlslens",
        as = "hlslens",
        after = "nvim_treesitter",
        config = function()
            require("core.plugins.hlslens")
        end,
    })
    use({
        n.pretty_fold or "anuvyklack/pretty-fold.nvim",
        as = "pretty_fold",
        after = "nvim_treesitter",
        config = function()
            require("core.plugins.pretty_fold")
        end,
    })
    use({
        n.pretty_fold_preview or "anuvyklack/fold-preview.nvim",
        as = "pretty_fold_preview",
        -- requires = "keymap_amend",
        after = "pretty_fold",
        config = function()
            require("core.plugins.others").pretty_fold_preview()
        end,
    })
    use({
        n.keymap_amend or "anuvyklack/keymap-amend.nvim",
        as = "keymap_amend",
        -- module = "keymap-amend",
    })

    use({
        n.ts_autotag or "windwp/nvim-ts-autotag",
        as = "ts_autotag",
        after = "nvim_treesitter",
        requires = "nvim_treesitter",
        config = function()
            require("core.plugins.others").ts_autotag()
        end,
    })
    use({
        n.lspsaga or "glepnir/lspsaga.nvim",
        as = "lspsaga",
        after = "cmp_nvim_lsp",
        config = function()
            require("core.plugins.others").lspsaga()
        end,
        cmd = "Lspsaga",
        module = "lspsaga",
    })
    use({
        n.dap or "mfussenegger/nvim-dap",
        as = "dap",
        module = "dap",
        config = function()
            require("core.plugins.dap")
        end,
    })
    use({
        n.dap_ui or "rcarriga/nvim-dap-ui",
        as = "dap_ui",
        after = "dap",
        module = "dapui",
        config = function()
            require("core.plugins.others").dap_ui()
        end,
    })
    use({
        n.dap_virtual_text or "theHamsta/nvim-dap-virtual-text",
        as = "dap_virtual_text",
        after = "dap",
        config = function()
            require("core.plugins.others").dap_virtual_text()
        end,
    })
    use({
        n.persistent_breakpoints_nvim or "Weissle/persistent-breakpoints.nvim",
        as = "persistent_breakpoints_nvim",
        after = "dap",
        module = { "persistent-breakpoints", "persistent-breakpoints.api" },
        config = function()
            require("core.plugins.others").persistent_breakpoints_nvim()
        end,
    })
    use({
        n.fterm or "numToStr/FTerm.nvim",
        as = "fterm",
        module = "FTerm",
        config = function()
            require("core.plugins.others").fterm()
        end,
    })
    use({
        n.mini or "echasnovski/mini.nvim",
        as = "mini",
        requires = "core",
        config = function()
            require("core.plugins.others").mini()
        end,
    })
    use({
        n.session_manager or "Shatur/neovim-session-manager",
        as = "session_manager",
        config = function()
            require("core.plugins.others").session_manager()
        end,
    })
    use({
        n.firenvim or "glacambre/firenvim",
        as = "firenvim",
        requires = "core",
        run = function()
            vim.fn["firenvim#install"](0)
        end,
    })
    use({
        n.cmake or "Shatur/neovim-cmake",
        as = "cmake",
        cmd = { "CMake" },
        ft = { "cpp", "c", "hpp", "h", "CMakeLists.txt" },
        config = function()
            require("core.plugins.cmake")
        end,
    })
    use({
        n.crates_nvim or "saecki/crates.nvim",
        as = "crates_nvim",
        requires = { 'plenary_nvim' },
        ft = { "Cargo.toml" },
        module = "cmp_crates",
        config = function()
            require("core.plugins.crates")
        end,
    })
    use({
        n.perfanno_nvim or "t-troebst/perfanno.nvim",
        as = "perfanno_nvim",
        cmd = { "PerfLoadFlat", "PerfLoadCallGraph", "PerfLoadFlameGraph", "PerfLuaProfileStart", "PerfLuaProfileStop",
            "PerfPickEvent", "PerfCycleFormat", "PerfAnnotate", "PerfToggleAnnotations", "PerfAnnotateSelection",
            "PerfAnnotateFunction", "PerfHottestLines", "PerfHottestSymbols", "PerfHottestCallersSelection",
            "PerfHottestCallersFunction" },
        config = function()
            require("core.plugins.perf")
        end,
    })
    use({
        n.hop_nvim or "phaazon/hop.nvim",
        as = "hop_nvim",
        module = "hop",
        config = function()
            require("core.plugins.others").hop()
        end,
    })
    use({
        n.distant or "chipsenkbeil/distant.nvim",
        as = "distant",
        module = "distant",
        cmd = { "DistantOpen", "DistantLaunch", "DistantInstall" },
        config = function()
            require("core.plugins.others").distant()
        end,
    })

end)
vim.cmd([[PackerInstall]])
packer.compile(n.compile_path)

local M = {}
M.packer = packer
M.status = packer.status
return M
