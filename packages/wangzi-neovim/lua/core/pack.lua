local n = require("core.gen")
local packer = require("packer")
packer.init({
  package_root = n.core .. "/site/pack",
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
packer.startup(function()
  use({
    n.core,
    as = "core",
    config = function()
      require("core")
    end,
  })
  use({
    n.packer,
    as = "packer.nvim",
    opt = true,
  })
  use({
    n.plenary_nvim,
    module = "plenary",
    requires = "core",
    as = "plenary_nvim",
  })
  use({
    n.impatient_nvim,
    as = "impatient_nvim",
    config = function()
      require("impatient")
    end,
  })
  use({ n.filetype, as = "filetype" })

  use({
    n.onedark_nvim,
    as = "onedark_nvim",
    requires = "core",
    config = function()
      require("core.plugins.others").onedark()
    end,
  })

  use({
    n.nvim_web_devicons,
    as = "nvim_web_devicons",
    after = "nvim_treesitter",
    config = function()
      require("core.plugins.icons")
    end,
    run = ":TSUpdate",
  })

  use({
    n.feline_nvim,
    as = "feline_nvim",
    after = "nvim_web_devicons",
    requires = "nvim_web_devicons",
    config = function()
      require("core.plugins.statusline")
    end,
  })

  use({
    n.bufferline_nvim,
    as = "bufferline_nvim",
    after = "nvim_web_devicons",
    requires = "nvim_web_devicons",
    module = "bufferline",
    config = function()
      require("core.plugins.bufferline")
    end,
  })

  use({
    n.indent_blankline_nvim,
    as = "indent_blankline_nvim",
    event = "BufRead",
    config = function()
      require("core.plugins.others").blankline()
    end,
  })

  use({
    n.nvim_colorizer_lua,
    as = "nvim_colorizer_lua",
    event = "BufRead",
    config = function()
      require("core.plugins.others").colorizer()
    end,
  })

  use({
    n.nvim_treesitter,
    as = "nvim_treesitter",
    event = "BufRead",
    config = function()
      require("core.plugins.treesitter")
    end,
    -- run = ":TSUpdate",
  })

  use({
    n.gitsigns_nvim,
    opt = true,
    as = "gitsigns.nvim",
    module = "gitsigns",
    config = function()
      require("core.plugins.others").gitsigns()
    end,
    setup = function()
      vim.defer_fn(function()
        require("packer").loader("gitsigns.nvim")
      end, 0)
    end,
  })

  use({
    n.nvim_lspconfig,
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
    n.lsp_signature_nvim,
    as = "lsp_signature_nvim",
    after = "nvim_lspconfig",
    config = function()
      require("core.plugins.others").signature()
    end,
  })

  use({
    n.vim_matchup,
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
    n.better_escape_nvim,
    as = "better_escape_nvim",
    event = "InsertCharPre",
    disable = true,
    config = function()
      require("core.plugins.others").better_escape()
    end,
  })

  use({
    n.friendly_snippets,
    as = "friendly_snippets",
    module = "cmp_nvim_lsp",
    event = "InsertCharPre",
  })

  use({
    n.nvim_cmp,
    as = "nvim_cmp",
    after = "friendly_snippets",
    module = "cmp",
    config = function()
      require("core.plugins.cmp")
    end,
  })

  use({
    n.luasnip,
    as = "luasnip",
    after = "friendly_snippets",
    config = function()
      require("core.plugins.others").luasnip()
    end,
  })

  use({
    n.cmp_luasnip,
    as = "cmp_luasnip",
    after = "luasnip",
    config = function()
      require("cmp").register_source("luasnip", require("cmp_luasnip").new())
    end,
  })

  use({
    n.cmp_nvim_lua,
    as = "cmp_nvim_lua",
    after = "cmp_luasnip",
    config = function()
      require("cmp").register_source("nvim_lua", require("cmp_nvim_lua").new())
    end,
  })

  use({
    n.cmp_nvim_lsp,
    as = "cmp_nvim_lsp",
    after = "cmp_nvim_lua",
    module = "cmp_nvim_lsp",
    config = function()
      require("cmp_nvim_lsp").setup()
    end,
  })

  use({
    n.cmp_buffer,
    as = "cmp_buffer",
    after = "nvim_cmp",
    requires = "nvim_cmp",
    module = "cmp_buffer",
    config = function()
      require("cmp").register_source("buffer", require("cmp_buffer").new())
    end,
  })
  use({
    -- "tzachar/cmp-tabnine",
    n.cmp_tabnine,
    as = "cmp_tabnine",
    run = "./install.sh",
    requires = "nvim_cmp",
    after = "nvim_cmp",
    config = function()
      require("cmp_tabnine").setup()
      require("core.plugins.others").cmp_tabnine()
    end,
  })
  use({
    -- "f3fora/cmp-spell",
    n.cmp_spell,
    as = "cmp_spell",
    after = "nvim_cmp",
    requires = "nvim_cmp",
    config = function()
      require("cmp").register_source("spell", require("cmp-spell").new())
    end,
  })
  use({
    n.cmp_path,
    as = "cmp_path",
    after = "cmp_buffer",
    requires = "cmp_buffer",
    config = function()
      require("cmp").register_source("path", require("cmp_path").new())
    end,
  })

  use({
    n.nvim_autopairs,
    as = "nvim_autopairs",
    config = function()
      require("core.plugins.others").autopairs()
    end,
  })

  use({
    n.alpha_nvim,
    as = "alpha_nvim",
    config = function()
      require("core.plugins.alpha")
    end,
  })

  use({
    n.comment_nvim,
    as = "comment_nvim",
    requires = "core",
    module = "Comment",
    keys = { "gcc", "gc" },
    config = function()
      require("core.plugins.others").comment()
    end,
  })

  use({
    n.nvim_tree_lua,
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
    n.telescope_nvim,
    as = "telescope_nvim",
    module = "telescope",
    cmd = "Telescope",
    config = function()
      require("core.plugins.telescope")
    end,
  })
  use({
    -- "jose-elias-alvarez/null-ls.nvim",
    n.null_ls,
    as = "null_ls",
    after = "nvim_lspconfig",
    config = function()
      require("core.plugins.others").null_ls()
    end,
  })
  use({
    -- "liuchengxu/vim-which-key",
    n.which_key,
    as = "which_key",
    requires = "onedark_nvim",
    module = "onedark",
    config = function()
      require("core.plugins.others").which_key()
    end,
  })
  use({
    -- "simrat39/symbols-outline.nvim",
    n.symbols_outline,
    as = "symbols_outline",
    module = "symbols-outline",
    cmd = {
      "SymbolsOutline",
      "SymbolsOutlineOpen",
      "SymbolsOutlineClose",
    },
    setup = "require('core.plugins.others').symbols_outline_pre()",
    config = function()
      require("core.plugins.others").symbols_outline()
    end,
  })
  use({
    -- "simrat39/rust-tools.nvim",
    n.rust_tools,
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
    ft = { "rs", "toml" },
  })
  use({
    -- "davidgranstrom/nvim-markdown-preview",
    n.markdown_preview,
    as = "markdown_preview",
    ft = "markdown",
    cmd = "MarkdownPreview",
  })
  use({
    -- "chentau/marks.nvim",
    n.marks,
    as = "marks",
    requires = "core",
    config = function()
      require("core.plugins.others").marks()
    end,
  })
  use({
    -- "Pocco81/AutoSave.nvim",
    n.auto_save,
    as = "auto_save",
    requires = "core",
    config = function()
      require("core.plugins.others").auto_save()
    end,
  })
  use({
    -- "mbbill/undotree",
    n.undotree,
    requires = "core",
    as = "undotree",
    cmd = "UndotreeToggle",
  })
  use({
    -- "p00f/nvim-ts-rainbow",
    n.ts_rainbow,
    as = "ts_rainbow",
    after = "nvim_treesitter",
    requires = "nvim_treesitter",
    config = function()
      require("core.plugins.others").ts_rainbow()
    end,
  })
  use({
    -- "sindrets/diffview.nvim",
    n.diffview,
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
    -- 'numToStr/Navigator.nvim',
    n.navigator,
    as = "navigator",
    requires = "core",
    config = function()
      require("core.plugins.others").navigator()
    end,
  })
  use({
    -- "RRethy/vim-illuminate",
    n.illuminate,
    as = "illuminate",
    module = "illuminate",
    after = "nvim_treesitter",
    requires = "onedark_nvim",
  })
  use({
    -- "kevinhwang91/nvim-hlslens",
    n.hlslens,
    as = "hlslens",
    after = "nvim_treesitter",
    config = function()
      require("core.plugins.others").hlslens()
    end,
  })
  use({
    n.pretty_fold,
    as = "pretty_fold",
    after = "nvim_treesitter",
    config = function()
      require("core.plugins.others").pretty_fold()
    end,
  })
  use({
    n.ts_autotag,
    as = "ts_autotag",
    after = "nvim_treesitter",
    requires = "nvim_treesitter",
    config = function()
      require("core.plugins.others").ts_autotag()
    end,
  })
  use({
    n.lspsaga,
    as = "lspsaga",
    after = "cmp_nvim_lsp",
    config = function()
      require("core.plugins.others").lspsaga()
    end,
    cmd = "Lspsaga",
    module = "lspsaga",
  })
  use({
    n.dap,
    as = "dap",
    module = "dap",
  })
  use({
    n.dap_ui,
    as = "dap_ui",
    after = "dap",
    module = "dapui",
    config = function()
      require("core.plugins.others").dap_ui()
    end,
  })
  use({
    n.dap_virtual_text,
    as = "dap_virtual_text",
    after = "dap",
    config = function()
      require("core.plugins.others").dap_virtual_text()
    end,
  })
  use({
    n.fterm,
    as = "fterm",
    module = "FTerm",
    config = function()
      require("core.plugins.others").fterm()
    end,
  })
  use({
    n.mini,
    as = "mini",
    requires = "core",
    config = function()
      require("core.plugins.others").mini()
    end,
  })
  use({
    n.session_manager,
    as = "session_manager",
    config = function()
      require("core.plugins.others").session_manager()
    end,
  })
  use({
    n.firenvim,
    as = "firenvim",
    requires = "core",
    run = function()
      vim.fn["firenvim#install"](0)
    end,
  })
end)
vim.cmd([[PackerInstall]])
packer.compile(n.compile_path)

local M = {}
M.packer = packer
M.status = packer.status
return M
