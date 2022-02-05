-- Example plugins file!
-- (suggestion) -> lua/custom/plugins/init.lua or anywhere in custom dir
local n = require "custom.nix-plugins"

return {
   {
      -- "jose-elias-alvarez/null-ls.nvim",
      n.null_ls,
      as = "null-ls.nvim",
      after = "nvim-lspconfig",
      config = "require('custom.plugins.plugin-config').null_ls()",
   },
   {
     -- "liuchengxu/vim-which-key",
     n.which_key,
     as = "vim-which-key",
     config = "require('custom.plugins.plugin-config').which_key()",
   },
   {
      -- "simrat39/symbols-outline.nvim",
      n.symbols_outline,
      as = "symbols-outline.nvim",
      cmd = {
         "SymbolsOutline",
         "SymbolsOutlineOpen",
         "SymbolsOutlineClose",
      },
   },
   {
      -- "simrat39/rust-tools.nvim",
      n.rust_tools,
      as = "rust-tools.nvim",
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
   },
   {
      -- "tzachar/cmp-tabnine",
      n.cmp_tabnine,
      as = "cmp-tabnine",
      run = "./install.sh",
      requires = "hrsh7th/nvim-cmp",
      after = "cmp-nvim-lsp",
      config = "require('custom.plugins.plugin-config').cmp_tabnine()",
   },
   {
      -- "f3fora/cmp-spell",
      n.cmp_spell,
      as = "cmp-spell",
      after = "cmp-nvim-lsp",
   },
   {
      -- "davidgranstrom/nvim-markdown-preview",
      n.markwown_preview,
      as = "nvim-markdown-preview",
      ft = "markdown",
      cmd = "MarkdownPreview",
   },
   {
      -- "chentau/marks.nvim",
      n.marks,
      as = "marks.nvim",
      config = "require('custom.plugins.plugin-config').marks()",
   },
   {
      -- "tpope/vim-surround",
      n.vim_surround,
      as = "vim-surround",
   },
   {
      -- "Pocco81/AutoSave.nvim",
      n.auto_save,
      as = "AutoSave.nvim",
      config = "require('custom.plugins.plugin-config').auto_save()",
   },
   {
     -- "mbbill/undotree",
     n.undotree,
     as = "undotree",
     cmd = "UndotreeToggle"
   },
   {
      -- "p00f/nvim-ts-rainbow",
      n.ts_rainbow,
      as = "ts-rainbow",
      after = "nvim-treesitter",
      event = "BufRead",
      config = "require('custom.plugins.plugin-config').ts_rainbow()",
   },
   {
     -- "tpope/vim-repeat"
     n.vim_repeat,
     as = "vim-repeat",
   },
   {
      -- "sindrets/diffview.nvim",
      n.diffview,
      as = "diffview.nvim",
      requires = "nvim-lua/plenary.nvim",
      cmd = {
         "DiffviewOpen",
         "DiffviewClose",
         "DiffviewClose",
         "DiffviewFocusFiles",
         "DiffviewRefresh",
      },
   },
   {
    -- 'numToStr/Navigator.nvim',
    n.navigator,
    as = "Navigator",
    config = "require('custom.plugins.plugin-config').navigator()",
   },
   {
     -- "RRethy/vim-illuminate",
     n.illuminate,
     as = "vim-illuminate",
   },
   {
     -- "kevinhwang91/nvim-hlslens",
     n.hlslens,
     as = "nvim-hlslens",
     event = "BufRead",
     config = "require('custom.plugins.plugin-config').hlslens()",
   },
}
