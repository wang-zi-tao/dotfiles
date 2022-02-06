-- This is an example chadrc file , its supposed to be placed in /lua/custom/

local M = {}

-- make sure you maintain the structure of `core/default_config.lua` here,
-- example of changing theme:

M.ui = {
   -- theme = "onedark",
}
M.options = {
     nvChad = {
      copy_cut = false, -- copy cut text ( x key ), visual and normal mode
      copy_del = true, -- copy deleted text ( dd key ), visual and normal mode
      insert_nav = true, -- navigation in insertmode
      window_nav = true,
   },
}

-- NOTE: we heavily suggest using Packer's lazy loading (with the 'event','cmd' fields)
-- see: https://github.com/wbthomason/packer.nvim
-- https://nvchad.github.io/config/walkthrough
local userPlugins = require "custom.plugins"
M.plugins = {
   install = userPlugins,
   status = {
      dashboard = true,
      colorizer = true,
      snippets = true,
   },
   options = {
      lspconfig = {
         setup_lspconf = "custom.plugins.lspconfig",
      },
      cmp = {
         lazy_load = true,
         sources = {
            { name = "nvim_lsp" },
            { name = "nvim_lua" },
            { name = "cmp_tabnine" },
            { name = "spell" },
            { name = "path" },
            { name = "buffer" },
         },
      },
      statusline = {
         hidden = {
            "help",
            "dashboard",
            "NvimTree",
            "terminal",
            "Undotree",
            "SymbolsOutline",
         },
         shortline = true,
         style = "default", -- default, round , slant , block , arrow
      },
      treesitter = {
        ensure_installed = {
           "lua",
           "vim",
           "nix",
           "rust",
           "c",
           "cpp",
           "typescript",
           "python",
           "sql",
           "llvm",
           "go",
        },
      },
   },
   default_plugin_config_replace = {
      dashboard = "custom.plugins.dashboard",
      nvim_tree = "custom.plugins.nvimtree",
      nvim_cmp = "custom.plugins.cmp",
   },
   default_plugin_remove = {
    -- "lewis6991/impatient.nvim",
   },
}
M.mappings = {
   terminal = {
      new_horizontal = "<leader>'",
      esc_termmode = "<ESC>",
   },
}
M.mappings.plugins = {
   comment = {
      toggle = "<leader>/",
   },

   dashboard = {
      bookmarks = "<leader>bm",
      new_file = "<leader>fn", -- basically create a new buffer
      open = "<leader>db", -- open dashboard
      session_load = "<leader>l",
      session_save = "<leader>s",
   },
   nvimtree = {
      toggle = "<leader>e",
      focus = "<leader>we",
   },
}
return M
