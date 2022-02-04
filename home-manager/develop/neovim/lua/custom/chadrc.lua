-- This is an example chadrc file , its supposed to be placed in /lua/custom/

local M = {}

-- make sure you maintain the structure of `core/default_config.lua` here,
-- example of changing theme:

M.ui = {
   theme = "onedark",
}

-- NOTE: we heavily suggest using Packer's lazy loading (with the 'event','cmd' fields)
-- see: https://github.com/wbthomason/packer.nvim
-- https://nvchad.github.io/config/walkthrough
local userPlugins = require "custom.plugins"
M.plugins = {
   install = userPlugins,
   status = {
      dashboard = true,
      -- colorizer = true,
      -- snippets = true,
   },
   options = {
      lspconfig = {
         setup_lspconf = "custom.plugins.lspconfig",
      },
      nvimtree = {
         view = {
            auto_resize = true,
         },
         git = {
            enable = true,
            ignore = true,
            timeout = 500,
         },
         diagnostics = {
            enable = true,
         },
      },
   },
   default_plugin_config_replace = {
      dashboard = "custom.plugins.dashboard",
      nvimtree = "custom.plugins.nvimtree",
   },
}
M.mappings = {
   terminal = {
      new_horizontal = "<leader>'",
      esc_termmode = "<leader>'",
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
