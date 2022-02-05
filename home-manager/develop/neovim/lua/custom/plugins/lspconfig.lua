require("packer").loader("vim-illuminate")
local illuminate=require("illuminate")
local M = {}
M.setup_lsp = function(attach, capabilities)
   local lspconfig = require "lspconfig"
   -- lspservers with default config
   local servers = {
      "clangd",
      "sumneko_lua",
      "vimls",
      "pyright",
      "rust_analyzer",
      "gopls",
      "html",
      "cssls",
      "tsserver",
      -- 'jsonls',
      "volar",
      "tailwindcss",
      "texlab",
      "yamlls",
      "cmake",
      "rnix",
   }
   for _, lsp in ipairs(servers) do
      lspconfig[lsp].setup {
         on_attach = function(client)
           attach(client)
           illuminate.on_attach(client)
         end,
         capabilities = capabilities,
         flags = {
            debounce_text_changes = 150,
         },
      }
   end
end
return M
