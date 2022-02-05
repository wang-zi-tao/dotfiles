-- Please check NvChad docs if you're totally new to nvchad + dont know lua!!
-- This is an example init file in /lua/custom/
-- this init.lua can load stuffs etc too so treat it like your ~/.config/nvim/
local g = vim.g
local opt = vim.opt

-- vim.opt.spell = true
-- vim.opt.spelllang = { "en_us" }
opt.lazyredraw = true
opt.autowrite = true
opt.relativenumber = true

vim.cmd [[
  augroup LspFormatting
      autocmd! * <buffer>
      autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()
  augroup END
]]
-- MAPPINGS
local map = require("core.utils").map

map("n", "<leader>fk", ":Telescope keymaps<CR>")
map("n", "<leader>fr", ":Telescope registers<CR>")
map("n", "<leader>fo", ":Telescope lsp_workspace_symbols<CR>")
map("n", "<leader>fi", ":Telescope lsp_references<CR>")
map("n", "<leader>fa", ":Telescope lsp_code_actions<CR>")
map("n", "<leader>q", ":close<CR>")
map("n", "<leader>W", ":wa<CR>:SessionSave<CR>")
map("n", "<leader>Q", ":SessionSave<CR>:quitall<CR>")
map("n", "<leader>pc", ":PackerCompile<CR>")
map("n", "<leader>pi", ":PackerInstall<CR>")

map("n", "<leader>o", ":SymbolsOutline<CR>") 
map("n", "<leader>u", ":UndotreeToggle<CR>")
map("n", "<leader>lf", ":lua vim.lsp.buf.formatting_sync()<CR>")
map("n", "<leader><Tab>", ":b#<CR>")

map("n", "<leader>ws", ":sp<CR>")
map("n", "<leader>wv", ":vs<CR>")

map("n", "\\q", ":close<CR>")
map("n", "\\y", '"+y')
map("n", "\\p", '"+p')
map("n", "\\P", '"+P')
map("v", "\\y", '"+y')
map("v", "\\p", '"+p')
map("v", "\\P", '"+P')



-- NOTE: the 4th argument in the map function is be a table i.e options but its most likely un-needed so dont worry about it
--
