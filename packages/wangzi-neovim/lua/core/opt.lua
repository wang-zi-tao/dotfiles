local opt = vim.opt
local g = vim.g
local wo = vim.wo

g.mapleader = " "
opt.swapfile = false
opt.autowrite = true
opt.autoread = true
opt.clipboard = "unnamedplus"
opt.cmdheight = 1
opt.cul = true
opt.expandtab = true
opt.fillchars = { eob = " " }
opt.foldenable = false
opt.foldmethod = "indent"
opt.foldnestmax = 8
opt.hidden = true
opt.ignorecase = true
opt.lazyredraw = true
opt.mouse = "a"
opt.number = true
opt.numberwidth = 2
opt.relativenumber = true
opt.ruler = false
opt.shiftwidth = 2
opt.shortmess:append("sI")
opt.signcolumn = "yes"
opt.smartcase = true
opt.smartindent = true
opt.splitbelow = true
opt.splitright = true
opt.tabstop = 8
opt.termguicolors = true
opt.timeoutlen = 400
opt.title = true
opt.undofile = true
opt.updatetime = 250
opt.whichwrap:append("<>[]hl")
if (vim.env.WSL_DISTRO_NAME) then
  g.clipboard = {
    name = "wsl",
    copy = {
      ["+"] = "win32yank.exe -i",
      ["*"] = "win32yank.exe -i",
    },
    paste = {
      ["+"] = "win32yank.exe -o",
      ["*"] = "win32yank.exe -o",
    },
    cache_enabled = 1,
  }
else
  g.clipboard = {
    name = "xclip-xfce4-clipman",
    copy = {
      ["+"] = "xclip -selection clipboard",
      ["*"] = "xclip -selection clipboard",
    },
    paste = {
      ["+"] = "xclip -selection clipboard -o",
      ["*"] = "xclip -selection clipboard -o",
    },
    cache_enabled = 1,
  }
end
local disabled_built_ins = {
  "2html_plugin",
  "filetype",
  "getscript",
  "getscriptPlugin",
  "gzip",
  "logipat",
  "netrw",
  "netrwPlugin",
  "netrwSettings",
  "netrwFileHandlers",
  "matchit",
  "tar",
  "tarPlugin",
  "rrhelper",
  "spellfile_plugin",
  "vimball",
  "vimballPlugin",
  "zip",
  "zipPlugin",
}
for _, plugin in pairs(disabled_built_ins) do
  vim.g["loaded_" .. plugin] = 1
end
--Defer loading shada until after startup_
vim.opt.shadafile = "NONE"
vim.schedule(function()
  vim.cmd([[ silent! rsh ]])
end)
vim.cmd([[ au TermOpen term://* setlocal nonumber norelativenumber | setfiletype terminal ]])
-- vim.cmd([[ autocmd BufEnter,BufRead,BufWinEnter,FileType,WinEnter * lua require("core.utils").hide_statusline() ]])
vim.cmd([[let mapleader = "\<space>"]])
