local opt = vim.opt
local g = vim.g
local wo = vim.wo
local o = vim.o

opt.list = true
-- opt.listchars:append "space:⋅"
opt.listchars:append("eol:↴")
opt.listchars:append("tab:  ")
-- opt.listchars:append "multispace:|   "
opt.listchars:append("trail:-")

opt.copyindent = true
opt.preserveindent = true
opt.swapfile = true
opt.backup = true
opt.writebackup = true
opt.backupcopy = "yes"
opt.backupdir = vim.fn.stdpath("state") .. "/backup//"
opt.autowrite = true
opt.autoread = true
opt.clipboard = "unnamedplus"
opt.cmdheight = 1
opt.cul = true
opt.expandtab = true
opt.fillchars = { eob = " " }
opt.foldenable = false
opt.foldmethod = "indent"
opt.foldnestmax = 4
opt.hidden = true
opt.ignorecase = true
opt.lazyredraw = true
opt.mouse = "a"
opt.number = true
opt.numberwidth = 2
opt.relativenumber = true
opt.ruler = false
opt.shortmess:append("sI")
opt.signcolumn = "yes"
opt.smartcase = true
opt.smartindent = true
opt.splitbelow = true
opt.splitright = true
opt.timeoutlen = 400
opt.title = true
opt.termguicolors = true
opt.undofile = true
opt.updatetime = 250
opt.whichwrap:append("<>[]hl")

opt.tabstop = 4
opt.softtabstop = -1
opt.shiftwidth = 0
opt.shiftround = true
opt.autoindent = true

opt.laststatus = 3
opt.splitkeep = "screen"

if vim.env.BOMB or 1 == vim.fn.has("win32") then
    o.bomb = true
end

-- vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter" }, {
--     callback = function()
--     end,
-- })

vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter" }, {
    pattern = { "*.cpp", "*.c", "*.cc", "*.inl", "CMakeLists.txt", "*.h", "*.hpp", "*.pch", "*.def" },
    callback = function()
        opt.tabstop = 4
        opt.softtabstop = 0
        opt.shiftwidth = 4
        vim.cmd([[set softtabstop=0 noexpandtab]])
        opt.shiftround = true
        opt.autoindent = true
        opt.expandtab = false
        -- vim.cmd [[set bomb]]
    end,
})

if 0 == vim.fn.has("win32") then
    if vim.env.WSL_DISTRO_NAME then
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
            name = "xclip",
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
else
    g.terminal_emulator = "powershell"
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
end
local disabled_built_ins = {
    -- "2html_plugin",
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
-- vim.cmd([[ au TermOpen term://* setlocal nonumber norelativenumber | setfiletype terminal ]])
vim.api.nvim_create_autocmd("TermOpen", {
    pattern = "term://*",
    command = "setlocal nonumber norelativenumber | setfiletype terminal",
})
vim.api.nvim_create_autocmd({ "BufWinEnter", "WinEnter" }, {
    pattern = "term://*",
    command = "startinsert",
})
-- vim.cm([[ autocmd BufWinEnter,WinEnter term://* startinsert ]])
-- vim.cmd([[ autocmd BufEnter,BufRead,BufWinEnter,FileType,WinEnter * lua require("core.utils").hide_statusline() ]])
-- vim.cmd([[let mapleader = "\<space>"]])

vim.cmd([[
function! SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc
]])

require("core.theme").define_sign()

vim.diagnostic.config({
    virtual_text = true,
    signs = true,
    update_in_insert = true,
    underline = true,
    severity_sort = false,
    float = {
        border = "rounded",
        source = "always",
        header = "",
        prefix = "",
    },
})

-- vim.api.nvim_create_autocmd("CursorHold", {
--     callback = function()
--         vim.diagnostic.open_float(nil, { focusable = false })
--     end,
-- })
--

if vim.has("win32") == 1 then
    local pid = vim.fn.getpid()
    require("plenary.job"):new({
        command = "wmic",
        args = { "where", "ProcessId=" .. pid, "call", "setpriority", "AboveNormal" }
    }):start()
end
