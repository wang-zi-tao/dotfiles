local keymap = vim.keymap.set
local function map(mod, key, exec, opt)
    vim.keymap.set(mod, key, exec, opt)
end

local wk = require("which-key")

wk.register({
    ["<leader>"] = {
        q = { "<cmd>close<CR>", "close" },
        Q = { "<cmd>quitall<CR>", "quit" },
        ['"'] = {
            "<cmd>terminal<CR>", "Terminal"
        },
        g = {
            a = { ":!git add .<CR>", "Git Add" },
            c = { ":terminal git commit<CR>", "Git Commit" },
            p = { ":!git push<CR>", "Git Push" },
            P = { ":!git pull<CR>", "Git Pull" },
        },
        b = {
            d = {
                "<cmd>bd<CR>",
                "Delete Buffer",
            },
            o = {
                [[let mycurf=expand("<cfile>")<cr><c-w> w :execute("e ".mycurf)]],
                "Open Path",
            },
            y = { "<cmd>%y+ <CR>", "Copy" },
        },
        ["<Tab>"] = {
            "<cmd>b#<CR>", "Switch Tab"
        },
        w = {
            name = "Window / Workspace",
            s = { "<cmd>sp<CR>", "Split" },
            v = { "<cmd>vs<CR>", "Split Vertical" },
        },
        z = {
            function()
                require("lazy").profile()
            end, "Packages"
        }
    },
    ["\\"] = {
        q = { "<cmd>close<CR>", "Close" },
        y = { '"+y', "Copy" },
        p = { '"+p', "Paste+" },
        P = { '"+P', "Prev Paste+" },
    },
})
wk.register({
    ["\\"] = {
        q = { "<cmd>close<CR>", "Close" },
        y = { '"+y', "Copy" },
        p = { '"+p', "Paste+" },
        P = { '"+P', "Prev Paste+" },
    },
    ["<leader>"] = {
        p = { "<cmd>diffput<CR>" },
    }
}, { mode = "v" })

map("n", "\\q", "<cmd>close<CR>", {desc="Close"})
map("n", "<leader>ws", "<cmd>sp<CR>", {desc="Split"})
map("n", "<leader>wv", "<cmd>vs<CR>", {desc="Vertical Split"})
map("n", "<leader>q", "<cmd>close<CR>", {desc="Close"})
map("n", "<C-up>", "<cmd>res +1<CR>")
map("n", "<C-down>", "<cmd>res -1<CR>")
map("n", "<C-left>", "<cmd>vertical resize-1<CR>")
map("n", "<C-right>", "<cmd>vertical resize+1<CR>")

map("i", "<C-h>", "<Left>")
map("i", "<C-e>", "<End>")
map("i", "<C-l>", "<Right>")
map("i", "<C-k>", "<Up>")
map("i", "<C-j>", "<Down>")
map("i", "<C-a>", "<ESC>^i")

map("t", "\\<ESC>", "<C-\\><C-n>")
map("t", "\\q", "<C-\\><C-n><cmd>close<CR>")
map("n", "Q", "<Nop>")

map("n", "<C-t>", "<cmd>enew <CR>") -- new buffer

