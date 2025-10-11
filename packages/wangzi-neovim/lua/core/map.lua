local keymap = vim.keymap.set
local function map(mod, key, exec, opt)
    vim.keymap.set(mod, key, exec, opt)
end

local wk = require("which-key")

wk.add({
    { '<leader>"',     "<cmd>terminal<CR>",                                             desc = "Terminal" },
    { "<leader><Tab>", "<cmd>b#<CR>",                                                   desc = "Switch Tab" },
    { "<leader>Q",     "<cmd>quitall<CR>",                                              desc = "quit" },
    { "<leader>bd",    "<cmd>bd<CR>",                                                   desc = "Delete Buffer" },
    { "<leader>bo",    'let mycurf=expand("<cfile>")<cr><c-w> w :execute("e ".mycurf)', desc = "Open Path" },
    { "<leader>by",    "<cmd>%y+ <CR>",                                                 desc = "Copy" },
    { "<leader>w",     group = "Window / Workspace" },
    { "<leader>ws",    "<cmd>sp<CR>",                                                   desc = "Split" },
    { "<leader>wv",    "<cmd>vs<CR>",                                                   desc = "Split Vertical" },
    { "]B",            "<cmd>bn<CR>",                                                   desc = "Next Buffer" },
    { "[B",            "<cmd>bp<CR>",                                                   desc = "Pervious Buffer" },
    { "<Tab>",         "<cmd>bn<CR>",                                                   desc = "Next Position" },
    { "<S-Tab>",       "<cmd>bp<CR>",                                                   desc = "Previous Position" },
    {
        "<leader>z",
        function()
            require("lazy").profile()
        end,
        desc = "Packages"
    },
    {
        "J",
        function()
            local filetype = vim.bo.filetype
            if filetype == "rust" then
                vim.cmd [[RustLsp joinLines]]
            else
                vim.cmd [[join]]
            end
        end,
        desc = "hover"
    },
    {
        "<C-k>",
        function()
            local dap = require("dap")
            if dap.status() ~= "" then
                require("dapui").eval(nil, { enter = true })
            end
        end,
        desc = "dap hover"
    },
    {
        "<A-k>",
        function()
            local dap = require("dap")
            if dap.status() ~= "" then
                require("dapui").eval(nil, { enter = true })
            end
        end,
        desc = "dap hover"
    },
    {
        "K",
        function()
            local filename = vim.fn.expand("%:t")
            local filetype = vim.bo.filetype
            if filename == "Cargo.toml" then
                require("crates").show_popup()
            elseif filetype == "rust" then
                vim.cmd [[RustLsp hover actions]]
            else
                vim.lsp.buf.hover()
            end
        end,
        desc = "hover"
    },
    { "\\P", '"+P',            desc = "Prev Paste+" },
    { "\\p", '"+p',            desc = "Paste+" },
    { "\\q", "<cmd>close<CR>", desc = "Close" },
    { "\\y", '"+y',            desc = "Copy" },
})

wk.add({
    mode = { "v" },
    { "<leader>p", desc = "<cmd>diffput<CR>" },
    { "\\P",       '"+P',                    desc = "Prev Paste+" },
    { "\\p",       '"+p',                    desc = "Paste+" },
    { "\\q",       "<cmd>close<CR>",         desc = "Close" },
    { "\\y",       '"+y',                    desc = "Copy" },
})

for i = 0, 9 do
    map("n", "<leader>w" .. i, "<cmd>" .. i .. "wincmd w<CR>", { desc = "Goto Window " .. i })
    map({ "n", "i" }, "<C-w>" .. i, "<cmd>" .. i .. "wincmd w<CR>", { desc = "Goto Window " .. i })
end

map("n", "\\q", "<cmd>close<CR>", { desc = "Close" })
map("n", "<leader>ws", "<cmd>sp<CR>", { desc = "Split" })
map("n", "<leader>wv", "<cmd>vs<CR>", { desc = "Vertical Split" })
map("n", "<leader>q", "<cmd>close<CR>", { desc = "Close" })
map("n", "<C-Q>", "<cmd>close<CR>", { desc = "Close" })
map("n", "<C-up>", "<cmd>res +1<CR>")
map("n", "<C-down>", "<cmd>res -1<CR>")
map("n", "<C-left>", "<cmd>vertical resize-1<CR>")
map("n", "<C-right>", "<cmd>vertical resize+1<CR>")
map("n", "<A-v>", "<C-v>")

map("i", "<C-h>", "<Left>")
map("i", "<C-e>", "<End>")
map("i", "<C-l>", "<Right>")
map("i", "<C-k>", "<Up>")
map("i", "<C-j>", "<Down>")
map("i", "<C-a>", "<ESC>^i")

map("t", "\\<ESC>", "<C-\\><C-n>")
map("t", "\\q", "<C-\\><C-n><cmd>close<CR>")
map("t", "<C-\\>", "<cmd>close<CR>")
map("n", "Q", "<Nop>")

map("n", "<C-t>", "<cmd>enew <CR>") -- new buffer
