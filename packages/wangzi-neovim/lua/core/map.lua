local function map(mod, key, exec, opt)
  vim.keymap.set(mod, key, exec)
end
map("n", "<leader>fk", function()
  require("telescope.builtin").keymaps()
end)
map("n", "<leader>fw", function()
  require("telescope.builtin").live_grep()
end)
map("n", "<leader>ff", function()
  require("telescope.builtin").find_files()
end)
map("n", "<leader>fr", function()
  require("telescope.builtin").registers()
end)
map("n", "<leader>fo", function()
  require("telescope.builtin").lsp_workspace_symbols()
end)
map("n", "<leader>fi", function()
  require("telescope.builtin").lsp_references()
end)
map("n", "<leader>fa", function()
  require("telescope.builtin").lsp_code_actions()
end)
map("n", "<leader>fb", function()
  require("telescope.builtin").buffers()
end)
map("n", "<leader>fs", function()
  require("telescope.builtin").git_status()
end)
map("n", "<leader>fc", function()
  require("telescope.builtin").git_commits()
end)
map("n", "<leader>fm", function()
  require("telescope.builtin").marks()
end)
map("n", "<leader>fd", function()
  require("telescope.builtin").lsp_document_symbols()
end)
map("n", "gd", function()
  require("telescope.builtin").lsp_definitions()
end)
map("n", "gi", function()
  require("telescope.builtin").lsp_implementations()
end)
map("n", "D", function()
  require("telescope.builtin").lsp_type_definitions()
end)
map("n", "<leader>gr", function()
  require("telescope.builtin").lsp_references()
end)
map("n", "<leader>la", function()
  require("lspsaga.codeaction").code_action()
end)
map("n", "<leader>lr", function()
  require("lspsaga.rename").rename()
end)
map("n", "<leader>ld", function()
  require("lspsaga.provider").preview_definition()
end)
map("n", [[<leader>']], function()
  require("FTerm").toggle()
end)
map("n", "\\'", function()
  require("FTerm").toggle()
end)
map("n", [[<C-'>]], function()
  require("FTerm").toggle()
end)
map("n", "<leader>q", "<cmd>close<CR>")
map("n", "<leader>w", "<cmd>wa<CR>")
map("n", "<leader>Q", "<cmd>quitall<CR>")
map("n", "<leader>ps", function()
  require("core.pack").status()
end)

map("n", "<leader>db", function()
  require("dap").toggle_breakpoint()
end)
map("n", "<leader>dB", function()
  require("dap").clear_breakpoint()
end)
map("n", "\\9", function()
  require("dap").toggle_breakpoint()
end)
map("n", "<F9>", function()
  require("dap").toggle_breakpoint()
end)
map("n", "<leader>dc", function()
  require("dap").continue()
end)
map("n", "<leader>dC", function()
  require("dap").close()
end)
map("n", "\\%", function()
  require("dap").close()
end)
map("n", "<S-F5>", function()
  require("dap").close()
end)
map("n", "<leader>do", function()
  require("dap").step_over()
end)
map("n", "\\-", function()
  require("dap").step_over()
end)
map("n", "<F11>", function()
  require("dap").step_over()
end)
map("n", "<leader>di", function()
  require("dap").step_into()
end)
map("n", "\\0", function()
  require("dap").step_into()
end)
map("n", "<F10>", function()
  require("dap").step_into()
end)
map("n", "<leader>dr", function()
  require("dap").run()
end)
map("n", "\\5", function()
  require("dap").run()
end)
map("n", "<F5>", function()
  require("dap").run()
end)
map("n", "<leader>dR", function()
  require("dap").repl.open()
end)
map("n", "<leader>duo", function()
  require("dapui").open()
end)
map("n", "<leader>duc", function()
  require("dapui").close()
end)

map("n", "<leader>gd", function()
  require("diffview").open()
end)
map("n", "<leader>gD", function()
  require("diffview").close()
end)

map("n", "<leader>hs", "<cmd>Gitsigns stage_hunk<CR>")
map("v", "<leader>hs", "<cmd>Gitsigns stage_hunk<CR>")
map("n", "<leader>hr", "<cmd>Gitsigns reset_hunk<CR>")
map("v", "<leader>hr", "<cmd>Gitsigns reset_hunk<CR>")
map("n", "<leader>hp", "<cmd>Gitsigns preview_hunk<CR>")
map("n", "<leader>hu", "<cmd>Gitsigns undo_stage_hunk<CR>")
map("n", "<leader>bs", "<cmd>Gitsigns stage_buffer<CR>")
map("n", "<leader>br", "<cmd>Gitsigns reset_buffer<CR>")
map("n", "<leader>hb", "<cmd>Gitsigns reset_buffer<CR>")
map("n", "<leader>tb", "<cmd>Gitsigns toggle_current_line_blame<CR>")
map("n", "<leader>hd", "<cmd>Gitsigns diffthis<CR>")
map("n", "<leader>hD", "<cmd>Gitsigns diffthis<CR>")
map("n", "<leader>td", "<cmd>Gitsigns toggle_deleted<CR>")
map("o", "ih", "<cmd><C-U>Gitsigns select_hunk<CR>")
map("x", "ih", "<cmd><C-U>Gitsigns select_hunk<CR>")

map("n", "<leader>e", function()
  require("nvim-tree").toggle()
end)
map("n", "<leader>we", function()
  require("nvim-tree").focus()
end)
map("n", "<leader>o", function()
  require("symbols-outline").toggle_outline()
end)
map("n", "<leader>u", "<cmd>UndotreeToggle<CR>")
map("n", "<leader><Tab>", "<cmd>b#<CR>")

map("n", "<leader>ws", "<cmd>sp<CR>")
map("n", "<leader>wv", "<cmd>vs<CR>")

map("n", "[d", function()
  vim.diagnostic.goto_prev()
end)
map("n", "]d", function()
  vim.diagnostic.goto_next()
end)
map("n", "K", function()
  vim.lsp.buf.hover()
end)
map("n", "ge", function()
  vim.diagnostic.open_float()
end)

map("n", "\\q", "<cmd>close<CR>")
map("n", "\\y", '"+y')
map("n", "\\p", '"+p')
map("n", "\\P", '"+P')
map("v", "\\y", '"+y')
map("v", "\\p", '"+p')
map("v", "\\P", '"+P')

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

map("n", "<C-h>", "<C-w>h")
map("n", "<C-l>", "<C-w>l")
map("n", "<C-k>", "<C-w>k")
map("n", "<C-j>", "<C-w>j")

map("t", "<ESC>", "<C-\\><C-n>")
map("n", "Q", "<Nop>")

map("n", "<leader>by", "<cmd>%y+ <CR>") -- copy whole file content
map("n", "<C-t>", "<cmd>enew <CR>") -- new buffer

map("n", "<Tab>", function()
  require("bufferline").cycle(1)
end)
map("n", "<S-Tab>", function()
  require("bufferline").cycle(-1)
end)