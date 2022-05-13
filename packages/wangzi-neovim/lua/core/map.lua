local function map(mod, key, exec, opt)
  vim.keymap.set(mod, key, exec)
end
local wk = require("which-key")
local function telescope()
  return require("telescope.builtin")
end
wk.register({
  q = { "<cmd>close<CR>", "close" },
  w = { "<cmd>wa<CR>", "save" },
  Q = { "<cmd>quitall<CR>", "quit" },
  ["'"] = {
    function()
      require("FTerm").toggle()
    end,
    "Terminal",
  },
  f = {
    name = "Telescope",
    k = {
      function()
        telescope().keymaps()
      end,
      "Keymaps",
    },
    w = {
      function()
        telescope().live_grep()
      end,
      "Grep",
    },
    f = {
      function()
        telescope().find_files()
      end,
      "Files",
    },
    r = {
      function()
        telescope().registers()
      end,
      "Registers",
    },
    o = {
      function()
        telescope().lsp_workspace_symbols()
      end,
      "WorkspaceSymbols",
    },
    a = {
      function()
        vim.lsp.buf.code_action()
      end,
      "Actions",
    },
    i = {
      function()
        telescope().lsp_references()
      end,
      "LSP Reference",
    },
    b = {
      function()
        telescope().buffers()
      end,
      "Buffers",
    },
    s = {
      function()
        telescope().git_status()
      end,
      "GitStatus",
    },
    c = {
      function()
        telescope().git_commits()
      end,
      "GitCommits",
    },
    m = {
      function()
        require("telescope.builtin").marks()
      end,
      "Marks",
    },
    d = {
      function()
        require("telescope.builtin").lsp_document_symbols()
      end,
      "Lsp_document_symbols",
    },
  },
  g = {
    name = "Diff",
    r = {
      function()
        telescope().lsp_references()
      end,
      "LspReferences",
    },
    d = {
      function()
        require("diffview").open()
      end,
      "Open Diff",
    },
    D = {
      function()
        require("diffview").close()
      end,
      "Close Diff",
    },
  },
  l = {
    name = "LSP",
    a = {
      function()
        require("lspsaga.codeaction").code_actions()
      end,
      "CodeActions",
    },
    r = {
      function()
        require("lspsaga.rename").rename()
      end,
      "Rename",
    },
    d = {
      function()
        require("lspsaga.provider").preview_definition()
      end,
      "PreviewDefinition",
    },
  },
  p = { name = "packer", s = {
    function()
      require("core.pack").status()
    end,
    "PackerStatus",
  } },
  d = {
    name = "debug",
    b = {
      function()
        require("dap").toggle_breakpoint()
      end,
      "BreakPoint",
    },
    B = {
      function()
        require("dap").clear_breakpoint()
      end,
      "Clear BreakPoint",
    },
    p = {
      function()
        require("dap").pause()
      end,
      "Pause",
    },
    c = {
      function()
        require("dap").continue()
      end,
      "Continue",
    },
    C = {
      function()
        require("dap").close()
      end,
      "Close",
    },
    o = {
      function()
        require("dap").step_over()
      end,
      "Step Over",
    },
    i = {
      function()
        require("dap").step_into()
      end,
      "Step Into",
    },
    l = {
      function()
        require("dap").run_to_cursor()
      end,
      "Run to Corsor",
    },
    r = {
      function()
        require("dap").run()
      end,
      "Run",
    },
    R = {
      function()
        require("dap").repl.open()
      end,
      "Repl",
    },
    u = {
      name = "UI",
      a = {
        function()
          require("telescope").extensions.dap.commands({})
        end,
        "Commands",
      },
      o = {
        function()
          require("dapui").open()
        end,
        "Open",
      },
      c = {
        function()
          require("dapui").close()
        end,
        "Close",
      },
      C = {
        function()
          require("telescope").extensions.dap.configurations({})
        end,
        "Configurations",
      },
    },
  },
  h = { name = "hunk" },
  b = { name = "buffer" },
}, { prefix = "<leader>" })
wk.register({
  g = {
    d = {
      function()
        require("telescope.builtin").lsp_definitions()
      end,
      "Define",
    },
    i = {
      function()
        require("telescope.builtin").lsp_implementations()
      end,
      "Implementation",
    },
    D = {
      function()
        require("telescope.builtin").lsp_type_definitions()
      end,
      "TypeDefinition",
    },
  },
})
map("n", "\\'", function()
  require("FTerm").toggle()
end)
map("n", "\\9", function()
  require("dap").toggle_breakpoint()
end)
map("n", "<F9>", function()
  require("dap").toggle_breakpoint()
end)
map("n", "\\%", function()
  require("dap").close()
end)
map("n", "<S-F5>", function()
  require("dap").close()
end)
map("n", "\\-", function()
  require("dap").step_over()
end)
map("n", "<F11>", function()
  require("dap").step_over()
end)
map("n", "\\0", function()
  require("dap").step_into()
end)
map("n", "<F10>", function()
  require("dap").step_into()
end)
map("n", "\\5", function()
  require("dap").run()
end)
map("n", "<F5>", function()
  require("dap").run()
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
