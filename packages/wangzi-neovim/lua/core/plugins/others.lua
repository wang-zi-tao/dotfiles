local n = require("core.gen")
local M = {}
M.autopairs = function()
  local autopairs = require("nvim-autopairs")
  autopairs.setup({ fast_wrap = {} })
end

M.better_escape = function()
  require("better_escape").setup({})
end

M.blankline = function()
  require("indent_blankline").setup({
    indentLine_enabled = 1,
    char = "▏",
    filetype_exclude = {
      "help",
      "terminal",
      "alpha",
      "packer",
      "lspinfo",
      "TelescopePrompt",
      "TelescopeResults",
      "nvchad_cheatsheet",
      "lsp-installer",
      "",
    },
    buftype_exclude = { "terminal" },
    show_trailing_blankline_indent = false,
    show_first_indent_level = false,
  })
end

M.colorizer = function()
  require("colorizer").setup({
    "*",
  }, {
    RGB = true, -- #RGB hex codes
    RRGGBB = true, -- #RRGGBB hex codes
    names = false, -- "Name" codes like Blue
    RRGGBBAA = false, -- #RRGGBBAA hex codes
    rgb_fn = false, -- CSS rgb() and rgba() functions
    hsl_fn = false, -- CSS hsl() and hsla() functions
    css = true, -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
    css_fn = false, -- Enable all CSS *functions*: rgb_fn, hsl_fn

    -- Available modes: foreground, background
    mode = "background", -- Set the display mode.
  })
  -- vim.cmd("ColorizerReloadAllBuffers")
end

M.comment = function()
  require("nvim_comment").setup({
    -- Linters prefer comment and line to have a space in between markers
    marker_padding = true,
    -- should comment out empty or whitespace only lines
    comment_empty = true,
    -- trim empty comment whitespace
    comment_empty_trim_whitespace = true,
    -- Should key mappings be created
    create_mappings = true,
    -- Normal mode mapping left hand side
    line_mapping = "gcc",
    -- Visual/Operator mapping left hand side
    operator_mapping = "gc",
    -- text object mapping, comment chunk,,
    comment_chunk_text_object = "ic",
    -- Hook function to call before commenting takes place
    hook = nil,
  })
  vim.cmd([[autocmd BufEnter *.cpp,*.h :lua vim.api.nvim_buf_set_option(0, "commentstring", "// %s")]])
  vim.cmd([[autocmd BufEnter *.nix :lua vim.api.nvim_buf_set_option(0, "commentstring", "# %s")]])
end

M.luasnip = function()
  require("luasnip").config.set_config({
    history = true,
    updateevents = "TextChanged,TextChangedI",
  })
  require("luasnip/loaders/from_vscode").load({ paths = {
    "./rust.json",
    "./c++.json",
  } })
  require("luasnip/loaders/from_vscode").load()
end

M.signature = function()
  require("lsp_signature").setup({
    bind = true,
    doc_lines = 0,
    floating_window = true,
    fix_pos = true,
    hint_enable = true,
    hint_prefix = " ",
    hint_scheme = "String",
    hi_parameter = "Search",
    max_height = 22,
    max_width = 120, -- max_width of signature floating_window, line will be wrapped if exceed max_width
    handler_opts = {
      border = "single", -- double, single, shadow, none
    },
    zindex = 200, -- by default it will be on top of all floating windows, set to 50 send it to bottom
    padding = "", -- character to pad on left and right of signature can be ' ', or '|'  etc
  })
end

M.which_key = function()
  require("which-key").setup({})
end
M.symbols_outline = function()
  local colors = require("core.colors").get()
  local blue = colors.blue
  for k, v in pairs(vim.g.symbols_outline.symbols) do
    vim.cmd("hi " .. v.hl .. " guifg=" .. blue)
  end
end
M.cmp_tabnine = function()
  require("cmp_tabnine.config"):setup({
    max_lines = 1000,
    max_num_results = 20,
    sort = true,
    run_on_every_keystroke = true,
    snippet_placeholder = "..",
    ignored_file_types = { -- default is not to ignore
      -- uncomment to ignore in lua:
      -- lua = true
    },
  })
end
M.marks = function()
  require("marks").setup({
    default_mappings = true,
    builtin_marks = {},
    cyclic = true,
    force_write_shada = false,
    refresh_interval = 250,
    sign_priority = { lower = 10, upper = 15, builtin = 8, bookmark = 20 },
    excluded_filetypes = {},
    bookmark_0 = {
      sign = "⚑",
      virt_text = "hello world",
    },
    mappings = {},
  })
end
M.auto_save = function()
  require("autosave").setup({
    enabled = true,
    execution_message = function()
      return "AutoSave: saved at " .. vim.fn.strftime("%H:%M:%S")
    end,
    events = { "InsertLeave", "TextChanged" },
    conditions = {
      exists = true,
      filename_is_not = {},
      filetype_is_not = {},
      modifiable = true,
    },
    write_all_buffers = false,
    on_off_commands = true,
    clean_command_line_interval = 1000,
    debounce_delay = 135,
  })
end
M.diffview = function()
  require("diffview").setup({})
end
M.navigator = function()
  require("Navigator").setup()
  local map = vim.api.nvim_set_keymap
  local opts = { noremap = true, silent = true }
  map("n", "<A-h>", "<CMD>lua require('Navigator').left()<CR>", opts)
  map("n", "<A-k>", "<CMD>lua require('Navigator').up()<CR>", opts)
  map("n", "<A-l>", "<CMD>lua require('Navigator').right()<CR>", opts)
  map("n", "<A-j>", "<CMD>lua require('Navigator').down()<CR>", opts)
  map("n", "<A-p>", "<CMD>lua require('Navigator').previous()<CR>", opts)
end
M.scrollbar = function()
  require("scrollbar").setup()
  require("scrollbar.handlers.search").setup()
end
M.ts_autotag = function() end
M.lspsaga = function()
  local saga = require("lspsaga")
  saga.init_lsp_saga({})
end
M.rust_tools = function()
  local adapter
  if n.vscode_lldb then
    adapter = require("rust-tools.dap").get_codelldb_adapter(
      n.vscode_lldb .. "/share/vscode/extensions/vadimcn.vscode-lldb/adapter/codelldb",
      n.vscode_lldb .. "/share/vscode/extensions/vadimcn.vscode-lldb/lldb/lib/liblldb.so"
    )
  end
  require("rust-tools").setup({
    tools = {
      inlay_hints = {
        show_variable_name = true,
      },
    },
    dap = {
      adapter = adapter
    },
  })
end
M.dap_ui = function()
  require("dapui").setup({})
  local dap = require("dap")
  dap.adapters.lldb = {
    type = "executable",
    command = "lldb-vscode", -- adjust as needed
    name = "lldb",
  }
  local path_cache = vim.fn.getcwd() .. "/"
  dap.configurations.cpp = {
    {
      name = "Launch",
      type = "lldb",
      request = "launch",
      program = function()
        path_cache = vim.fn.input("Path to executable: ", path_cache, "file")
        return path_cache
      end,
      cwd = "${workspaceFolder}",
      -- stopOnEntry = true,
    },
  }
  dap.configurations.c = dap.configurations.cpp
  dap.configurations.rust = dap.configurations.cpp
end
M.dap_virtual_text = function()
  require("nvim-dap-virtual-text").setup()
end
M.dap_install = function()
  local dap_install = require("dap-install")
  dap_install.setup({
    installation_path = vim.fn.stdpath("data") .. "/dapinstall/",
  })
end
M.fterm = function()
  require("FTerm").setup({
    border = "rounded",
    dimensions = {
      height = 0.9,
      width = 0.9,
    },
  })
end
M.mini = function()
  require("mini.surround").setup({
    n_lines = 65536,
    highlight_duration = 500,
    mappings = {
      add = "S", -- Add surrounding
      delete = "ds", -- Delete surrounding
      find = "sf", -- Find surrounding (to the right)
      find_left = "sF", -- Find surrounding (to the left)
      highlight = "sh", -- Highlight surrounding
      replace = "cs", -- Replace surrounding
      update_n_lines = "sn", -- Update `n_lines`
    },
  })
end
M.session_manager = function()
  local Path = require("plenary.path")
  require("session_manager").setup({
    sessions_dir = Path:new(vim.fn.stdpath("data"), "sessions"), -- The directory where the session files will be saved.
    path_replacer = "__", -- The character to which the path separator will be replaced for session files.
    colon_replacer = "++", -- The character to which the colon symbol will be replaced for session files.
    autoload_mode = require("session_manager.config").AutoloadMode.CurrentDir, -- Define what to do when Neovim is started without arguments. Possible values: Disabled, CurrentDir, LastSession
    autosave_last_session = true, -- Automatically save last session on exit and on session switch.
    autosave_ignore_not_normal = true, -- Plugin will not save a session when no buffers are opened, or all of them aren't writable or listed.
    autosave_ignore_filetypes = { -- All buffers of these file types will be closed before the session is saved.
      "gitcommit",
      "FTerm",
      "NvimTree",
    },
    autosave_only_in_session = false, -- Always autosaves session. If true, only autosaves after a session is active.
  })
end
return M
