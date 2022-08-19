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
  require("Navigator").setup({ autosave = "all" })
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
  require("dapui").setup({
    icons = { expanded = "▾", collapsed = "▸" },
    mappings = {
      -- Use a table to apply multiple mappings
      expand = { "<CR>", "<2-LeftMouse>" },
      open = "o",
      remove = "d",
      edit = "e",
      repl = "r",
      toggle = "t",
    },
    -- Expand lines larger than the window
    -- Requires >= 0.7
    expand_lines = vim.fn.has("nvim-0.7"),
    -- Layouts define sections of the screen to place windows.
    -- The position can be "left", "right", "top" or "bottom".
    -- The size specifies the height/width depending on position. It can be an Int
    -- or a Float. Integer specifies height/width directly (i.e. 20 lines/columns) while
    -- Float value specifies percentage (i.e. 0.3 - 30% of available lines/columns)
    -- Elements are the elements shown in the layout (in order).
    -- Layouts are opened in order so that earlier layouts take priority in window sizing.
    layouts = {
      {
        elements = {
          -- Elements can be strings or table with id and size keys.
          { id = "scopes", size = 0.25 },
          { id = "watches", size = 0.16 },
          "stacks",
          { id = "breakpoints", size = 0.1 },
        },
        size = 40, -- 40 columns
        position = "left",
      },
      {
        elements = {
          "repl",
          "console",
        },
        size = 0.25, -- 25% of total lines
        position = "bottom",
      },
    },
    floating = {
      max_height = nil, -- These can be integers or a float between 0 and 1.
      max_width = nil, -- Floats will be treated as percentage of your screen.
      border = "single", -- Border style. Can be "single", "double" or "rounded"
      mappings = {
        close = { "q", "<Esc>" },
      },
    },
    windows = { indent = 1 },
    render = {
      max_type_length = nil, -- Can be integer or nil.
    }
  })
end
M.dap_virtual_text = function()
  require("nvim-dap-virtual-text").setup {
    enabled = true, -- enable this plugin (the default)
    enabled_commands = true, -- create commands DapVirtualTextEnable, DapVirtualTextDisable, DapVirtualTextToggle, (DapVirtualTextForceRefresh for refreshing when debug adapter did not notify its termination)
    highlight_changed_variables = true, -- highlight changed values with NvimDapVirtualTextChanged, else always NvimDapVirtualText
    highlight_new_as_changed = true, -- highlight new variables in the same way as changed variables (if highlight_changed_variables)
    show_stop_reason = true, -- show stop reason when stopped for exceptions
    commented = true, -- prefix virtual text with comment string
    only_first_definition = true, -- only show virtual text at first definition (if there are multiple)
    all_references = false, -- show virtual text on all all references of the variable (not only definitions)
    filter_references_pattern = '<module', -- filter references (not definitions) pattern when all_references is activated (Lua gmatch pattern, default filters out Python modules)
    -- experimental features:
    virt_text_pos = 'eol', -- position of virtual text, see `:h nvim_buf_set_extmark()`
    all_frames = true, -- show virtual text for all stack frames not only current. Only works for debugpy on my machine.
    virt_lines = false, -- show virtual lines instead of virtual text (will flicker!)
    virt_text_win_col = nil -- position the virtual text at a fixed window column (starting from the first text column) ,
    -- e.g. 80 to position at column 80, see `:h nvim_buf_set_extmark()`
  }
end
M.persistent_breakpoints_nvim = function()
  require('persistent-breakpoints').setup {
    save_dir = vim.fn.stdpath('data') .. '/nvim_checkpoints',
    -- record the performance of different function. run :lua require('persistent-breakpoints.api').print_perf_data() to see the result.
    perf_record = false,
  }
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
    autoload_mode = require("session_manager.config").AutoloadMode.Disabled, -- Define what to do when Neovim is started without arguments. Possible values: Disabled, CurrentDir, LastSession
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
M.hop = function()
  require 'hop'.setup({

  })
end
M.project = function()
  require("project_nvim").setup {
    -- Manual mode doesn't automatically change your root directory, so you have
    -- the option to manually do so using `:ProjectRoot` command.
    manual_mode = false,
    -- Methods of detecting the root directory. **"lsp"** uses the native neovim
    -- lsp, while **"pattern"** uses vim-rooter like glob pattern matching. Here
    -- order matters: if one is not detected, the other is used as fallback. You
    -- can also delete or rearangne the detection methods.
    detection_methods = { "lsp", "pattern" },
    -- All the patterns used to detect root dir, when **"pattern"** is in
    -- detection_methods
    patterns = { "_darcs", ".hg", ".bzr", ".svn", "Makefile", "package.json" },
    -- Table of lsp clients to ignore by name
    -- eg: { "efm", ... }
    ignore_lsp = {},
    -- Don't calculate root dir on specific directories
    -- Ex: { "~/.cargo/*", ... }
    exclude_dirs = {},
    -- Show hidden files in telescope
    show_hidden = false,
    -- When set to false, you will get a message when project.nvim changes your
    -- directory.
    silent_chdir = false,
    -- Path where project.nvim will store the project history for use in
    -- telescope
    datapath = vim.fn.stdpath("data"),
  }
end
return M
