local n = require("core.gen")
local M = {}
M.onedark = function()
  local onedark = require("onedark")
  onedark.setup({
    -- Main options --
    style = "deep", -- Default theme style. Choose between 'dark', 'darker', 'cool', 'deep', 'warm', 'warmer' and 'light'
    transparent = false, -- Show/hide background
    term_colors = false, -- Change terminal color as per the selected theme style
    ending_tildes = false, -- Show the end-of-buffer tildes. By default they are hidden
    -- toggle theme style ---
    toggle_style_key = "<leader>ts", -- Default keybinding to toggle
    toggle_style_list = { "dark", "darker", "cool", "deep", "warm", "warmer", "light" }, -- List of styles to toggle between

    -- Change code style ---
    -- Options are italic, bold, underline, none
    -- You can configure multiple style with comma seperated, For e.g., keywords = 'italic,bold'
    code_style = {
      comments = "italic",
      keywords = "none",
      functions = "none",
      strings = "none",
      variables = "none",
    },

    -- Custom Highlights --
    colors = {}, -- Override default colors
    highlights = {
      NormalBorder = { fg = "$blue" },
      VertSplit = { fg = "$blue" },
    }, -- Override highlight groups

    -- Plugins Config --
    diagnostics = {
      darker = true, -- darker colors for diagnostic
      undercurl = true, -- use undercurl instead of underline for diagnostics
      background = false, -- use background color for virtual text
    },
  })
  onedark.load()
end
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
  require("Comment").setup({
    padding = true,
    sticky = true,
    ignore = nil,
    toggler = {
      line = "gcc",
      block = "gbc",
    },
    opleader = {
      line = "gc",
      block = "gb",
    },
    extra = {
      above = "gcO",
      below = "gco",
      eol = "gcA",
    },
    mappings = {
      basic = true,
      extra = true,
      extended = false,
    },
    pre_hook = nil,
    post_hook = nil,
  })
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

M.lsp_handlers = function()
  local function lspSymbol(name, icon)
    local hl = "DiagnosticSign" .. name
    vim.fn.sign_define(hl, { text = icon, numhl = hl, texthl = hl })
  end

  lspSymbol("Error", "")
  lspSymbol("Info", "")
  lspSymbol("Hint", "")
  lspSymbol("Warn", "")

  vim.diagnostic.config({
    virtual_text = {
      prefix = "",
    },
    signs = true,
    underline = true,
    update_in_insert = false,
  })

  vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
    border = "single",
  })
  vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
    border = "single",
  })

  -- suppress error messages from lang servers
  vim.notify = function(msg, log_level)
    if msg:match("exit code") then
      return
    end
    if log_level == vim.log.levels.ERROR then
      vim.api.nvim_err_writeln(msg)
    else
      vim.api.nvim_echo({ { msg } }, true, {})
    end
  end
end

M.gitsigns = function()
  require("gitsigns").setup({
    signs = {
      add = { hl = "GitSignsAdd", text = "│", numhl = "GitSignsAddNr", linehl = "GitSignsAddLn" },
      change = { hl = "GitSignsChange", text = "│", numhl = "GitSignsChangeNr", linehl = "GitSignsChangeLn" },
      delete = { hl = "GitSignsDelete", text = "_", numhl = "GitSignsDeleteNr", linehl = "GitSignsDeleteLn" },
      topdelete = { hl = "GitSignsDelete", text = "‾", numhl = "GitSignsDeleteNr", linehl = "GitSignsDeleteLn" },
      changedelete = { hl = "GitSignsChange", text = "~", numhl = "GitSignsChangeNr", linehl = "GitSignsChangeLn" },
    },
    signcolumn = true, -- Toggle with `:Gitsigns toggle_signs`
    numhl = false, -- Toggle with `:Gitsigns toggle_numhl`
    linehl = false, -- Toggle with `:Gitsigns toggle_linehl`
    word_diff = true, -- Toggle with `:Gitsigns toggle_word_diff`
    watch_gitdir = {
      interval = 1000,
      follow_files = true,
    },
    attach_to_untracked = true,
    current_line_blame = false, -- Toggle with `:Gitsigns toggle_current_line_blame`
    current_line_blame_opts = {
      virt_text = true,
      virt_text_pos = "eol", -- 'eol' | 'overlay' | 'right_align'
      delay = 1000,
      ignore_whitespace = false,
    },
    sign_priority = 6,
    update_debounce = 100,
    status_formatter = nil, -- Use default
    max_file_length = 40000,
    preview_config = {
      -- Options passed to nvim_open_win
      border = "single",
      style = "minimal",
      relative = "cursor",
      row = 0,
      col = 1,
    },
    yadm = {
      enable = false,
    },
  })
end
M.which_key = function()
  require("which-key").setup({})
end
M.symbols_outline_pre = function()
  vim.g.symbols_outline = {
    highlight_hovered_item = true,
    show_guides = true,
    auto_preview = true,
    position = "right",
    relative_width = true,
    width = 25,
    auto_close = false,
    show_numbers = false,
    show_relative_numbers = false,
    show_symbol_details = true,
    preview_bg_highlight = "Pmenu",
    keymaps = { -- These keymaps can be a string or a table for multiple keys
      close = { "<Esc>", "q" },
      goto_location = "<Cr>",
      focus_location = "o",
      hover_symbol = "<C-space>",
      toggle_preview = "K",
      rename_symbol = "r",
      code_actions = "a",
    },
    lsp_blacklist = {},
    symbol_blacklist = {},
    symbols = {
      File = { icon = "", hl = "SymbolOutlineURI" },
      Module = { icon = "", hl = "SymbolOutlineNamespace" },
      Namespace = { icon = "", hl = "SymbolOutlineNamespace" },
      Package = { icon = "", hl = "SymbolOutlineNamespace" },
      Class = { icon = "C", hl = "SymbolOutlineType" },
      Method = { icon = "F", hl = "SymbolOutlineMethod" },
      Property = { icon = "", hl = "SymbolOutlineMethod" },
      Field = { icon = "", hl = "SymbolOutlineField" },
      Constructor = { icon = "F", hl = "SymbolOutlineConstructor" },
      Enum = { icon = "", hl = "SymbolOutlineType" },
      Interface = { icon = "", hl = "SymbolOutlineType" },
      Function = { icon = "", hl = "SymbolOutlineFunction" },
      Variable = { icon = "", hl = "SymbolOutlineConstant" },
      Constant = { icon = "", hl = "SymbolOutlineConstant" },
      String = { icon = "", hl = "SymbolOutlineString" },
      Number = { icon = "#", hl = "SymbolOutlineNumber" },
      Boolean = { icon = "01", hl = "SymbolOutlineBoolean" },
      Array = { icon = "", hl = "SymbolOutlineConstant" },
      Object = { icon = "", hl = "SymbolOutlineType" },
      Key = { icon = "", hl = "SymbolOutlineType" },
      Null = { icon = "0", hl = "SymbolOutlineType" },
      EnumMember = { icon = "", hl = "SymbolOutlineField" },
      Struct = { icon = "", hl = "SymbolOutlineType" },
      Event = { icon = "", hl = "SymbolOutlineType" },
      Operator = { icon = "+", hl = "SymbolOutlineOperator" },
      TypeParameter = { icon = "𝙏", hl = "SymbolOutlineParameter" },
    },
  }
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
M.ts_rainbow = function()
  require("nvim-treesitter.configs").setup({
    rainbow = {
      -- Setting colors
      colors = {
        -- Colors here
      },
      -- Term colors
      termcolors = {
        -- Term colors here
      },
    },
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
M.hlslens = function()
  require("hlslens").setup({
    override_lens = function(render, plist, nearest, idx, r_idx)
      local sfw = vim.v.searchforward == 1
      local indicator, text, chunks
      local abs_r_idx = math.abs(r_idx)
      if abs_r_idx > 1 then
        indicator = ("%d%s"):format(abs_r_idx, sfw ~= (r_idx > 1) and "▲" or "▼")
      elseif abs_r_idx == 1 then
        indicator = sfw ~= (r_idx == 1) and "▲" or "▼"
      else
        indicator = ""
      end

      local lnum, col = unpack(plist[idx])
      if nearest then
        local cnt = #plist
        if indicator ~= "" then
          text = ("(%s %d/%d)"):format(indicator, idx, cnt)
        else
          text = ("(%d/%d)"):format(idx, cnt)
        end
        chunks = { { " ", "Ignore" }, { text, "HlSearchLensNear" } }
      else
        text = ("(%s %d)"):format(indicator, idx)
        chunks = { { " ", "Ignore" }, { text, "HlSearchLens" } }
      end
      render.set_virt(0, lnum - 1, col - 1, chunks, nearest)
    end,
  })
end
M.pretty_fold = function()
  require("pretty-fold").setup({
    fill_char = "━",
    sections = {
      left = {
        "━ ",
        function()
          return string.rep("*", vim.v.foldlevel)
        end,
        " ━┫",
        "content",
        "┣",
      },
      right = {
        "┫ ",
        "number_of_folded_lines",
        ": ",
        "percentage",
        " ┣━━",
      },
    },
    remove_fold_markers = true,
    -- Keep the indentation of the content of the fold string.
    keep_indentation = true,
    -- Possible values:
    -- "delete" : Delete all comment signs from the fold string.
    -- "spaces" : Replace all comment signs with equal number of spaces.
    -- false    : Do nothing with comment signs.
    process_comment_signs = "spaces",
    -- Comment signs additional to the value of `&commentstring` option.
    comment_signs = {},
    -- List of patterns that will be removed from content foldtext section.
    stop_words = {
      "@brief%s*", -- (for C++) Remove '@brief' and all spaces after.
    },
    add_close_pattern = true, -- true, 'last_line' or false
    matchup_patterns = {
      -- beginning of the line -> any number of spaces -> 'do' -> end of the line
      { "^%s*do$", "end" }, -- `do ... end` blocks
      { "^%s*if", "end" }, -- if
      { "^%s*for", "end" }, -- for
      { "function%s*%(", "end" }, -- 'function( or 'function (''
      { "{", "}" },
      { "%(", ")" }, -- % to escape lua pattern char
      { "%[", "]" }, -- % to escape lua pattern char
    },
  })
  require("pretty-fold.preview").setup()
end
M.ts_autotag = function() end
M.lspsaga = function()
  local saga = require("lspsaga")
  saga.init_lsp_saga({})
end
M.rust_tools = function()
  require("rust-tools").setup({
    tools = {
      inlay_hints = {
        show_variable_name = true,
      },
    },
    dap = {
      adapter = require("rust-tools.dap").get_codelldb_adapter(
        n.vscode_lldb .. "/share/vscode/extensions/vadimcn.vscode-lldb/adapter/codelldb",
        n.lldb_lib .. "/lib/liblldb.so"
      ),
    },
  })
end
M.dap_ui = function()
  require("dapui").setup({})
  local dap = require("dap")
  dap.adapters.lldb = {
    type = "executable",
    command = n.lldb .. "/bin/lldb-vscode", -- adjust as needed
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
