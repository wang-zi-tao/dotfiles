-- Eviline config for lualine
-- Author: shadmansaleh
-- Credit: glepnir
local lualine = require("lualine")
local noice = require("noice")

-- Color table for highlights
-- stylua: ignore
local colors = {
    bg = '#202328',
    fg = '#bbc2cf',
    yellow = '#ECBE7B',
    cyan = '#008080',
    darkblue = '#081633',
    green = '#98be65',
    orange = '#FF8800',
    violet = '#a9a1e1',
    magenta = '#c678dd',
    blue = '#51afef',
    red = '#ec5f67',
    white = "#abb2bf",
    darker_black = "#1b1f27",
    black = "#1e222a",  --  nvim bg
    black2 = "#252931",
    one_bg = "#282c34", -- real bg of onedark
    one_bg2 = "#353b45",
    one_bg3 = "#30343c",
    grey = "#42464e",
    grey_fg = "#565c64",
    grey_fg2 = "#6f737b",
    light_grey = "#6f737b",
    baby_pink = "#DE8C92",
    pink = "#ff75a0",
    line = "#2a2e36", -- for lines like vertsplit
    vibrant_green = "#7eca9c",
    nord_blue = "#81A1C1",
    sun = "#EBCB8B",
    purple = "#b4bbc8",
    dark_purple = "#c882e7",
    teal = "#519ABA",
    statusline_bg = "#22262e",
    lightbg = "#2d3139",
    lightbg2 = "#262a32",
    pmenu_bg = "#A3BE8C",
    folder_bg = "#61afef",
}

local conditions = {
  buffer_not_empty = function()
    return vim.fn.empty(vim.fn.expand("%:t")) ~= 1
  end,
  hide_in_width = function()
    return vim.fn.winwidth(0) > 80
  end,
  check_git_workspace = function()
    local filepath = vim.fn.expand("%:p:h")
    local gitdir = vim.fn.finddir(".git", filepath .. ";")
    return gitdir and #gitdir > 0 and #gitdir < #filepath
  end,
}

-- Config

-- Inserts a component in lualine_c at left section

local mode_color = {
  n = colors.red,
  i = colors.green,
  v = colors.blue,
  [""] = colors.blue,
  V = colors.blue,
  c = colors.magenta,
  no = colors.red,
  s = colors.orange,
  S = colors.orange,
  [""] = colors.orange,
  ic = colors.yellow,
  R = colors.violet,
  Rv = colors.violet,
  cv = colors.red,
  ce = colors.red,
  r = colors.cyan,
  rm = colors.cyan,
  ["r?"] = colors.cyan,
  ["!"] = colors.red,
  t = colors.red,
}
local mode = {
  -- mode component
  function()
    return ""
  end,
  color = { fg = "#ffffff" },
  -- color = function()
  --     return { fg = mode_color[vim.fn.mode()] }
  -- end,
}

local filesize = {
  -- filesize component
  "filesize",
  icon = "",
  cond = conditions.buffer_not_empty,
  color = { fg = "#ffffff", gui = "bold" },
}
local filetype = {
  "filetype",
  colored = true, -- Displays filetype icon in color if set to true
  icon_only = true, -- Display only an icon for filetype
  icon = { align = "right" }, -- Display filetype icon on the right hand side
  align = "right",
  -- icon =    {'X', align='right'}
  -- Icon string ^ in table is ignored in filetype component
}

local filename = {
  "filename",
  symbols = {
    modified = "", -- Text to show when the file is modified.
    readonly = "[-]", -- Text to show when the file is non-modifiable or readonly.
    unnamed = "[No Name]", -- Text to show for unnamed buffers.
    newfile = " ", -- Text to show for newly created file before first write
  },
  path = 0,
  padding = { left = 0, right = 1 },
  cond = conditions.buffer_not_empty,
  color = { fg = colors.fg, gui = "bold" },
}

local branch = {
  "branch",
  icon = "",
  color = {
    fg = colors.blue,
    separator = { right = "" },
  },
}

local diff = {
  "diff",
  icon = "",
  -- Is it me or the symbol for modified us really weird
  symbols = { added = " ", modified = "", removed = " " },
  diff_color = {
    added = { fg = colors.green },
    modified = { fg = colors.orange },
    removed = { fg = colors.red },
  },
  cond = conditions.hide_in_width,
  color = {
    fg = colors.blue,
    separator = { left = "" },
  },
}
local left_separator = {
  function()
    return ""
  end,
  color = {},
}

local right_separator = {
  function()
    return ""
  end,
  color = {},
}

local diagnostics = {
  "diagnostics",
  sources = { "nvim_diagnostic" },
  symbols = { error = " ", warn = " ", info = " " },
  diagnostics_color = {
    color_error = { fg = colors.red },
    color_warn = { fg = colors.yellow },
    color_info = { fg = colors.cyan },
  },
}

-- Insert mid section. You can make any number of sections in neovim :)
-- for lualine it's any number greater then 2
local midle = {
  function()
    return "%="
  end,
  color = {
    separator = { left = "", right = "" },
  },
}
local lsp_indexing = {
  color = {
    separator = { left = "", right = "" },
    fg = colors.blue,
    gui = "bold",
  },
  function()
    local Lsp = vim.lsp.util.get_progress_messages()[1]

    if Lsp then
      local msg = Lsp.message or ""
      local percentage = Lsp.percentage or 0
      local title = Lsp.title or ""
      local spinners = {
        "",
        "",
        "",
      }

      local success_icon = {
        "",
        "",
        "",
      }

      local ms = vim.loop.hrtime() / 1000000
      local frame = math.floor(ms / 120) % #spinners

      if percentage >= 70 then
        return string.format(" %%<%s %s %s (%s%%%%) ", success_icon[frame + 1], title, msg, percentage)
      end

      return string.format(" %%<%s %s %s (%s%%%%) ", spinners[frame + 1], title, msg, percentage)
    end

    return ""
  end,
}

local message = {
  noice.api.status.message.get_hl,
  cond = noice.api.status.message.has,
}
local command = {
  noice.api.status.command.get,
  cond = noice.api.status.command.has,
  color = { fg = colors.grey_fg2 },
}
local noice_mode = {
  noice.api.status.mode.get,
  cond = noice.api.status.mode.has,
  color = { fg = colors.grey_fg2 },
}
local search = {
  noice.api.status.search.get,
  cond = noice.api.status.search.has,
  color = { fg = colors.grey_fg2 },
}
local lsp_symbol = {
  rust_analyzer = "",
  clangd = " ",
  pyright = "",
  rnix = " ",
  lua_ls = "",
}
local lsp = {
  -- Lsp server name .
  function()
    local msg = "No Active Lsp"
    local buf_ft = vim.api.nvim_buf_get_option(0, "filetype")
    local clients = vim.lsp.get_active_clients()
    if next(clients) == nil then
      return msg
    end
    for _, client in ipairs(clients) do
      local filetypes = client.config.filetypes
      if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
        return " LSP:" .. (lsp_symbol[client.name] or client.name)
      end
    end
    return msg
  end,
  color = { fg = colors.fg, gui = "bold" },
}

-- Add components to right sections
local encoding = {
  "o:encoding", -- option component same as &encoding in viml
  fmt = string.upper, -- I'm not sure why it's upper case either ;)
  cond = conditions.hide_in_width,
  color = { fg = colors.blue, gui = "bold" },
}

local file_format = {
  "fileformat",
  symbols = {
    unix = " ", -- e712
    dos = "", -- e70f
    mac = "", -- e711
  },
  color = { fg = colors.blue, gui = "bold" },
}

local location = {
  "location",
  icon = "",
  color = {
    fg = "#ffffff",
  },
}

local progress = {
  "progress",
  icon = " ",
  padding = 0,
  color = {
    fg = "#ffffff",
    gui = "bold",
  },
}

local lsp_count = {
  function()
    local data = require("dr-lsp").lspCountTable()
    return string.format("LSP: %dD >> %dR", data.workspace.definitions, data.workspace.references)
  end,
  color = { fg = colors.blue },
}

-- Now don't forget to initialize lualine

lualine.setup({
  options = {
    -- Disable sections and component separators
    -- component_separators = '',
    -- section_separators = '',
    section_separators = { left = "", right = "" },
    component_separators = { left = "", right = "" },
    -- component_separators = { left = '', right = '' },
    theme = {
      normal = {
        a = { bg = colors.blue },
        b = { bg = colors.grey },
        c = { bg = colors.black },
      },

      insert = { a = { bg = colors.green } },
      terminal = { a = { bg = colors.violet } },
      visual = { a = { bg = colors.yellow } },
      replace = { a = { bg = colors.red } },

      inactive = {
        a = { bg = colors.black },
        b = { bg = colors.black },
        c = { bg = colors.black },
      },
    },
    disabled_filetypes = {
      "NvimTree",
      "Outline",
      "lspsagaoutline",
      "dapui_scopes",
      "dapui_watches",
      "dapui_stacks",
      "dapui_breakpoints",
    },
  },
  sections = {
    lualine_a = { mode },
    lualine_b = { filetype, filename },
    lualine_c = {
      branch,
      diff,
      lsp_count,
      {
        require("dr-lsp").lspProgress,
        color = { fg = colors.blue },
      },
      midle,
      -- lsp_indexing,
    },
    lualine_x = { diagnostics, lsp, "tabnine" },
    lualine_y = { encoding, file_format },
    lualine_z = { location, progress, filesize },
  },
  inactive_sections = {
    -- these are to remove the defaults
    lualine_a = {},
    lualine_b = {},
    lualine_y = {},
    lualine_z = {},
    lualine_c = {},
    lualine_x = {},
  },
  extensions = {
    "quickfix",
    "nvim-tree",
    "symbols-outline",
    "nvim-dap-ui",
  },
})
