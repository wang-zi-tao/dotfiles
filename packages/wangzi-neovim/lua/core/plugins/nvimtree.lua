local g = vim.g

g.nvim_tree_add_trailing = 0 -- append a trailing slash to folder names
g.nvim_tree_root_folder_modifier = table.concat({ ":t:gs?$?/..", string.rep(" ", 1000), "?:gs?^??" })

g.nvim_tree_show_icons = {
  folders = 1,
  files = 1,
  git = 1,
}

g.nvim_tree_icons = {
  default = "",
  symlink = "",
  git = {
    deleted = "﯊",
    ignored = "",
    renamed = "➜",
    staged = "✓",
    unmerged = "",
    unstaged = "",
    untracked = "★",
  },
  folder = {
    arrow_open = "",
    arrow_closed = "",
    default = "",
    open = "",
    empty = "",
    empty_open = "",
    symlink = "",
    symlink_open = "",
  },
}
require("nvim-tree").setup({
  disable_netrw = false,
  hijack_netrw = true,
  open_on_setup = false,
  ignore_ft_on_setup = {},
  auto_close = false,
  auto_reload_on_write = true,
  open_on_tab = false,
  hijack_cursor = false,
  update_cwd = false,
  diagnostics = {
    enable = true,
    show_on_dirs = true,
    icons = {
      hint = "",
      info = "",
      warning = "",
      error = "",
    },
  },
  update_focused_file = {
    enable = true,
    update_cwd = false,
    ignore_list = {},
  },
  system_open = {
    cmd = "xdg-open",
    args = {},
  },
  filters = {
    dotfiles = false,
    custom = {},
  },
  git = {
    enable = true,
    ignore = true,
    timeout = 500,
  },
  view = {
    width = 25,
    hide_root_folder = false,
    side = "left",
    auto_resize = true,
  },
  trash = {
    cmd = "rmtrash",
    require_confirm = true,
  },
  renderer = {
    indent_markers = {
      enable = false,
      icons = {
        corner = "└ ",
        edge = "│ ",
        none = "  ",
      },
    },
    icons = {
      webdev_colors = true,
    },
  },
  hijack_directories = {
    enable = true,
    auto_open = true,
  },
  actions = {
    open_file = {
      quit_on_open = true,
      window_picker = {
        enable = true,
        chars = "1234567890",
        exclude = {
          filetype = {
            "notify",
            "packer",
            "qf",
          },
          buftype = { "terminal" },
        },
      },
    },
  },
})
