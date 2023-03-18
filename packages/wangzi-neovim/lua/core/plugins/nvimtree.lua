local function config()
    require("nvim-tree").setup({
        disable_netrw = false,
        hijack_netrw = true,
        open_on_setup = false,
        ignore_ft_on_setup = {},
        -- auto_close = false,
        auto_reload_on_write = true,
        open_on_tab = true,
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
            -- auto_resize = true,
        },
        trash = {
            cmd = "rmtrash",
            require_confirm = true,
        },
        renderer = {
            add_trailing = false,
            root_folder_modifier = table.concat({ ":t:gs?$?/..", string.rep(" ", 1000), "?:gs?^??" }),
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
                show = {
                    folder = true,
                    folder_arrow = true,
                    file = true,
                    git = true,
                },
                glyphs = {
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
            },
        },
        hijack_directories = {
            enable = true,
            auto_open = true,
        },
        actions = {
            open_file = {
                quit_on_open = false,
                window_picker = {
                    enable = true,
                    chars = "asdfghjkl",
                    exclude = {
                        filetype = {
                            "notify",
                            "packer",
                            "qf",
                            "OUTLINE",
                        },
                        buftype = { "terminal" },
                    },
                },
            },
        },
    })
end

return {
    "kyazdani42/nvim-tree.lua",
    dir = gen.nvim_tree_lua,
    name = "nvim_tree_lua",
    dependencies = { "nvim_web_devicons", "onedark_nvim" },
    module = "nvim-tree",
    cmd = { "NvimTreeToggle", "NvimTreeFocus" },
    lazy = true,
    config = config,
    keys = {
        { "<leader>e", "<cmd>NvimTreeToggle<cr>", desc = "File Tree" },
    },
}
