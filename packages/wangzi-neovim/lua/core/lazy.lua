local gen = gen
local state = {}
global = {}

function global:toggle_term(number)
    require("toggleterm")
    self.toggleterm_nvim[number]:toggle()
end

vim.g.mapleader = " "
require("lazy").setup({
    require("core.plugins.cmp"),
    require("core.plugins.telescope"),
    require("core.plugins.treesitter"),
    require("core.plugins.dap"),
    require("core.plugins.lspconfig"),
    require("core.plugins.ui"),

    -- require("core.plugins.nvimtree"),
    require("core.plugins.neotree"),
    require("core.plugins.symbols_outline_pre"),
    require("core.plugins.git"),
    require("core.plugins.vgit"),
    require("core.plugins.ai"),
    require("core.plugins.terminal"),
    require("core.plugins.coding"),
    require("core.plugins.navigation"),
    require("core.plugins.view"),
    require("core.plugins.trouble"),
    require("core.plugins.trailblazer"),
    require("core.plugins.perf"),
    require("core.plugins.lsp"),
    require("core.plugins.rust"),
    require("core.plugins.cpp"),
    require("core.plugins.null_ls"),
    require("core.plugins.fold"),

    {
        "nvim-lua/plenary.nvim",
        name = "plenary_nvim",
        dir = gen.plenary_nvim,
        module = "plenary",
        lazy = true,
        keys = {
            {
                "<leader>pp",
                function()
                    require("plenary.profile").start(".nvim-profile.log", { flame = true })
                    vim.notify("start profiling")
                end,
                desc = "Profile Start",
            },
            {
                "<leader>pP",
                function()
                    local Job = require 'plenary.job'
                    local file = ".nvim-profile.log"
                    require("plenary.profile").stop()
                    Job:new({
                        command = "nix",
                        args = { "-p", "inferno", "--command", "inferno-flamegraph " .. file .. " > .nvim-profile.svg" },
                        on_exit = function(j)
                            Job:new({ command = "xdg-open", args = { file } }):start()
                        end
                    }):start()
                    vim.notify("end profiling")
                end,
                desc = "Profile Stop",
            },
        },
    },

    {
        "kkharji/sqlite.lua",
        dir = gen.sqlite,
        name = "sqlite",
        module = { "sqlite", "sqlite.tbl", "sqlite.db" },
        lazy = false,
        init = function()
            if gen.libsqlite then
                vim.g.sqlite_clib_path = gen.libsqlite
            end
        end,
    },
    {
        "anuvyklack/keymap-amend.nvim",
        dir = gen.keymap_amend,
        name = "keymap_amend",
        lazy = true,
        module = "keymap-amend",
    },

    {
        "glacambre/firenvim",
        dir = gen.firenvim,
        name = "firenvim",
        lazy = true,
        build = function()
            vim.fn["firenvim#install"](0)
        end,
    },
    {
        "chipsenkbeil/distant.nvim",
        dir = gen.distant,
        name = "distant",
        module = "distant",
        cmd = { "DistantOpen", "DistantLaunch", "DistantInstall" },
        lazy = true,
        config = function()
            require("core.plugins.others").distant()
        end,
    },
}, {
    -- root = gen.core or (vim.fn.stdpath("data") .. "/lazy"), -- directory where plugins will be installed
    defaults = {
        -- lazy = true, -- should plugins be lazy-loaded?
    },
    ui = {
        border = "rounded",
    },
    readme = {
        enabled = gen.core ~= nil,
        -- root = gen.core or (vim.fn.stdpath("state") .. "/lazy/readme"),
        files = { "README.md", "lua/**/README.md" },
        -- only generate markdown helptags for plugins that dont have docs
        skip_if_doc_exists = true,
    },
    concurrency = 16,
    git = {
        -- defaults for the `Lazy log` command
        -- log = { "-10" }, -- show the last 10 commits
        log = { "--since=3 days ago" }, -- show commits from the last 3 days
        timeout = 120,                  -- kill processes that take more than 2 minutes
        -- lazy.nvim requires git >=2.19.0. If you really want to use lazy with an older version,
        -- then set the below to false. This should work, but is NOT supported and will
        -- increase downloads a lot.
        filter = true,
    },
    checker = { enabled = vim.fn.has("win32") == 1 },
    performance = {
        cache = {
            enabled = true,
        },
        reset_packpath = true, -- reset the package path to improve startup time
        rtp = {
            reset = true,      -- reset the runtime path to $VIMRUNTIME and your config directory
            ---@type string[]
            paths = {},        -- add any custom paths here that you want to includes in the rtp
            ---@type string[] list any plugins you want to disable here
            disabled_plugins = {
                -- "gzip",
                -- "matchit",
                -- "matchparen",
                -- "netrwPlugin",
                -- "tarPlugin",
                -- "tohtml",
                -- "tutor",
                -- "zipPlugin",
            },
        },
    },
})
