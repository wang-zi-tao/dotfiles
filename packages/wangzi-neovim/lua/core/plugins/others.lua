local n = require("core.gen")
local M = {}
M.dap_install = function()
    local dap_install = require("dap-install")
    dap_install.setup({
        installation_path = vim.fn.stdpath("data") .. "/dapinstall/",
    })
end
M.distant = function()
    local actions = require("distant.nav.actions")

    local settings = {
        distant = {
            args = { "--shutdown-after", "60" },
        },
        -- Settings that apply when editing a remote file
        file = {
            mappings = {
                ["-"] = actions.up,
            },
        },
        -- Settings that apply to the navigation interface
        dir = {
            mappings = {
                ["<Return>"] = actions.edit,
                ["-"] = actions.up,
                ["K"] = actions.mkdir,
                ["N"] = actions.newfile,
                ["R"] = actions.rename,
                ["D"] = actions.remove,
            },
        },
        -- Maximimum time to wait (in milliseconds) for requests to finish
        max_timeout = 15 * 1000,
        -- Time to wait (in milliseconds) inbetween checks to see
        -- if a request timed out
        timeout_interval = 250,
        -- Time to wait (in milliseconds) inbetween polling checks to
        -- see if an async function has completed
        poll_interval = 200,
        -- Settings to use to start LSP instances
        lsp = {},
    }
    require("distant").setup({
        -- Applies Chip's personal settings to every machine you connect to
        --
        -- 1. Ensures that distant servers terminate with no connections
        -- 2. Provides navigation bindings for remote directories
        -- 3. Provides keybinding to jump into a remote file's parent directory
        ["*"] = settings,
    })
end
return M
