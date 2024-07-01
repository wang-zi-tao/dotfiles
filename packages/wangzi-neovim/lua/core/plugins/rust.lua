vim.g.rustaceanvim = function()
    -- Update this path
    local extension_path = vim.env.HOME .. "/.vscode/extensions/vadimcn.vscode-lldb-1.10.0/"
    if gen.vscode_lldb then
        extension_path = gen.vscode_lldb .. "/share/vscode/extensions/vadimcn.vscode-lldb/"
    end
    local codelldb_path = extension_path .. "adapter/codelldb"
    local liblldb_path = extension_path .. "lldb/lib/liblldb"

    -- The path is different on Windows
    if 1 == vim.fn.has("win32") then
        codelldb_path = extension_path .. "adapter\\codelldb.exe"
        liblldb_path = extension_path .. "lldb\\bin\\liblldb.dll"
    else
        -- The liblldb extension is .so for Linux and .dylib for MacOS
        liblldb_path = liblldb_path .. (1 == vim.fn.has("linux") and ".so" or ".dylib")
    end

    local cfg = require("rustaceanvim.config")
    return {
        tools = {

            --- how to execute terminal commands
            --- options right now: termopen / quickfix / toggleterm / vimux
            ---@type RustaceanExecutor
            executor = require("rustaceanvim.executors").termopen,

            --- callback to execute once rust-analyzer is done initializing the workspace
            --- The callback receives one parameter indicating the `health` of the server: "ok" | "warning" | "error"
            ---@type fun(health:RustAnalyzerInitializedStatus) | nil
            on_initialized = nil,

            --- automatically call RustReloadWorkspace when writing to a Cargo.toml file.
            ---@type boolean
            reload_workspace_from_cargo_toml = true,

            --- options same as lsp hover
            ---@see vim.lsp.util.open_floating_preview
            ---@class RustaceanHoverActionsConfig
            hover_actions = {

                --- whether to replace Neovim's built-in `vim.lsp.buf.hover`.
                ---@type boolean
                replace_builtin_hover = false,
            },

            code_actions = {
                --- whether to fall back to `vim.ui.select` if there are no grouped code actions
                ---@type boolean
                ui_select_fallback = false,
            },

            --- options same as lsp hover
            ---@see vim.lsp.util.open_floating_preview
            ---@type table Options applied to floating windows.
            float_win_config = {

                -- the border that is used for floating windows
                ---@see vim.api.nvim_open_win()
                ---@type string[][] | string
                border = "rounded", -- maybe: 'double', 'rounded', 'shadow', 'single',

                --- maximal width of floating windows. Nil means no max.
                ---@type integer | nil
                max_width = nil,

                --- maximal height of floating windows. Nil means no max.
                ---@type integer | nil
                max_height = nil,

                --- whether the window gets automatically focused
                --- default: false
                ---@type boolean
                auto_focus = false,
            },

            --- settings for showing the crate graph based on graphviz and the dot
            --- command
            ---@class RustaceanCrateGraphConfig
            crate_graph = {
                -- backend used for displaying the graph
                -- see: https://graphviz.org/docs/outputs/
                -- default: x11
                ---@type string
                backend = "x11",
                -- where to store the output, nil for no output stored (relative
                -- path from pwd)
                -- default: nil
                ---@type string | nil
                output = nil,
                -- true for all crates.io and external crates, false only the local
                -- crates
                -- default: true
                ---@type boolean
                full = true,
                ---@type string | nil
                pipe = nil,
            },

            ---@type fun(url:string):nil
            open_url = function(url)
                require("rustaceanvim.os").open_url(url)
            end,
        },
        dap = {
            adapter = cfg.get_codelldb_adapter(codelldb_path, liblldb_path),
        },
        server = {
            on_attach = require("core.plugins.lspconfig").on_attach,
            settings = function(project_root)
                local ra = require("rustaceanvim.config.server")
                return ra.load_rust_analyzer_settings(project_root, {
                    settings_file_pattern = "rust-analyzer.json",
                })
            end,
        },
    }
end

return {
    {
        "simrat39/rust-tools.nvim",
        dir = gen.rust_tools,
        name = "rust_tools",
        dependencies = "nvim_lspconfig",
        enabled = false,
        cmd = {
            "RustSetInlayHints",
            "RustDisableInlayHints",
            "RustToggleInlayHints",
            "RustRunnables",
            "RustExpandMacro",
            "RustOpenCargo",
            "RustParentModule",
            "RustJoinLines",
            "RustHoverActions",
            "RustHoverRange",
            "RustMoveItemDown",
            "RustMoveItemUp",
            "RustStartStandaloneServerForBuffer",
            "RustDebuggables",
            "RustViewCrateGraph",
            "RustReloadWorkspace",
            "RustSSR",
        },
        lazy = true,
        config = function()
            local adapter
            if gen.vscode_lldb then
                adapter = require("rust-tools.dap").get_codelldb_adapter(
                    gen.vscode_lldb .. "/share/vscode/extensions/vadimcn.vscode-lldb/adapter/codelldb",
                    gen.vscode_lldb .. "/share/vscode/extensions/vadimcn.vscode-lldb/lldb/lib/liblldb.so"
                )
            end
            require("rust-tools").setup({
                tools = {
                    inlay_hints = {
                        auto = false,
                        show_variable_name = true,
                    },
                },
                dap = {
                    adapter = adapter,
                },
            })
        end,
        keys = {
            {
                "<leader>ca",
                function()
                    require("rust-tools").hover_actions.hover_actions()
                end,
                mode = "n",
                desc = "Cargo actio",
            },
        },
        ft = { "rs", "rust", "toml" },
    },
    {
        "mrcjkb/rustaceanvim",
        dir = gen.rustaceanvim,
        name = "rustaceanvim",
        version = "^4", -- Recommended
        dependencies = {
            "nvim_lspconfig",
            {
                "jubnzv/virtual-types.nvim",
                dir = gen.virtual_types_nvim,
                name = "virtual_types_nvim",
                module = "virtualtypes",
            },
        },
        cmd = { "RustLsp" },
        lazy = true,
        init = function()
            require("which-key").register({
                r = { name = "Rust" },
            }, { prefix = "<leader>" })
        end,
        keys = {
            {
                "<leader>ra",
                function()
                    vim.cmd.RustLsp({ "hover", "actions" })
                end,
                mode = "n",
                desc = "Cargo action",
            },
            {
                "<leader>rm",
                function()
                    vim.cmd.RustLsp("parentModule")
                end,
                desc = "Rust parent mod",
            },
            {
                "<leader>rc",
                function()
                    vim.cmd.RustLsp("openCargo")
                end,
                desc = "Rust Cargo.toml",
            },
            {
                "<leader>ro",
                function()
                    vim.cmd.RustLsp("workspaceSymbol")
                end,
                desc = "workspace symbols",
            },
            {
                "<leader>rd",
                function()
                    vim.cmd.RustLsp("openDocs")
                end,
                desc = "open docs.rs",
            },
            {
                "<leader>rs",
                function()
                    vim.cmd.RustLsp({ "ssr" })
                end,
                desc = "view syntax tree",
            },
            {
                "<leader>rvh",
                function()
                    vim.cmd.RustLsp({ "view", "hir" })
                end,
                desc = "view hir",
            },
            {
                "<leader>rvm",
                function()
                    vim.cmd.RustLsp({ "view", "mir" })
                end,
                desc = "view hir",
            },
            {
                "<leader>rvs",
                function()
                    vim.cmd.RustLsp("syntaxTree")
                end,
                desc = "view hir",
            },
        },
        ft = { "rs", "rust", "toml" },
    },
    {
        "saecki/crates.nvim",
        dir = gen.crates_nvim,
        name = "crates_nvim",
        dependencies = { "nvim_lspconfig", "plenary_nvim" },
        ft = { "toml" },
        module = "crates",
        lazy = true,
        config = function()
            require("core.plugins.crates")
        end,
        keys = {
            {
                "<leader>cu",
                function()
                    require("crates").upgrade_crate(nil)
                end,
                desc = "Cargo Upgrade ",
            },
            {
                "<leader>cU",
                function()
                    require("crates").upgrade_crates(nil)
                end,
                desc = "Cargo Upgrade Crates",
            },
            {
                "<leader>ch",
                function()
                    require("crates").open_homepage()
                end,
                desc = "Crate Homepage",
            },
            {
                "<leader>cR",
                function()
                    require("crates").open_repository()
                end,
                desc = "Crate Repository",
            },
            {
                "<leader>cD",
                function()
                    require("crates").open_documentation()
                end,
                desc = "Crate Documentation",
            },
            {
                "<leader>ci",
                function()
                    require("crates").open_crates_io()
                end,
                desc = "crate.io",
            },
            {
                "<leader>cp",
                function()
                    require("crates").show_popup()
                end,
                desc = "show popup",
            },
        },
    },

}
