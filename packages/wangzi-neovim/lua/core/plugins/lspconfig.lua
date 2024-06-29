local function on_attach(client, bufnr)
    local navbuddy = require("nvim-navbuddy")
    navbuddy.attach(client, bufnr)
    require("lsp-format").on_attach(client, bufnr)

    if client.supports_method("textDocument/codeLens") then
        require("virtualtypes").on_attach(client, bufnr)
    end

    local function map(mod, key, exec, opt)
        opt = opt or { noremap = true, silent = false }
        if key == "" then
            return
        end
        vim.api.nvim_set_keymap(mod, key, exec, opt)
    end

    local m = {
        declaration = "gD",
        definition = "",
        hover = "K",
        implementation = "",
        signature_help = "gk",
        add_workspace_folder = "<leader>Wa",
        remove_workspace_folder = "<leader>Wr",
        list_workspace_folders = "<leader>Wl",
        type_definition = "",
        rename = "<leader>ra",
        references = "",
        float_diagnostics = "<leader>le",
        goto_prev = "[d",
        goto_next = "]d",
        set_loclist = "<leader>lq",
        formatting = "<leader>lf",
    }
    -- map("n", m.declaration, "<cmd>lua vim.lsp.buf.declaration()<CR>")
    -- map("n", m.definition, "<cmd>lua vim.lsp.buf.definition()<CR>")
    map("n", "K", "<cmd>Lspsaga hover_doc<CR>")
    -- map("n", m.implementation, "<cmd>lua vim.lsp.buf.implementation()<CR>")
    map("n", m.signature_help, "<cmd>lua vim.lsp.buf.signature_help()<CR>")
    map("n", m.add_workspace_folder, "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>")
    map("n", m.remove_workspace_folder, "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>")
    map("n", m.list_workspace_folders, "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>")
    -- map("n", m.type_definition, "<cmd>lua vim.lsp.buf.type_definition()<CR>")
    -- map("n", m.rename, "<cmd>lua vim.lsp.buf.rename()<CR>")
    -- map("n", m.code_action, "<cmd>lua vim.lsp.buf.code_action()<CR>")
    -- map("n", m.references, "<cmd>lua vim.lsp.buf.references()<CR>")
    -- map("n", m.float_diagnostics, "<cmd>lua vim.diagnostic.open_float()<CR>")
    -- map("n", m.goto_prev, "<cmd>lua vim.diagnostic.goto_prev()<CR>")
    -- map("n", m.goto_next, "<cmd>lua vim.diagnostic.goto_next()<CR>")
    map("n", m.set_loclist, "<cmd>lua vim.diagnostic.setloclist()<CR>")
    -- map("n", m.formatting, "<cmd>lua vim.lsp.buf.format { async = true }<CR>", { silent = true })

    local keymap = vim.keymap.set
    -- keymap("n", "gd", "<cmd>Lspsaga peek_definition<CR>")
    -- keymap("n", "gD", "<cmd>Lspsaga goto_definition<CR>")
    -- keymap("n", "gr", "<cmd>Lspsaga rename<CR>")
    -- keymap("n", "gt", "<cmd>Lspsaga peek_type_definition<CR>")
    -- keymap("n", "gT", "<cmd>Lspsaga goto_type_definition<CR>")
    -- keymap("n", "gh", "<cmd>Lspsaga lsp_finder<CR>")
end

local function get_capabilities()
    local capabilities = require("cmp_nvim_lsp").default_capabilities()
    capabilities.textDocument.completion.completionItem.documentationFormat = { "markdown", "plaintext" }
    capabilities.textDocument.completion.completionItem.snippetSupport = true
    capabilities.textDocument.completion.completionItem.preselectSupport = true
    capabilities.textDocument.completion.completionItem.insertReplaceSupport = true
    capabilities.textDocument.completion.completionItem.labelDetailsSupport = true
    capabilities.textDocument.completion.completionItem.deprecatedSupport = true
    capabilities.textDocument.completion.completionItem.commitCharactersSupport = true
    capabilities.textDocument.completion.completionItem.tagSupport = { valueSet = { 1 } }
    capabilities.textDocument.completion.completionItem.resolveSupport = {
        properties = {
            "documentation",
            "detail",
            "additionalTextEdits",
        },
    }
end

local function config()
    local lspconfig = require("lspconfig")
    -- local illuminate = require("illuminate")

    require("core.plugins.lsp_handlers")

    local capabilities = get_capabilities()
    vim.lsp.inlay_hint.enable(true)
    -- lspservers with default config
    local servers = {
        "lua_ls",
        "vimls",
        "pyright",
        "ruff",
        -- "rust_analyzer",
        "gopls",
        "html",
        "tsserver",
        -- 'jsonls',
        "volar",
        -- "tailwindcss",
        "texlab",
        "yamlls",
        "cmake",
        -- "rnix",
        "nil_ls",
        "vala_ls",
        "wgsl_analyzer",
        "clangd",
    }
    local option = {
        on_attach = on_attach,
        capabilities = capabilities,
        flags = {
            debounce_text_changes = 150,
        },
    }
    for _, lsp in ipairs(servers) do
        lspconfig[lsp].setup(option)
    end
    lspconfig.nil_ls.setup({
        on_attach = on_attach,
        capabilities = capabilities,
        flags = {
            debounce_text_changes = 150,
        },
        settings = {
            formatting = {
                command = "nixpkgs-fmt",
            },
            ["nil"] = {
                flake = {
                    autoArchive = true,
                    autoEvalInputs = true,
                    nixpkgsInputName = "nixpkgs",
                },
            },
        },
    })
    require("core.plugins.cpp").clangd_config(on_attach, capabilities)
end

return {
    on_attach = on_attach,
    capabilities = get_capabilities,
    {
        "neovim/nvim-lspconfig",
        dir = gen.nvim_lspconfig,
        name = "nvim_lspconfig",
        module = "lspconfig",
        lazy = true,
        event = { "BufRead", "VeryLazy" },
        config = config,
        init = function()
            require("which-key").register({
                l = { name = "LSP" },
                c = { name = "CMake / Cargo" },
            }, { prefix = "<leader>" })
        end,
        keys = {
            {
                "<leader>lf",
                function()
                    vim.lsp.buf.format()
                end,
                desc = "Format",
            },
            {
                "K",
                function()
                    if vim.fn.expand("%:t") == "Cargo.toml" then
                        require("crates").show_popup()
                    else
                        vim.lsp.buf.hover()
                    end
                end,
                mode = "n",
                desc = "Hover",
            },
        },
        dependencies = {
            "none_ls",
        }
    },
    {
        "williamboman/mason.nvim",
        dir = gen.mason_nvim,
        name = "mason_nvim",
        cmd = { "Mason", "MasonUpdate", "MasonInstall", "MasonUninstall", "MasonUninstallAll", "MasonLog" },
        lazy = 0 == vim.fn.has("win32"),
        dependencies = "nvim_lspconfig",
        config = function()
            require("mason").setup({
                -- The directory in which to install packages.
                install_root_dir = vim.fn.stdpath("data") .. "/mason",

                -- Where Mason should put its bin location in your PATH. Can be one of:
                -- - "prepend" (default, Mason's bin location is put first in PATH)
                -- - "append" (Mason's bin location is put at the end of PATH)
                -- - "skip" (doesn't modify PATH)
                ---@type '"prepend"' | '"append"' | '"skip"'
                PATH = "prepend",

                -- Controls to which degree logs are written to the log file. It's useful to set this to vim.log.levels.DEBUG when
                -- debugging issues with package installations.
                log_level = vim.log.levels.INFO,

                -- Limit for the maximum amount of packages to be installed at the same time. Once this limit is reached, any further
                -- packages that are requested to be installed will be put in a queue.
                max_concurrent_installers = 4,

                -- [Advanced setting]
                -- The registries to source packages from. Accepts multiple entries. Should a package with the same name exist in
                -- multiple registries, the registry listed first will be used.
                registries = {
                    "lua:mason-registry.index",
                },

                -- The provider implementations to use for resolving supplementary package metadata (e.g., all available versions).
                -- Accepts multiple entries, where later entries will be used as fallback should prior providers fail.
                -- Builtin providers are:
                --   - mason.providers.registry-api  - uses the https://api.mason-registry.dev API
                --   - mason.providers.client        - uses only client-side tooling to resolve metadata
                providers = {
                    "mason.providers.registry-api",
                    "mason.providers.client",
                },

                github = {
                    -- The template URL to use when downloading assets from GitHub.
                    -- The placeholders are the following (in order):
                    -- 1. The repository (e.g. "rust-lang/rust-analyzer")
                    -- 2. The release version (e.g. "v0.3.0")
                    -- 3. The asset name (e.g. "rust-analyzer-v0.3.0-x86_64-unknown-linux-gnu.tar.gz")
                    download_url_template = "https://github.com/%s/releases/download/%s/%s",
                },

                pip = {
                    -- Whether to upgrade pip to the latest version in the virtual environment before installing packages.
                    upgrade_pip = false,

                    -- These args will be added to `pip install` calls. Note that setting extra args might impact intended behavior
                    -- and is not recommended.
                    --
                    -- Example: { "--proxy", "https://proxyserver" }
                    install_args = {},
                },

                ui = {
                    -- Whether to automatically check for new versions when opening the :Mason window.
                    check_outdated_packages_on_open = true,

                    -- The border to use for the UI window. Accepts same border values as |nvim_open_win()|.
                    border = "rounded",

                    -- Width of the window. Accepts:
                    -- - Integer greater than 1 for fixed width.
                    -- - Float in the range of 0-1 for a percentage of screen width.
                    width = 0.8,

                    -- Height of the window. Accepts:
                    -- - Integer greater than 1 for fixed height.
                    -- - Float in the range of 0-1 for a percentage of screen height.
                    height = 0.9,

                    icons = {
                        -- The list icon to use for installed packages.
                        package_installed = "◍",
                        -- The list icon to use for packages that are installing, or queued for installation.
                        package_pending = "◍",
                        -- The list icon to use for packages that are not installed.
                        package_uninstalled = "◍",
                    },

                    keymaps = {
                        -- Keymap to expand a package
                        toggle_package_expand = "<CR>",
                        -- Keymap to install the package under the current cursor position
                        install_package = "i",
                        -- Keymap to reinstall/update the package under the current cursor position
                        update_package = "u",
                        -- Keymap to check for new version for the package under the current cursor position
                        check_package_version = "c",
                        -- Keymap to update all installed packages
                        update_all_packages = "U",
                        -- Keymap to check which installed packages are outdated
                        check_outdated_packages = "C",
                        -- Keymap to uninstall a package
                        uninstall_package = "X",
                        -- Keymap to cancel a package installation
                        cancel_installation = "<C-c>",
                        -- Keymap to apply language filter
                        apply_language_filter = "<C-f>",
                    },
                },
            })
        end,
    },
}
