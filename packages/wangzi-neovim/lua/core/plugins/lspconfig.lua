local function on_attach(client, bufnr)
    require("nvim-navbuddy").attach(client, bufnr)
    require("lsp-format").on_attach(client, bufnr)
    require("core.plugins.lsp").lsp_signature_on_attach(bufnr)

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
    -- map("n", "K", "<cmd>Lspsaga hover_doc<CR>")
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

    if vim.fn.has("win32") == 0 then
        vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
    end
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
    local lspconfig_util = require("lspconfig.util")
    -- local illuminate = require("illuminate")
    require("core.plugins.lsp_handlers")
    local num_of_job = require("core.utils").num_of_core()

    local function setup_lsp(lsp_name, opts)
        local opts = opts or {}
        lspconfig[lsp_name].setup({
            on_attach = on_attach,
            flags = {
                debounce_text_changes = 150,
            },
            settings = opts.settings,
            cmd = opts.cmd,
        })
    end

    -- lspservers with default config
    local servers = {
        "lua_ls",
        "vimls",
        "pyright",
        "jedi_language_server",
        -- "ruff",
        -- "rust_analyzer",
        "gopls",
        "html",
        "ts_ls",
        "jsonls",
        "eslint",
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
        "csharp_ls",
        "cssls",
    }
    for _, lsp in ipairs(servers) do
        setup_lsp(lsp)
    end
    require("core.plugins.cpp").clangd_config(on_attach, nil)
    setup_lsp("java_language_server", {
        cmd = { "java-language-server" }
    })
    setup_lsp("clangd", {
        cmd = {
            "clangd",
            "--background-index",
            "--pch-storage=disk",
            "--log=error",
            num_of_job ~= 0 and "-j=" .. tostring(num_of_job) or nil,
        },
        root_dir = lspconfig_util.root_pattern(
            "wps_3rdparty_list.cmake",
            '.clang-tidy',
            '.clang-format',
            'compile_commands.json',
            'compile_flags.txt',
            'configure.ac'
        ),
    })
    setup_lsp("nil_ls", {
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
            require("which-key").add({
                { "<leader>l", group = "LSP" },
                { "<leader>c", group = "CMake / Cargo" },
            })
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
            "lsp_signature_nvim",
        }
    },
    {
        "williamboman/mason.nvim",
        dir = gen.mason_nvim,
        name = "mason_nvim",
        cmd = { "Mason", "MasonUpdate", "MasonInstall", "MasonUninstall", "MasonUninstallAll", "MasonLog" },
        lazy = 0 == vim.fn.has("win32"),
        dependencies = {
            "nvim_lspconfig",
            "jay-babu/mason-nvim-dap.nvim",
            "WhoIsSethDaniel/mason-tool-installer.nvim",
            {
                "jay-babu/mason-null-ls.nvim",
                event = { "BufReadPre", "BufNewFile" },
                dependencies = {
                    "none_ls",
                },
            }
        },
        config = function()
            require("mason").setup({
                registries = {
                    "lua:mason-registry.index",
                },
                providers = {
                    "mason.providers.registry-api",
                    "mason.providers.client",
                },

                github = {
                    download_url_template = "https://github.com/%s/releases/download/%s/%s",
                },

                pip = {
                    upgrade_pip = true,
                    install_args = {},
                },

                ui = {
                    check_outdated_packages_on_open = true,
                    border = "rounded",
                    width = 0.8,
                    height = 0.9,
                },
            })
        end,
    },
}
