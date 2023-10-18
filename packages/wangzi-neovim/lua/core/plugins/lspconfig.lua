local function config()
    local num_of_processers = 16
    if vim.fn.has("win32") > 0 then
        num_of_processers = tonumber(vim.env.NUMBER_OF_PROCESSORS or 0)
    elseif vim.fn.has("unix") > 0 then
        local handle = io.popen("nproc")
        local result = handle:read("*a")
        handle:close()
        num_of_processers = tonumber(result or 0)
    end
    local num_of_job = num_of_processers
    if num_of_processers > 4 then
        num_of_job = num_of_processers - 4
    end
    local lspconfig = require("lspconfig")
    -- local illuminate = require("illuminate")
    local function setup_lsp(attach, capabilities)
        -- lspservers with default config
        local servers = {
            "lua_ls",
            "vimls",
            "pyright",
            "rust_analyzer",
            "gopls",
            "html",
            "tsserver",
            -- 'jsonls',
            "volar",
            -- "tailwindcss",
            "texlab",
            "yamlls",
            "cmake",
            "rnix",
            "vala_ls",
            -- "clangd",
        }
        local option = {
            on_attach = function(client)
                attach(client)
                -- illuminate.on_attach(client)
            end,
            capabilities = capabilities,
            flags = {
                debounce_text_changes = 150,
            },
        }
        for _, lsp in ipairs(servers) do
            lspconfig[lsp].setup(option)
        end
        require("clangd_extensions").setup {
            server = {
                on_attach = function(client)
                    attach(client)
                    -- illuminate.on_attach(client)
                end,
                capabilities = capabilities,
                flags = {
                    debounce_text_changes = 150,
                },
                -- cmd = { "clangd", "--background-index", "--pch-storage=disk", "-j=" .. tostring(num_of_job) }
                cmd = { "clangd", "--background-index", "--pch-storage=disk", "--log=error",
                    num_of_job ~= 0 and "-j=" .. tostring(num_of_job) or nil }
            },
            extensions = {
                -- defaults:
                -- Automatically set inlay hints (type hints)
                autoSetHints = true,
                -- These apply to the default ClangdSetInlayHints command
                inlay_hints = {
                    -- Only show inlay hints for the current line
                    only_current_line = false,
                    -- Event which triggers a refersh of the inlay hints.
                    -- You can make this "CursorMoved" or "CursorMoved,CursorMovedI" but
                    -- not that this may cause  higher CPU usage.
                    -- This option is only respected when only_current_line and
                    -- autoSetHints both are true.
                    only_current_line_autocmd = "CursorHold",
                    -- whether to show parameter hints with the inlay hints or not
                    show_parameter_hints = true,
                    -- prefix for parameter hints
                    parameter_hints_prefix = "<- ",
                    -- prefix for all the other hints (type, chaining)
                    other_hints_prefix = "=> ",
                    -- whether to align to the length of the longest line in the file
                    max_len_align = false,
                    -- padding from the left if max_len_align is true
                    max_len_align_padding = 1,
                    -- whether to align to the extreme right or not
                    right_align = false,
                    -- padding from the right if right_align is true
                    right_align_padding = 7,
                    -- The color of the hints
                    highlight = "Comment",
                    -- The highlight group priority for extmark
                    priority = 100,
                },
                ast = {
                    role_icons = {
                        type = "",
                        declaration = "",
                        expression = "",
                        specifier = "",
                        statement = "",
                        ["template argument"] = "",
                    },
                    kind_icons = {
                        Compound = "",
                        Recovery = "",
                        TranslationUnit = "",
                        PackExpansion = "",
                        TemplateTypeParm = "",
                        TemplateTemplateParm = "",
                        TemplateParamObject = "",
                    },

                    highlights = {
                        detail = "Comment",
                    },
                },
                memory_usage = {
                    border = "rounded",
                },
                symbol_info = {
                    border = "rounded",
                },
            },
        }
    end

    require("core.plugins.lsp_handlers")

    local lsp_formatting = function(bufnr)
        vim.lsp.buf.format({
            filter = function(client)
                -- apply whatever logic you want (in this example, we'll only use null-ls)
                return client.name == "null-ls"
            end,
            bufnr = bufnr,
        })
    end
    local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

    local function on_attach(client, bufnr)
        if client.supports_method("textDocument/formatting") then
            vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
            vim.api.nvim_create_autocmd("BufWritePre", {
                group = augroup,
                buffer = bufnr,
                callback = function()
                    local old_print = vim.notify
                    vim.notify = function(...)
                    end
                    lsp_formatting(bufnr)
                    vim.notify = old_print
                end,
            })
        end

        if client.supports_method("textDocument/codeLens") then
            require('virtualtypes').on_attach(client, bufnr)
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
        map("n", m.hover, "<cmd>lua vim.lsp.buf.hover()<CR>")
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

    local capabilities = require('cmp_nvim_lsp').default_capabilities()
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
    setup_lsp(on_attach, capabilities)
end

local function setup_lspsaga()
    require("lspsaga").setup({
        outline = {
            win_position = "right",
            win_with = "",
            win_width = 30,
            show_detail = true,
            auto_preview = true,
            auto_refresh = true,
            auto_close = true,
            custom_sort = nil,
            keys = {
                jump = "<CR>",
                expand_collapse = "u",
                quit = "q",
            },
        },
        finder = {
            --percentage
            max_height = 1.0,
            force_max_height = false,
            keys = {
                jump_to = 'l',
                edit = { 'o', '<CR>' },
                vsplit = 's',
                split = 'i',
                tabe = 't',
                tabnew = 'r',
                quit = { 'q', '<ESC>' },
                close_in_preview = '<ESC>'
            },
        },
        ui = {
            -- This option only works in Neovim 0.9
            title = true,
            -- Border type can be single, double, rounded, solid, shadow.
            border = "single",
            winblend = 0,
            expand = "",
            collapse = "",
            code_action = "",
            incoming = " ",
            outgoing = " ",
            hover = ' ',
            kind = {},
        },
    })
    vim.api.nvim_create_autocmd({ "VimResized" }, {
        pattern = "lspsagaoutline",
        callback = function(args)
            vim.notify("resize")
            local alltabpages = vim.api.nvim_list_tabpages();
            local window = nil
            for _, tabpage in ipairs(alltabpages) do
                local winlist = vim.api.nvim_tabpage_list_wins(tabpage)
                for _, win in ipairs(winlist) do
                    local buf = vim.api.nvim_win_get_buf(win)
                    if (buf == args.buf) then
                        window = win
                        vim.api.nvim_set_current_win(win)
                        break
                    end
                end
                if window then
                    break
                end
            end
            local width = vim.api.nvim_win_get_width(window)
            if width > 32 then
                vim.api.nvim_win_set_width(32)
            end
        end,
    })
end

return {
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
                c = { name = "CMake / Cargo", },
            }, { prefix = "<leader>" })
        end,
        keys = {
            { "<leader>lf", function() vim.lsp.buf.format() end, desc = "Format", },
            {
                "K",
                function()
                    if vim.fn.expand('%:t') == "Cargo.toml" then
                        require('crates').show_popup()
                    else
                        vim.lsp.buf.hover()
                    end
                end,
                mode = "n",
                desc = "Hover"
            },
        },
        dependencies = {
            {
                "ray-x/lsp_signature.nvim",
                dir = gen.lsp_signature_nvim,
                name = "lsp_signature_nvim",
                lazy = true,
                config = function()
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
                        max_width = 120,        -- max_width of signature floating_window, line will be wrapped if exceed max_width
                        handler_opts = {
                            border = "rounded", -- double, single, shadow, none
                        },
                        zindex = 200,           -- by default it will be on top of all floating windows, set to 50 send it to bottom
                        padding = "",           -- character to pad on left and right of signature can be ' ', or '|'  etc
                    })
                end,
            },
            {
                "jose-elias-alvarez/null-ls.nvim",
                dir = gen.null_ls,
                name = "null_ls",
                lazy = true,
                config = function()
                    require("core.plugins.null_ls")
                end,
            },
            {
                "jubnzv/virtual-types.nvim",
                dir = gen.virtual_types_nvim,
                name = "virtual_types_nvim",
                event = "LspAttach",
                module = "virtualtypes",
                config = function()
                    require("core.plugins.null_ls")
                end,
            },
            {
                "glepnir/lspsaga.nvim",
                dir = gen.lspsaga,
                name = "lspsaga",
                event = "LspAttach",
                dependencies = { "nvim_web_devicons" },
                config = setup_lspsaga,
                cmd = "Lspsaga",
                module = "lspsaga",
                keys = {
                    { "[d", "<cmd>Lspsaga diagnostic_jump_prev<CR>", mode = "n", desc = "Previous Diagnostic" },
                    { "]d", "<cmd>Lspsaga diagnostic_jump_next<CR>", mode = "n", desc = "Next Diagnostic" },
                    {
                        "[e",
                        function()
                            require("lspsaga.diagnostic"):goto_prev({ severity = vim.diagnostic.severity.ERROR })
                        end,
                        mode = "n",
                        desc = "Previous Error"
                    },
                    {
                        "]e",
                        function()
                            require("lspsaga.diagnostic"):goto_next({ severity = vim.diagnostic.severity.ERROR })
                        end,
                        mode = "n",
                        desc = "Next Error"
                    },
                    {
                        "ge",
                        function()
                            require("lspsaga.diagnostic"):goto_next({ severity = vim.diagnostic.severity.ERROR })
                        end,
                        mode = "n",
                        desc = "Diagnostic"
                    },
                    { "gd", "<cmd>Lspsaga peek_definition<CR>",      mode = "n", desc = "Peek Definition" },
                    {
                        "gD",
                        function()
                            require("trailblazer").new_trail_mark()
                            vim.cmd [[Lspsaga goto_definition]]
                        end,
                        mode = "n",
                        desc = "Goto Definition"
                    },
                    { "gr", "<cmd>Lspsaga rename<CR>",               mode = "n", desc = "Rename" },
                    { "gt", "<cmd>Lspsaga peek_type_definition<CR>", mode = "n", desc = "Peek Type Definition" },
                    { "gT", "<cmd>Lspsaga goto_type_definition<CR>", mode = "n", desc = "Goto Type Definition" },
                    {
                        "gh",
                        function()
                            require("trailblazer").new_trail_mark()
                            vim.cmd [[Lspsaga lsp_finder]]
                        end,
                        mode = "n",
                        desc = "LSP Finder"
                    },
                    -- { "K", "<cmd>Lspsaga hover_doc<CR>", mode = "n", desc = "Hover" },
                    {
                        "<A-d>",
                        "<cmd>Lspsaga term_toggle<CR>",
                        mode = { "n", "t" },
                        desc =
                        "Lspsaga Terminal"
                    },

                    { "<leader>la", "<cmd>Lspsaga code_action<CR>",          desc = "CodeActions", },
                    { "<leader>lr", "<cmd>Lspsaga rename<CR>",               desc = "Rename", },
                    { "<leader>ld", "<cmd>Lspsaga peek_definition<CR>",      desc = "PreviewDefinition", },
                    { "<leader>lD", "<cmd>Lspsaga peek_type_definition<CR>", desc = "PreviewDefinition", },
                    { "<leader>lo", "<cmd>Lspsaga outline<CR>",              desc = "Outline", },
                    { "<leader>lc", "<cmd>Lspsaga incoming_calls<CR>",       desc = "Incoming call" },
                    { "<leader>lC", "<cmd>Lspsaga outgoing_calls<CR>",       desc = "Outgoing call" },
                    { "<leader>lt", "<cmd>Lspsaga term_toggle<CR>",          desc = "Terminal" },
                    { "<leader>lh", "<cmd>Lspsaga lsp_finder<CR>",           desc = "finder", }
                }
            },
        },
    },
    {
        "simrat39/rust-tools.nvim",
        dir = gen.rust_tools,
        name = "rust_tools",
        dependencies = "nvim_lspconfig",
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
                        show_variable_name = true,
                    },
                },
                dap = {
                    adapter = adapter
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
                desc = "Cargo actio"
            },
        },
        ft = { "rs", "rust", "toml" },
    },
    {
        "p00f/clangd_extensions.nvim",
        dir = gen.clangd_extensions_nvim,
        name = "clangd_extensions_nvim",
        ft = { "h", "cpp", "cc", "c" },
        dependencies = "nvim_lspconfig",
        module = "clangd_extensions",
        lazy = true,
        config = function()
        end,
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
    {
        "Shatur/neovim-cmake",
        dir = gen.cmake,
        name = "cmake",
        cmd = { "CMake" },
        dependencies = "nvim_lspconfig",
        ft = { "cpp", "c", "hpp", "h", "CMakeLists.txt" },
        lazy = true,
        config = function()
            require("core.plugins.cmake")
        end,
        keys = {
            { "<leader>cm", function() vim.cmd ":CMake<CR>" end, desc = "CMake" },
            { "<leader>cc", ":CMake configure<CR>",              desc = "CMake configure" },
            { "<leader>cC", ":CMake clean<CR>",                  desc = "CMake clean" },
            { "<leader>cr", ":CMake build_and_run<CR>",          desc = "CMake run" },
            { "<leader>cd", ":CMake build_and_debug<CR>",        desc = "CMake debug" },
            { "<leader>ct", ":CMake select_build_type<CR>",      desc = "CMake build type" },
            { "<leader>cs", ":CMake select_target<CR>",          desc = "CMake select target" },
            { "<leader>cB", ":CMake build_all<CR>",              desc = "CMake build all" },
            { "<leader>cb", ":CMake build<CR>",                  desc = "CMake build" },
        }
    },
    {
        "saecki/crates.nvim",
        dir = gen.crates_nvim,
        name = "crates_nvim",
        dependencies = { "nvim_lspconfig", 'plenary_nvim' },
        ft = { "toml" },
        module = "crates",
        lazy = true,
        config = function()
            require("core.plugins.crates")
        end,
        keys = {
            { "<leader>cu", function() require('crates').upgrade_crate(nil) end,   desc = "Cargo Upgrade " },
            { "<leader>cU", function() require('crates').upgrade_crates(nil) end,  desc = "Cargo Upgrade Crates" },
            { "<leader>ch", function() require('crates').open_homepage() end,      desc = "Crate Homepage" },
            { "<leader>cR", function() require('crates').open_repository() end,    desc = "Crate Repository" },
            { "<leader>cD", function() require('crates').open_documentation() end, desc = "Crate Documentation" },
            { "<leader>ci", function() require('crates').open_crates_io() end,     desc = "crate.io" }
        }
    },
}
