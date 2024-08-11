local function config()
    local cmp = require("cmp")
    local luasnip = require("luasnip")
    local cmp_lsp_rs = require("cmp_lsp_rs")
    local compare = cmp.config.compare
    local comparators = cmp_lsp_rs.comparators

    local has_words_before = function()
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
    end

    local feedkey = function(key, mode)
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, true)
    end
    cmp.setup({
        snippet = {
            expand = function(args)
                luasnip.lsp_expand(args.body)
            end,
        },
        window = {
            -- completion = cmp.config.window.bordered(),
            -- documentation = cmp.config.window.bordered(),
        },
        formatting = {
            format = function(entry, vim_item)
                local icons = require("core.theme").symbols.lsp
                local icon = icons[vim_item.kind] or {}
                vim_item.kind = string.format("%s %s", icon.icon or "", vim_item.kind)
                vim_item.menu = ({
                    buffer = " ",
                    nvim_lsp = " ",
                    nvim_lua = " ",
                    path = " ",
                    cmp_tabnine = " ",
                })[entry.source.name]
                if entry.source.name == "cmp_tabnine" then
                    local detail = (entry.completion_item.labelDetails or {}).detail
                    vim_item.kind = " "
                    if detail and detail:find(".*%%.*") then
                        vim_item.kind = vim_item.kind .. " " .. detail
                    end

                    if (entry.completion_item.data or {}).multiline then
                        vim_item.kind = vim_item.kind .. " " .. "[ML]"
                    end
                end

                return vim_item
            end,
        },
        mapping = cmp.mapping.preset.insert({
            ["<C-b>"] = cmp.mapping.scroll_docs(-4),
            ["<C-f>"] = cmp.mapping.scroll_docs(4),
            ["<C-u>"] = cmp.mapping.scroll_docs(-4),
            ["<C-d>"] = cmp.mapping.scroll_docs(4),
            ["<C-j>"] = cmp.mapping.scroll_docs(-1),
            ["<C-k>"] = cmp.mapping.scroll_docs(1),
            ["<C-Space>"] = cmp.mapping.complete(),
            ["<C-e>"] = cmp.mapping.abort(),
            ["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
            ["<Tab>"] = cmp.mapping(function(fallback)
                if cmp.visible() then
                    cmp.select_next_item()
                elseif luasnip.expand_or_jumpable() then
                    luasnip.expand_or_jump()
                elseif has_words_before() then
                    cmp.complete()
                else
                    fallback()
                end
            end, { "i", "s" }),
            ["<S-Tab>"] = cmp.mapping(function(fallback)
                if cmp.visible() then
                    cmp.select_prev_item()
                elseif luasnip.jumpable(-1) then
                    luasnip.jump(-1)
                else
                    fallback()
                end
            end, { "i", "s" }),
        }),
        sources = cmp.config.sources({
            { name = "nvim_lsp" },
            { name = "luasnip" }, -- For luasnip users.
            { name = "nvim_lua" },
            { name = "cmp_tabnine" },
            { name = "spell" },
            { name = "path" },
            { name = "zsh" },
            { name = "git" },
            { name = "crates" },
        }, {
            { name = "buffer", keyword_length = 3 },
        }),
        sorting = {
            comparators = {
                compare.exact,
                compare.score,
                -- comparators.inherent_import_inscope,
                comparators.inscope_inherent_import,
                comparators.sort_by_label_but_underscore_last,
            }
        }
    })

    -- Set configuration for specific filetype.
    cmp.setup.filetype("gitcommit", {
        sources = cmp.config.sources({
            { name = "cmp_git" }, -- You can specify the `cmp_git` source if you were installed it.
            { name = "buffer" },
        }),
    })

    -- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
    cmp.setup.cmdline({ "/", "?" }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
            { name = "buffer" },
        },
    })

    -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
    cmp.setup.cmdline(":", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
            { name = "cmdline" },
            { name = "path" },
            { name = "cmdline_history" },
        }),
    })

    local cmp_autopairs = require("nvim-autopairs.completion.cmp")
    cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())
    -- cmp.register_source("buffer", require("cmp_buffer"))
end

local tabnine_build
if 0 == vim.fn.has("win32") then
    tabnine_build = "./install.sh"
else
    tabnine_build = "powershell ./install.ps1"
end
return {
    "hrsh7th/nvim-cmp",
    dir = gen.nvim_cmp,
    name = "nvim_cmp",
    module = "cmp",
    lazy = true,
    config = config,
    event = { "VeryLazy" },
    dependencies = {
        "friendly_snippets",
        {
            "rafamadriz/friendly-snippets",
            dir = gen.friendly_snippets,
            name = "friendly_snippets",
            module = "cmp_nvim_lsp",
            lazy = true,
        },
        {
            "l3mon4d3/luasnip",
            dir = gen.luasnip,
            name = "luasnip",
            module = "luasnip",
            dependencies = "friendly_snippets",
            lazy = true,
            config = function()
                require("luasnip").config.set_config({
                    history = true,
                    updateevents = "TextChanged,TextChangedI",
                })
                require("luasnip/loaders/from_vscode").load({
                    paths = {
                        "./rust.json",
                        "./c++.json",
                    },
                })
                require("luasnip/loaders/from_vscode").load()
            end,
        },

        {
            "saadparwaiz1/cmp_luasnip",
            dir = gen.cmp_luasnip,
            name = "cmp_luasnip",
            dependencies = "luasnip",
            lazy = true,
            config = function()
                require("cmp").register_source("luasnip", require("cmp_luasnip").new())
            end,
        },

        {
            "hrsh7th/cmp-nvim-lua",
            dir = gen.cmp_nvim_lua,
            name = "cmp_nvim_lua",
            dependencies = "cmp_luasnip",
            lazy = true,
            config = function()
                require("cmp").register_source("nvim_lua", require("cmp_nvim_lua").new())
            end,
        },

        {
            "hrsh7th/cmp-nvim-lsp",
            dir = gen.cmp_nvim_lsp,
            name = "cmp_nvim_lsp",
            dependencies = "nvim_lspconfig",
            module = "cmp_nvim_lsp",
            lazy = true,
            config = function()
                require("cmp_nvim_lsp").setup()
            end,
        },
        {
            "hrsh7th/cmp-buffer",
            dir = gen.cmp_buffer,
            name = "cmp_buffer",
            module = "cmp_buffer",
            lazy = true,
            config = function()
                require("cmp").register_source("buffer", require("cmp_buffer").new())
            end,
        },
        {
            "f3fora/cmp-spell",
            dir = gen.cmp_spell,
            name = "cmp_spell",
            lazy = true,
            config = function()
                require("cmp").register_source("spell", require("cmp-spell").new())
            end,
        },
        {
            "hrsh7th/cmp-cmdline",
            dir = gen.cmp_cmdline,
            name = "cmp_cmdline",
            lazy = true,
            config = function()
                require("cmp").register_source("cmdline", require("cmp_cmdline").new())
            end,
        },
        {
            "dmitmel/cmp-cmdline-history",
            dir = gen.cmp_cmdline_history,
            name = "cmp_cmdline_history",
            lazy = true,
            config = function()
                require("cmp").register_source("cmdline_history", require("cmp_cmdline_history").new())
            end,
        },
        {
            "hrsh7th/cmp-path",
            dir = gen.cmp_path,
            name = "cmp_path",
            dependencies = "cmp_buffer",
            lazy = true,
            config = function()
                require("cmp").register_source("path", require("cmp_path").new())
            end,
        },
        {
            "tamago324/cmp-zsh",
            dir = gen.cmp_zsh,
            name = "cmp_zsh",
            dependencies = "cmp_buffer",
            lazy = true,
            config = function()
                require("cmp").register_source("zsh", require("cmp_zsh").new())
                require("cmp_zsh").setup({
                    zshrc = true,                      -- Source the zshrc (adding all custom completions). default: false
                    filetypes = { "deoledit", "zsh" }, -- Filetypes to enable cmp_zsh source. default: {"*"}
                })
            end,
        },
        {
            "petertriho/cmp-git",
            dir = gen.cmp_git,
            name = "cmp_git",
            dependencies = "cmp_buffer",
            lazy = true,
            config = function()
                require("core.plugins.cmp_git")
            end,
        },
        gen.cmp_tabnine
        and {
            "tzachar/cmp-tabnine",
            dir = gen.cmp_tabnine,
            name = "cmp_tabnine",
            build = tabnine_build,
            -- module = "cmp_tabnine",
            disable = gen.cmp_tabnine ~= false,
            lazy = true,
            config = function()
                require("cmp_tabnine").setup()
                require("cmp_tabnine.config"):setup({
                    max_lines = 1000,
                    max_num_results = 20,
                    sort = true,
                    run_on_every_keystroke = false,
                    snippet_placeholder = "..",
                    ignored_file_types = { -- default is not to ignore
                        -- uncomment to ignore in lua:
                        -- lua = true
                    },
                })
                local cmp_tabnine = require("cmp_tabnine")
                local prefetch = vim.api.nvim_create_augroup("prefetch", { clear = true })
                vim.api.nvim_create_autocmd("BufRead", {
                    group = prefetch,
                    pattern = "*.h",
                    callback = function()
                        cmp_tabnine:prefetch(vim.fn.expand("%:p"))
                    end,
                })
                vim.api.nvim_create_autocmd("BufRead", {
                    group = prefetch,
                    pattern = "*.cpp",
                    callback = function()
                        cmp_tabnine:prefetch(vim.fn.expand("%:p"))
                    end,
                })
                local compare = require("cmp.config.compare")
                require("cmp").setup({
                    sorting = {
                        priority_weight = 2,
                        comparators = {
                            require("cmp_tabnine.compare"),
                            compare.offset,
                            compare.exact,
                            compare.score,
                            compare.recently_used,
                            compare.kind,
                            compare.sort_text,
                            compare.length,
                            compare.order,
                        },
                    },
                })
            end,
        }
        or {},
        {
            "zjp-CN/nvim-cmp-lsp-rs",
            dir = gen.nvim_cmp_lsp_rs,
            name = "nvim_cmp_lsp_rs",
            module = "cmp_lsp_rs",
            ---@type cmp_lsp_rs.Opts
            opts = {
                -- Filter out import items starting with one of these prefixes.
                -- A prefix can be crate name, module name or anything an import
                -- path starts with, no matter it's complete or incomplete.
                -- Only literals are recognized: no regex matching.
                unwanted_prefix = { "color", "ratatui::style::Styled" },
                -- make these kinds prior to others
                -- e.g. make Module kind first, and then Function second,
                --      the rest ordering is merged from a default kind list
                kind = function(k)
                    -- The argument in callback is type-aware with opts annotated,
                    -- so you can type the CompletionKind easily.
                    return { k.Module, k.Function }
                end,
                -- Override the default comparator list provided by this plugin.
                -- Mainly used with key binding to switch between these Comparators.
                combo = {
                    -- The key is the name for combination of comparators and used
                    -- in notification in swiching.
                    -- The value is a list of comparators functions or a function
                    -- to generate the list.
                    alphabetic_label_but_underscore_last = function()
                        local comparators = require("cmp_lsp_rs").comparators
                        return { comparators.sort_by_label_but_underscore_last }
                    end,
                    recentlyUsed_sortText = function()
                        local compare = require("cmp").config.compare
                        local comparators = require("cmp_lsp_rs").comparators
                        -- Mix cmp sorting function with cmp_lsp_rs.
                        return {
                            compare.recently_used,
                            compare.sort_text,
                            comparators.sort_by_label_but_underscore_last
                        }
                    end,
                },
            },
        },
    },
}
