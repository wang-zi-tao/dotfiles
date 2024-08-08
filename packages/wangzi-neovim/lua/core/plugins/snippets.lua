return {
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
            require("luasnip/loaders/from_vscode").load({ paths = paths, })
            local ls = require("luasnip")
            local s = ls.snippet
            local sn = ls.snippet_node
            local t = ls.text_node
            local i = ls.insert_node
            local f = ls.function_node
            local c = ls.choice_node
            local d = ls.dynamic_node
            local r = ls.restore_node
            local l = require("luasnip.extras").lambda
            local rep = require("luasnip.extras").rep
            local p = require("luasnip.extras").partial
            local m = require("luasnip.extras").match
            local n = require("luasnip.extras").nonempty
            local dl = require("luasnip.extras").dynamic_lambda
            local fmt = require("luasnip.extras.fmt").fmt
            local fmta = require("luasnip.extras.fmt").fmta
            local types = require("luasnip.util.types")
            local conds = require("luasnip.extras.expand_conditions")

            ls.config.set_config({
                history = true,
                updateevents = "TextChanged,TextChangedI",
            })

            local newline = function()
                return t { "", "" }
            end

            ls.add_snippets("markdown", {
                s("class_parent", { i(1, "class"), t(" --|> "), i(2, "parent_class"), newline() }),
                s("class_interface", { i(1, "class"), t(" ..|> "), i(2, "interface"), newline() }),
                s("class_reference", { i(1, "class"), t(" --o "), i(2, "class2"), newline() }),
                s("class_has", { i(1, "class"), t(" --* "), i(2, "class2"), newline() }),
                s("class_connect", { i(1, "class"), t(" --> "), i(2, "class2"), t(" "), i(3), newline() }),

                s("ts_call", {
                    i(1, "class1"), t(" --> "), i(2, "class2"), t(" : "), i(3, "function"), t("("), i(4, ""), t({ ")", "" }),
                    t("activate "), rep(2), newline(),
                    t("\t"), newline(),
                    rep(1), t(" <-- "), rep(2), t(" : return "), i(4, ""), newline(),
                    t("deactivate "), rep(2), newline(),
                }),
                s("ts_message", { i(1, "class"), t(" --> "), i(2, "class2"), t(" : "), i(3, "message"), newline() }),
            })

            require("luasnip.loaders.from_vscode").load({})
        end,
    },
}
