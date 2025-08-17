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
            require("luasnip/loaders/from_vscode").lazy_load()
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
            local postfix = require "luasnip.extras.postfix".postfix

            ls.config.set_config({
                history = true,
                updateevents = "TextChanged,TextChangedI",
            })

            local newline = function()
                return t { "", "" }
            end

            ls.add_snippets("rs", {
                s("default", { t("Default::default()") }),
                s("Query_arg", {
                    i(1, "arg_name"), t(": Query<("), i(2, "arg_type"), t(")>,"),
                }),
                postfix("Option", {
                    f(function(_, snip)
                        return "Option<" .. snip.captures[1] .. ">"
                    end, { 1 }),
                }),
                postfix("Vec", {
                    f(function(_, snip)
                        return "Vec<" .. snip.captures[1] .. ">"
                    end, { 1 }),
                }),
                postfix("anyhow_Result", {
                    f(function(_, snip)
                        return "anyhow::Result<" .. snip.captures[1] .. ">"
                    end, { 1 }),
                }),
            })

            ls.add_snippets("markdown", {
                s("class_parent", { i(1, "class"), t(" --|> "), i(2, "parent_class"), newline() }),
                s("class_interface", { i(1, "class"), t(" ..|> "), i(2, "interface"), newline() }),
                s("class_reference", { i(1, "class"), t(" --o "), i(2, "class2"), newline() }),
                s("class_has", { i(1, "class"), t(" --* "), i(2, "class2"), newline() }),
                s("class_connect", { i(1, "class"), t(" --> "), i(2, "class2"), t(" "), i(3), newline() }),

                s("ts_call", {
                    i(1, "class1"), t(" -> "), i(2, "class2"), t(" ++ : "), i(3, "function"), t("("), i(4, ""), t({ ")",
                    "" }),
                    t("\t"), newline(),
                    t("return "), newline(),
                }),
                s("ts_call_this", {
                    i(1, "class1"), t(" -> "), i(2, "class2"), t(" ++ : "), i(3, "function"), t("("), i(4, ""), t({ ")",
                    "" }),
                    t("\t"), newline(),
                }),
                s("ts_message", { i(1, "class"), t(" -> "), i(2, "class2"), t(" : "), i(3, "message"), newline() }),
                s("ts_event", {
                    t("[-> "), i(1, "class"), t(" : "), i(2, "function"), t("("), i(3, ""), t({ ")", "" }),
                    t("activate "), rep(1), newline(),
                    t("\t"), newline(),
                    t("[<- "), rep(1), t(" : return "), i(4, ""), newline(),
                    t("deactivate "), rep(1), newline(),
                }),
                s("ts_alt", {
                    t("alt"), i(1), newline(),
                    t("\t"), i(2), newline(),
                    t("end"), newline(),
                }),
                s("ts_alt_else", {
                    t("alt"), i(1), newline(),
                    t("\t"), i(2), newline(),
                    t("else"), newline(),
                    t("\t"), i(3), newline(),
                    t("end"), newline(),
                }),
                s("ts_loop", {
                    t("loop"), i(1), newline(),
                    t("\t"), i(2), newline(),
                    t("end"), newline(),
                }),
            })

            ls.add_snippets("cpp", {
                s("jsapi_h", {
                    t("struct "), i(1, "class"), newline(),
                    t("{"), newline(),
                    t("static HRESULT GetInMainThread(bool bCefEngine,"), newline(),
                    t("\tconst JSContextEnv& jsEnv,"), newline(),
                    t("\tint nAttributeIdx,"), newline(),
                    t("\tJSInputData inputData,"), newline(),
                    t("\tKJSVariant& retval,"), newline(),
                    t("\tJSApiObject* pJSApiObj,"), newline(),
                    t("\tKJSVariant& exception);"), newline(),
                    newline(),
                    t("static HRESULT SetInMainThread(const JSContextEnv& jsEnv,"), newline(),
                    t("\tint nAttributeIdx,"), newline(),
                    t("\tJSInputData inputData,"), newline(),
                    t("\tKJSVariant& value,"), newline(),
                    t("\tKJSVariant& exception);"), newline(),
                    newline(),
                    t("static HRESULT ExecuteInMainThread(bool bCefEngine,"), newline(),
                    t("\tconst JSContextEnv& jsEnv,"), newline(),
                    t("\tint nAttributeIdx,"), newline(),
                    t("\tJSInputData inputData,"), newline(),
                    t("\tJSApiArguments& arguments,"), newline(),
                    t("\tKJSVariant& retval,"), newline(),
                    t("\tJSApiObject* pJSApiObj,"), newline(),
                    t("\tKJSVariant& exception);"), newline(),
                    newline(),
                    t("static void GetProperties(JSInputData inputData,"), newline(),
                    t("\tconst JSContextEnv& jsEnv,"), newline(),
                    t("\tFnJSAPIGet& fnGet,"), newline(),
                    t("\tFnJSAPISet& fnSet,"), newline(),
                    t("\tFnJSAPIExecute& fnExecute,"), newline(),
                    t("\tIJSPropertyList* pProps);"), newline(),
                    t("};"), newline()
                }),
                s("jsapi_cpp", {
                    t("enum {"), newline(),
                    t("\t"), i(1, "class"), t("_API,"), newline(),
                    t("};"), newline(),
                    newline(),
                    t("HRESULT "), rep(1), t("::GetInMainThread(bool bCefEngine,"), newline(),
                    t("\tconst JSContextEnv& jsEnv,"), newline(),
                    t("\tint nAttributeIdx,"), newline(),
                    t("\tJSInputData inputData,"), newline(),
                    t("\tKJSVariant& retval,"), newline(),
                    t("\tJSApiObject* pJSApiObj,"), newline(),
                    t("\tKJSVariant& exception)"), newline(),
                    t("{"), newline(),
                    t("\tswitch (nAttributeIdx)"), newline(),
                    t("\t{"), newline(),
                    t("\tdefault:"), newline(),
                    t("\t\tbreak;"), newline(),
                    t("\t}"), newline(),
                    t("\treturn S_OK;"), newline(),
                    t("}"), newline(),
                    newline(),
                    t("HRESULT "), rep(1), t("::SetInMainThread(const JSContextEnv& jsEnv,"), newline(),
                    t("\tint nAttributeIdx,"), newline(),
                    t("\tJSInputData inputData,"), newline(),
                    t("\tKJSVariant& value,"), newline(),
                    t("\tKJSVariant& exception)"), newline(),
                    t("{"), newline(),
                    t("\tswitch (nAttributeIdx)"), newline(),
                    t("\t{"), newline(),
                    t("\tdefault:"), newline(),
                    t("\t\tbreak;"), newline(),
                    t("\t}"), newline(),
                    t("\treturn S_OK;"), newline(),
                    t("}"), newline(),
                    newline(),
                    t("HRESULT "), rep(1), t("::ExecuteInMainThread(bool bCefEngine,"), newline(),
                    t("\tconst JSContextEnv& jsEnv,"), newline(),
                    t("\tint nAttributeIdx,"), newline(),
                    t("\tJSInputData inputData,"), newline(),
                    t("\tJSApiArguments& arguments,"), newline(),
                    t("\tKJSVariant& retval,"), newline(),
                    t("\tJSApiObject* pJSApiObj,"), newline(),
                    t("\tKJSVariant& exception)"), newline(),
                    t("{"), newline(),
                    t("\tswitch (nAttributeIdx)"), newline(),
                    t("\t{"), newline(),
                    t("\tdefault:"), newline(),
                    t("\t\tbreak;"), newline(),
                    t("\t}"), newline(),
                    t("\treturn S_OK;"), newline(),
                    t("}"), newline(),
                    newline(),
                    t("void "), rep(1), t(
                    "::GetProperties(JSInputData inputData, const JSContextEnv &jsEnv, FnJSAPIGet &fnGet, FnJSAPISet &fnSet, FnJSAPIExecute &fnExecute, IJSPropertyList *pProps)"),
                    newline(),
                    t("{"), newline(),
                    t("\tfnGet = "), rep(1), t("::GetInMainThread;"), newline(),
                    t("\tfnSet = "), rep(1), t("::SetInMainThread;"), newline(),
                    t("\tfnExecute = "), rep(1), t("::ExecuteInMainThread;"), newline(),
                    newline(),
                    t("\tIJSPropertyInfo* pInfo = NULL;"), newline(),
                    newline(),
                    t("\tpInfo = pProps->Add(__X(\"API\"), JSObjectPropertyFunction, "), rep(1), t("_API, NO_AUTH);"),
                    newline(),
                    t("\t{"), newline(),
                    t("\t\tpInfo->AddParamInfo(VT_BSTR, __X(\"path\"), EJSParamNormal);"), newline(),
                    t("\t\tpInfo->SetReturnInfo(VT_BOOL);"), newline(),
                    t("\t}"), newline(),
                    t("}"), newline(),
                    newline(),
                }),
                s("comclass", {
                    t("class "), i(1, "class"), t(": public "), i(2, "IUnknown"), t(" {"), newline(),
                    t("public:"), newline(),
                    t("\tBEGIN_QUERYINTERFACE(IUnknown)"), newline(),
                    t("\t\tADD_INTERFACE("), rep(2), t(")"), newline(),
                    t("\tEND_QUERYINTERFACE()"), newline(),
                    t("\tDECLARE_CLASS("), rep(1), t(")"), newline(),
                    t("\tDECLARE_COUNT("), rep(1), t(")"), newline(),
                    newline(),
                    t("\t"), rep(1), t("();"), newline(),
                    t("\tvoid Init();"), newline(),
                    newline(),
                    t("private:"), newline(),
                    t("};"), newline(),
                }),
                s("ks_new", {
                    t("ks_stdptr<"), i(1, "type"), t("> sp"), rep(1), t(";"), newline(),
                    t("sp"), rep(1), t(".attach(KS_NEW_("), rep(1), t("));"), newline(),
                    t("sp"), rep(1), t("->Init();"), newline(),
                }),
                postfix("std_vector", {
                    f(function(_, snip)
                        return "std::vector<" .. snip.captures[1] .. ">"
                    end, { 1 }),
                }),
                postfix("unique_ptr", {
                    f(function(_, snip)
                        return "std::unique_ptr<" .. snip.captures[1] .. ">"
                    end, { 1 }),
                }),
                postfix("shared_ptr", {
                    f(function(_, snip)
                        return "std::shared_ptr<" .. snip.captures[1] .. ">"
                    end, { 1 }),
                }),
                postfix("ks_stdptr", {
                    f(function(_, snip)
                        return "std::ks_stdptr<" .. snip.captures[1] .. ">"
                    end, { 1 }),
                }),
                postfix("const_ref", {
                    f(function(_, snip)
                        return "const " .. snip.captures[1] .. "&"
                    end, { 1 }),
                }),
            })
        end,
    },
}
