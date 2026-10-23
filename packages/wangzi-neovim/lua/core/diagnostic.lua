-- 诊断增强
--
-- 背景：virtual text 只能显示单行，clangd / pyright 的长诊断在屏幕上会被截断。
-- 这里把"读诊断"和"跳诊断"统一改成弹浮窗，浮窗开启 wrap，长消息可以完整阅读：
--   1) K：hover 浮窗里先显示 LSP hover 内容，再追加当前行诊断全文
--   2) [d / ]d / [e / ]e：跳转到诊断后，用浮窗显示该行诊断全文
local M = {}

local api = vim.api

local ns = api.nvim_create_namespace("core.diagnostic")
local hover_ns = api.nvim_create_namespace("core.diagnostic.hover_range")

local severity = vim.diagnostic.severity
local severity_name = {
    [severity.ERROR] = "ERROR",
    [severity.WARN] = "WARN",
    [severity.INFO] = "INFO",
    [severity.HINT] = "HINT",
}
local severity_hl = {
    [severity.ERROR] = "DiagnosticError",
    [severity.WARN] = "DiagnosticWarn",
    [severity.INFO] = "DiagnosticInfo",
    [severity.HINT] = "DiagnosticHint",
}

---浮窗统一选项：限制宽高，超出后浮窗内部滚动，避免一条长消息铺满整屏
---@param extra? table
---@return table
function M.float_opts(extra)
    local opts = {
        border = "rounded",
        wrap = true,
        max_width = math.max(40, math.min(120, math.floor(vim.o.columns * 0.7))),
        max_height = math.max(10, math.floor(vim.o.lines * 0.6)),
    }
    return vim.tbl_extend("force", opts, extra or {})
end

---取某一行上的诊断，按严重程度、列号排序
---@param bufnr integer
---@param lnum? integer 0-based 行号，缺省取光标所在行
---@return vim.Diagnostic[]
function M.get_line(bufnr, lnum)
    if lnum == nil then
        lnum = api.nvim_win_get_cursor(0)[1] - 1
    end
    local diags = vim.diagnostic.get(bufnr, { lnum = lnum })
    table.sort(diags, function(a, b)
        if a.severity ~= b.severity then
            return a.severity < b.severity
        end
        return a.col < b.col
    end)
    return diags
end

---把诊断渲染成浮窗里的文本行
---@param diags vim.Diagnostic[]
---@return string[] lines
---@return table[] highlights
function M.render(diags)
    local lines, highlights = {}, {}
    for i, d in ipairs(diags) do
        if i > 1 then
            table.insert(lines, "")
        end

        local head = { severity_name[d.severity] or "DIAG" }
        if d.source ~= nil and d.source ~= "" then
            table.insert(head, d.source)
        end
        if d.code ~= nil and tostring(d.code) ~= "" then
            table.insert(head, "[" .. tostring(d.code) .. "]")
        end
        local title = table.concat(head, " ") .. string.format("  %d:%d", d.lnum + 1, d.col + 1)

        table.insert(lines, title)
        table.insert(highlights, {
            line = #lines - 1,
            length = #title,
            group = severity_hl[d.severity] or "DiagnosticError",
        })

        -- 消息本身可能包含换行，按行拆开，避免被压成一行后截断
        for _, line in ipairs(vim.split(d.message or "", "\n", { plain = true })) do
            table.insert(lines, line)
        end
    end
    return lines, highlights
end

---给浮窗 buffer 加严重程度高亮
---@param bufnr integer
---@param offset integer 诊断文本起始行（0-based）
---@param highlights table[]
local function apply_highlights(bufnr, offset, highlights)
    if not bufnr or not api.nvim_buf_is_valid(bufnr) then
        return
    end
    local last = api.nvim_buf_line_count(bufnr) - 1
    for _, h in ipairs(highlights) do
        local line = offset + h.line
        if line >= 0 and line <= last then
            pcall(api.nvim_buf_set_extmark, bufnr, ns, line, 0, {
                end_col = h.length,
                hl_group = h.group,
                priority = 1000,
            })
        end
    end
end

---只显示诊断的浮窗
---@param diags vim.Diagnostic[]
---@return integer? bufnr
---@return integer? winid
function M.open_diag_float(diags)
    local lines, highlights = M.render(diags)
    local bufnr, winid = vim.lsp.util.open_floating_preview(lines, "plaintext", M.float_opts())
    apply_highlights(bufnr, 0, highlights)
    return bufnr, winid
end

---替换 vim.lsp.handlers["textDocument/hover"] 的 handler：
---保留原生 hover 的全部行为，额外把当前行诊断追加到同一个浮窗里
---@param _ lsp.ResponseError?
---@param result lsp.Hover?
---@param ctx lsp.HandlerContext
---@param config? table
---@return integer? bufnr
---@return integer? winid
function M.hover_handler(_, result, ctx, config)
    config = vim.tbl_extend("force", {}, config or {})
    config.focus_id = ctx and ctx.method or "textDocument/hover"

    local bufnr = ctx and ctx.bufnr or api.nvim_get_current_buf()
    if api.nvim_get_current_buf() ~= bufnr then
        -- 慢 server 的结果回来时 buffer 已经切走，丢弃
        return
    end

    local diags = M.get_line(bufnr)
    local diag_lines, diag_highlights = M.render(diags)

    local syntax = "markdown"
    local contents = {}
    if result and result.contents then
        if type(result.contents) == "table" and result.contents.kind == "plaintext" then
            syntax = "plaintext"
            contents = vim.split(result.contents.value or "", "\n", { trimempty = true })
        else
            contents = vim.lsp.util.convert_input_to_markdown_lines(result.contents)
        end
    end

    -- hover 没内容时退化成纯诊断浮窗，比只弹一句 "No information available" 有用
    if vim.tbl_isempty(contents) then
        if #diag_lines > 0 then
            return M.open_diag_float(diags)
        end
        if config.silent ~= true then
            vim.notify("No information available")
        end
        return
    end

    local offset = #contents
    if #diag_lines > 0 then
        table.insert(contents, "")
        table.insert(contents, "---")
        table.insert(contents, "")
        offset = #contents
        vim.list_extend(contents, diag_lines)
    end

    local float_bufnr, winid = vim.lsp.util.open_floating_preview(contents, syntax, M.float_opts(config))
    if #diag_lines > 0 then
        apply_highlights(float_bufnr, offset, diag_highlights)
    end
    return float_bufnr, winid
end

---把多个 client 的 hover 结果渲染成浮窗文本，并给符号本身打高亮
---复刻 nvim 0.11 runtime/lua/vim/lsp/buf.lua 中 hover 的内联渲染逻辑
---@param results table<integer, {err?: table, result?: table}>
---@param bufnr integer
---@return string[]? contents
---@return string syntax
local function hover_contents(results, bufnr)
    local ok_results = {}
    for client_id, resp in pairs(results) do
        if resp.err then
            if vim.lsp.log and vim.lsp.log.error then
                vim.lsp.log.error(resp.err.code, resp.err.message)
            end
        elseif resp.result then
            ok_results[client_id] = resp.result
        end
    end
    if vim.tbl_isempty(ok_results) then
        return nil, "markdown"
    end

    local contents, syntax = {}, "markdown"
    local nresults = #vim.tbl_keys(ok_results)
    for client_id, result in pairs(ok_results) do
        local client = vim.lsp.get_client_by_id(client_id)
        if nresults > 1 and client then
            table.insert(contents, string.format("# %s", client.name))
        end

        if type(result.contents) == "table" and result.contents.kind == "plaintext" then
            local value = result.contents.value or ""
            if nresults == 1 then
                syntax = "plaintext"
                contents = vim.split(value, "\n", { trimempty = true })
            else
                -- 多 client 时用代码块包住 plaintext，避免 markdown 渲染错乱
                table.insert(contents, "```")
                vim.list_extend(contents, vim.split(value, "\n", { trimempty = true }))
                table.insert(contents, "```")
            end
        else
            vim.list_extend(contents, vim.lsp.util.convert_input_to_markdown_lines(result.contents))
        end

        local range = result.range
        if range and client and vim.hl and vim.hl.range then
            local from, to = range.start, range["end"]
            local from_col = vim.lsp.util._get_line_byte_from_position(bufnr, from, client.offset_encoding)
            local to_col = vim.lsp.util._get_line_byte_from_position(bufnr, to, client.offset_encoding)
            vim.hl.range(
                bufnr,
                hover_ns,
                "LspReferenceTarget",
                { from.line, from_col },
                { to.line, to_col },
                { priority = vim.hl.priorities.user }
            )
        end

        table.insert(contents, "---")
    end

    -- 去掉最后一个 client 之后多余的分隔符
    contents[#contents] = nil
    if vim.tbl_isempty(contents) then
        return nil, syntax
    end
    return contents, syntax
end

---K 的入口：hover 内容 + 当前行诊断全文，同一个浮窗里一次开出来
---
---注意：nvim 0.11 的 vim.lsp.buf.hover() 改用 buf_request_all + 内联渲染，
---**不再经过** vim.lsp.handlers["textDocument/hover"]，覆盖 handler 追加诊断对它无效。
---所以这里自己发一次 buf_request_all，渲染完 hover 内容再并上诊断。
---@param config? table
---@return integer? bufnr
---@return integer? winid
function M.hover(config)
    config = vim.tbl_extend("force", {}, config or {})
    config.focus_id = "textDocument/hover"

    local bufnr = api.nvim_get_current_buf()
    local winid = api.nvim_get_current_win()
    local lnum = api.nvim_win_get_cursor(winid)[1] - 1

    local diags = M.get_line(bufnr, lnum)
    local diag_lines, diag_highlights = M.render(diags)

    local function no_hover()
        if #diag_lines > 0 then
            return M.open_diag_float(diags)
        end
        if config.silent ~= true then
            vim.notify("No information available")
        end
    end

    if #vim.lsp.get_clients({ bufnr = bufnr, method = "textDocument/hover" }) == 0 then
        return no_hover()
    end

    vim.lsp.buf_request_all(0, "textDocument/hover", function(client)
        return vim.lsp.util.make_position_params(winid, client.offset_encoding)
    end, function(results, ctx)
        local rbuf = (ctx and ctx.bufnr) or bufnr
        if api.nvim_get_current_buf() ~= rbuf then
            -- 慢 server 的结果回来时 buffer 已经切走，丢弃
            return
        end

        local contents, syntax = hover_contents(results, rbuf)
        if contents == nil then
            return no_hover()
        end

        local offset
        if #diag_lines > 0 then
            vim.list_extend(contents, { "", "---", "" })
            offset = #contents
            vim.list_extend(contents, diag_lines)
        end

        local float_bufnr, float_winid = vim.lsp.util.open_floating_preview(contents, syntax, M.float_opts(config))
        if offset ~= nil then
            apply_highlights(float_bufnr, offset, diag_highlights)
        end
        if float_winid ~= nil then
            api.nvim_create_autocmd("WinClosed", {
                pattern = tostring(float_winid),
                once = true,
                callback = function()
                    api.nvim_buf_clear_namespace(rbuf, hover_ns, 0, -1)
                    return true
                end,
            })
        end
        return float_bufnr, float_winid
    end)
end

---跳转到上一条/下一条诊断，并用浮窗显示该行诊断全文
---@param count integer 正数向后，负数向前
---@param severity? integer|integer[] 只跳指定严重程度
function M.jump(count, severity)
    vim.diagnostic.jump({
        count = count,
        severity = severity,
        float = M.float_opts({ scope = "line" }),
    })
end

return M
