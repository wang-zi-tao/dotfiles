--- 用于CodeCode Agent调用neovim的接口
local M = {}

function M.clean_table(tbl)
    if type(tbl) ~= "table" then return tbl end
    local clean = {}
    for k, v in pairs(tbl) do
        if type(v) == "table" then
            clean[k] = M.clean_table(v)
        elseif type(v) ~= "function" and type(v) ~= "thread" and type(v) ~= "userdata" then
            clean[k] = v
        end
    end
    return clean
end

---@param f function
---@param task_id integer
---@param channel_id integer
---@param argsJson? string
function M.run_async(f, channel_id, task_id, argsJson)
    coroutine.wrap(function(f, channel_id, task_id, argsJson)
        local succ, ret = pcall(function()
            local args = argsJson and vim.json.decode(argsJson)
            local ret = f(args)
            return ret
        end)

        vim.rpcnotify(channel_id, "async_task_finish", {
            task_id = task_id,
            succ = succ,
            ret = succ and ret,
            error = not succ and ret,
        })
    end)(f, channel_id, task_id, argsJson)
end

function M.dap_subscribe(channel_id)
    local dap = require("dap")
    local client_name = "agent_" .. channel_id

    ---@param session dap.Session
    dap.listeners.after.event_stopped[client_name] = function(session, event)
        local stacks = M.dap_get_stack()
        local stacks_top_10 = {}
        local stack_length = #stacks
        for i, frame in ipairs(stacks.frames) do
            if i < stack_length - 10 then break end
            table.insert(stacks_top_10, frame)
        end

        vim.fn.rpcnotify(channel_id, "dap_pause", {
            session = session.id,
            config_name = session.config.name,
            stop_event = event,
            stacks_top_10 = stacks_top_10
        })
    end

    ---@param session dap.Session
    dap.listeners.after.event_terminated[client_name] = function(session, event)
        vim.fn.rpcnotify(channel_id, "event_terminated", {
            session = session.id,
            config_name = session.config.name,
            event = event,
        })
    end

    ---@param session dap.Session
    dap.listeners.after.event_exited[client_name] = function(session, event)
        vim.fn.rpcnotify(channel_id, "event_exited", {
            session = session.id,
            config_name = session.config.name,
            event = event,
        })
    end
end

--- 获取当前调试 session，若不存在则报错
---@return dap.Session
local function get_session()
    local dap = require("dap")
    local session = dap.session()
    if not session then
        error("no debug session")
    end
    return session
end

--- 求值表达式
---@param expr string
---@return dap.EvaluateResponse
function M.dap_eval(expr)
    local session = get_session()
    local err, result = session:request("evaluate", {
        expression = expr,
        context = "repl",
        format = { hex = false },
        frameId = session.current_frame.id,
    })
    if err or not result then
        error("evaluate error: " .. vim.inspect(err))
    end
    return result
end

--- 启动调试（自定义参数会深度合并到预注册配置上）
--- 例如: dap_start({ config_name="cppdbg", config={ program="/path/to/exe", args={"--flag"} } })
---        dap_start({ config_name="cppdbg", config={ pid=12345, request="attach" } })
---@param opts {lang?: string, config_name: string, config?: table}
---@return table
function M.dap_start(opts)
    opts = opts or {}
    local dap = require("dap")
    local lang = opts.lang or "cpp"
    local config_name = opts.config_name

    if not config_name or config_name == "" then
        error("config_name is required")
    end

    local configs = dap.configurations[lang]
    if not configs or vim.tbl_isempty(configs) then
        error(string.format("no configurations found for language '%s'", lang))
    end

    local target
    for _, cfg in ipairs(configs) do
        if cfg.name == config_name then
            target = cfg
            break
        end
    end

    if not target then
        error(string.format("configuration '%s' not found for language '%s'", config_name, lang))
    end

    -- 深度合并自定义参数到模板配置上
    if opts.config and type(opts.config) == "table" then
        -- 浅拷贝模板避免污染原始配置
        target = vim.tbl_deep_extend("force", {}, target, opts.config)
    end

    dap.run(target)
    return M.clean_table { ok = true, lang = lang, config_name = config_name, config = target }
end

--- 停止调试
---@return table
function M.dap_stop()
    local dap = require("dap")
    dap.terminate({ all = false, hierarchy = true })
    return { ok = true }
end

--- 进入函数
---@return table
function M.dap_step_into()
    local dap = require("dap")
    dap.step_into()
    return { ok = true }
end

--- 单步跳过
---@return table
function M.dap_step_over()
    local dap = require("dap")
    dap.step_over()
    return { ok = true }
end

--- 跳出函数
---@return table
function M.dap_step_out()
    local dap = require("dap")
    dap.step_out()
    return { ok = true }
end

--- 获取调用栈
---@return {thread_id: number, frames: table[]}
function M.dap_get_stack()
    local session = get_session()
    local current_thread = session.stopped_thread_id
    if not current_thread then
        error("no current thread")
    end

    local err, resp = session:request("stackTrace", { threadId = session.stopped_thread_id })
    if err or not resp then
        error("stackTrace error: " .. vim.inspect(err))
    end

    local frames = {}
    for _, frame in ipairs(resp.stackFrames or {}) do
        table.insert(frames, {
            id = frame.id,
            name = frame.name,
            line = frame.line,
            column = frame.column,
            source = frame.source and frame.source.path or nil,
        })
    end

    return M.clean_table { thread_id = session.stopped_thread_id, frames = frames }
end

--- 获取线程列表
---@return table {threads: table[]}
function M.dap_get_threads()
    local session = get_session()
    local err, resp = session:request("threads", {})
    if err or not resp then
        error("threads error: " .. vim.inspect(err))
    end

    local threads = {}
    for _, thread in ipairs(resp.threads or {}) do
        table.insert(threads, {
            id = thread.id,
            name = thread.name,
        })
    end

    return { threads = threads }
end

--- 切换线程
---@param opts {thread_id: number}
---@return table
function M.dap_switch_thread(opts)
    opts = opts or {}
    local thread_id = tonumber(opts.thread_id)
    if not thread_id then
        error("thread_id must be a valid number")
    end

    local session = get_session()
    local err, resp = session:request("stackTrace", { threadId = thread_id })
    if err or not resp then
        error("switch_thread error: " .. vim.inspect(err))
    end

    local first = resp.stackFrames and resp.stackFrames[1]
    if not first then
        error("no frames for thread")
    end

    return {
        thread_id = thread_id,
        frame = {
            id = first.id,
            name = first.name,
            line = first.line,
            column = first.column,
            source = first.source and first.source.path or nil,
        },
    }
end

--- 获取所有调试会话
---@return table {sessions: table[]}
function M.dap_get_sessions()
    local dap = require("dap")
    local sessions = {}
    for _, session in pairs(dap.sessions()) do
        table.insert(sessions, {
            name = session.config and session.config.name or nil,
            id = session.id,
            type = session.config and session.config.type or nil,
            root = session.config and session.config.cwd or nil,
        })
    end
    return { sessions = sessions }
end

--- 切换调试会话
---@param opts {session_name: string}
---@return table
function M.dap_switch_session(opts)
    opts = opts or {}
    local target_name = opts.session_name
    if not target_name or target_name == "" then
        error("session_name is required")
    end

    local dap = require("dap")
    local target
    for _, session in pairs(dap.sessions()) do
        if session.config and session.config.name == target_name then
            target = session
            break
        end
    end

    if not target then
        error(string.format("session with name '%s' not found", target_name))
    end

    dap.set_session(target)
    return {
        session = {
            name = target.config and target.config.name or nil,
            id = target.session_id,
            type = target.config and target.config.type or nil,
        },
    }
end

--- 添加监视表达式
---@param opts {expr: string}
---@return table
function M.dap_add_watch(opts)
    opts = opts or {}
    local expr = opts.expr
    if not expr or expr == "" then
        error("expr is required")
    end
    local dapui_ok, dapui = pcall(require, "dapui")
    if dapui_ok and dapui.eval then
        dapui.eval(expr)
    end
    return { expression = expr }
end

-- ── 断点操作 ──

--- 解析文件路径为 buffer number，失败则报错
---@param file? string
---@return integer
local function resolve_bufnr(file)
    if file then
        local bufnr = vim.fn.bufnr(file, true) -- true = 必要时创建未加载的 buffer
        if bufnr == -1 then
            error("file not found: " .. file)
        end
        return bufnr
    end
    return vim.api.nvim_get_current_buf()
end

--- 将当前 buffer 的断点同步到活动调试会话
---@param bufnr integer
local function sync_breakpoints(bufnr)
    local dap = require("dap")
    local session = dap.session()
    if session then
        session:set_breakpoints(require("dap.breakpoints").get(bufnr))
    end
end

--- 添加断点（支持条件/日志/命中次数）
---@param opts {file?: string, line: number, condition?: string, hit_condition?: string, log_message?: string}
---@return table
function M.dap_add_breakpoint(opts)
    opts = opts or {}
    if not opts.line then
        error("line is required")
    end
    local breakpoints = require("dap.breakpoints")
    local bufnr = resolve_bufnr(opts.file)

    breakpoints.toggle({
        condition = opts.condition,
        hit_condition = opts.hit_condition,
        log_message = opts.log_message,
        replace = true,
    }, bufnr, opts.line)

    sync_breakpoints(bufnr)

    return {
        file = vim.api.nvim_buf_get_name(bufnr),
        line = opts.line,
        condition = opts.condition,
    }
end

--- 切换断点（有则删、无则加）
---@param opts {file?: string, line?: number, condition?: string, hit_condition?: string, log_message?: string}
---@return table
function M.dap_toggle_breakpoint(opts)
    opts = opts or {}
    local breakpoints = require("dap.breakpoints")
    local bufnr = resolve_bufnr(opts.file)
    local lnum = opts.line or vim.api.nvim_win_get_cursor(0)[1]

    breakpoints.toggle({
        condition = opts.condition,
        hit_condition = opts.hit_condition,
        log_message = opts.log_message,
    }, bufnr, lnum)

    sync_breakpoints(bufnr)

    return { ok = true, file = vim.api.nvim_buf_get_name(bufnr), line = lnum }
end

--- 删除指定位置的断点
---@param opts {file?: string, line: number}
---@return table
function M.dap_remove_breakpoint(opts)
    opts = opts or {}
    if not opts.line then
        error("line is required")
    end
    local breakpoints = require("dap.breakpoints")
    local bufnr = resolve_bufnr(opts.file)
    local removed = breakpoints.remove(bufnr, opts.line)

    sync_breakpoints(bufnr)

    return { removed = removed, file = vim.api.nvim_buf_get_name(bufnr), line = opts.line }
end

--- 列出所有断点
---@return table {breakpoints: {file: string, line: number, condition?: string, hitCondition?: string, logMessage?: string, verified?: boolean, message?: string}[]}
function M.dap_list_breakpoints()
    local breakpoints = require("dap.breakpoints")
    local all = breakpoints.get()
    local result = {}
    for bufnr, bps in pairs(all) do
        local fname = vim.api.nvim_buf_get_name(bufnr)
        for _, bp in ipairs(bps) do
            table.insert(result, {
                file = fname,
                line = bp.line,
                condition = bp.condition,
                hitCondition = bp.hitCondition,
                logMessage = bp.logMessage,
                verified = bp.state and bp.state.verified,
                message = bp.state and bp.state.message,
            })
        end
    end
    return { breakpoints = result }
end

--- 清除所有断点
---@return table
function M.dap_clear_breakpoints()
    local dap = require("dap")
    dap.clear_breakpoints()
    return { ok = true }
end

--- 获取调试配置列表
---@param opts? {lang?: string}
---@return table {configurations: {lang: string, name: string, type: string?, request: string?, cwd?: string}[]}
function M.dap_get_configurations(opts)
    opts = opts or {}
    local dap = require("dap")
    local result = {}

    local langs = opts.lang and { opts.lang } or vim.tbl_keys(dap.configurations)
    for _, lang in ipairs(langs) do
        local configs = dap.configurations[lang]
        if configs then
            for _, cfg in ipairs(configs) do
                table.insert(result, M.clean_table {
                    lang = lang,
                    name = cfg.name,
                    type = cfg.type,
                    request = cfg.request,
                    cwd = cfg.cwd,
                })
            end
        end
    end

    if not opts.lang and #result == 0 then
        error("no debug configurations found")
    end

    if opts.lang and #result == 0 then
        error(string.format("no configurations found for language '%s'", opts.lang))
    end

    return { configurations = result }
end

return M
