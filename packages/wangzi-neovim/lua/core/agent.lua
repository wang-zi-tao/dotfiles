--- 用于CodeCode Agent调用neovim的接口
local M = {}

M.nextId = 0
M.disableSessionEvent = {}

function M.get_next_id()
    local id = M.nextId
    M.nextId = M.nextId + 1
    return id
end

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

--- 兼容 opts 的两种传入形态（防御性，防止旧调用方用 {args={...}} 包装）：
---   平铺  : dap_switch_thread({ thread_id = 44772 })
---   嵌套  : dap_switch_thread({ args = { thread_id = 44772 } })
--- TS 侧 encodeLuaArgs 现会内联成平铺的 `local args={...}`，此函数仅作向后兼容兜底。
--- 注意：所有 dap_* 函数均不使用顶层 args 字段，故只要 opts.args 为 table 即可安全解包。
---@param opts table|nil
---@return table|nil
local function unwrap_opts(opts)
    if type(opts) == "table" and type(opts.args) == "table" then
        return opts.args
    end
    return opts
end

function M.rpcnotify(channel_id, method, argument) 
  local ok, err = pcall(vim.rpcnotify,  channel_id, method, argument)
  if not ok then
    error("rpcnotify error: " .. tostring(err))
  end
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

        M.rpcnotify(channel_id, "async_task_finish", M.clean_table {
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
        if M.disableSessionEvent[session.id] then
            return
        end

        local stacks = M.dap_get_stack({ limit = 10, thread_id = event.threadId })
        M.rpcnotify(channel_id, "dap_pause", M.clean_table {
            session = session.id,
            thread_id = event.threadId,
            config_name = session.config.name,
            stop_event = event,
            stacks_top_10 = stacks.frames,
        })
    end

    ---@param session dap.Session
    dap.listeners.after.event_terminated[client_name] = function(session, event)
        if M.disableSessionEvent[session.id] then
            return
        end

        M.rpcnotify(channel_id, "event_terminated", M.clean_table {
            session = session.id,
            config_name = session.config.name,
            event = event,
        })
    end

    ---@param session dap.Session
    dap.listeners.after.event_exited[client_name] = function(session, event)
        if M.disableSessionEvent[session.id] then
            return
        end

        M.rpcnotify(channel_id, "event_exited",  M.clean_table{
            session = session.id,
            config_name = session.config.name,
            event = event,
        })
    end
end

--- 获取当前调试 session，若不存在则报错
---@return dap.Session
function M.get_session()
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
    local session = M.get_session()
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

-- ── 反汇编 ──

--- 反汇编（Disassembly）：基于 DAP "disassemble" 请求，围绕当前帧指令指针（PC）反汇编机器码。
--- 参考 https://github.com/Jorenar/nvim-dap-disasm
--- 默认在 PC 前/后各取 16 条指令（共 33 条），可通过 before/after 调整；
--- 也可用 memory_reference 显式指定任意地址进行反汇编。
---@param opts? {
---    memory_reference?: string, -- 显式反汇编起始地址（缺省用当前帧的 instructionPointerReference）
---    before?: number,           -- PC 之前指令条数（默认 16）
---    after?: number,            -- PC 之后指令条数（默认 16）
---    instruction_count?: number,-- 覆盖总指令条数（默认 before+1+after）
---    instruction_offset?: number,-- 覆盖起始偏移（默认 -before）
---    resolve_symbols?: boolean, -- 是否请求解析符号
--- }
---@return {memory_reference: string, instruction_count: number, instruction_offset: number, pc_index?: number, instructions: table[], lines: string[]}
function M.dap_disasm(opts)
    opts = unwrap_opts(opts) or {}
    local session = M.get_session()

    -- 反汇编起始地址：优先显式 memory_reference，否则用当前帧的 instructionPointerReference
    local pc = opts.memory_reference
    if not pc and session.current_frame then
        pc = session.current_frame.instructionPointerReference
    end
    if not pc then
        error("no instruction pointer reference (session is running or no current frame)")
    end

    local function get_num(v, def)
        if type(v) == "number" and v >= 0 then
            return v
        end
        return def
    end

    local before = get_num(opts.before, 16)
    local after = get_num(opts.after, 16)
    local instruction_count = get_num(opts.instruction_count, before + 1 + after)
    local instruction_offset = opts.instruction_offset
    if type(instruction_offset) ~= "number" then
        instruction_offset = -before
    end

    local err, resp = session:request("disassemble", {
        memoryReference = pc,
        instructionCount = instruction_count,
        instructionOffset = instruction_offset,
        resolveSymbols = opts.resolve_symbols or nil,
    })
    if err or not resp then
        error("disassemble error: " .. vim.inspect(err))
    end

    local instructions = resp.instructions or {}
    local pc_index
    local lines = {}
    for i, ins in ipairs(instructions) do
        if ins.address == pc then
            pc_index = i
        end
        table.insert(lines, string.format("%s:\t%s\t%s",
            ins.address or "", ins.instructionBytes or "??", ins.instruction or "??"))
    end

    return M.clean_table {
        memory_reference = pc,
        instruction_count = instruction_count,
        instruction_offset = instruction_offset,
        pc_index = pc_index,
        instructions = instructions,
        lines = lines,
    }
end

--- 启动调试（自定义参数会深度合并到预注册配置上）
--- 例如: dap_start({ config_name="cppdbg", config={ program="/path/to/exe", args={"--flag"} } })
---        dap_start({ config_name="cppdbg", config={ pid=12345, request="attach" } })
---@param opts {lang?: string, config_name: string, config?: table}
---@return table
function M.dap_start(opts)
    opts = unwrap_opts(opts) or {}
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

    local ok = pcall(function()
        dap.run(target)
    end)
    return M.clean_table { ok = true, lang = lang, config_name = config_name, config = target }
end

--- 继续运行（协程阻塞等待命中断点，返回停止位置信息）
---@return table
function M.dap_continue()
    return step_and_wait(function()
        require("dap").continue()
    end)
end

--- 停止调试
---@return table
function M.dap_stop()
    local dap = require("dap")
    dap.terminate({ all = false, hierarchy = true })
    return { ok = true }
end

-- ── 步进 + 等待停止 ──

--- 执行步进并等待命中断点或退出，返回停止时的状态信息。
--- 使用 coroutine.yield() 挂起，由 event_stopped/event_terminated 监听器通过 coroutine.resume() 唤醒。
---@param step_fn function
---@return table {status: string, reason?: string, thread_id?: number, frames?: table[]}
local function step_and_wait(step_fn)
    local dap = require("dap")
    local session = dap.session()
    if not session then
        error("no debug session")
    end
    if not session.stopped_thread_id then
        error("session is not stopped")
    end

    local co = coroutine.running()
    if not co then
        error("must be called within a coroutine")
    end

    local sid = session.id
    local stopped_body = nil
    local terminated = false
    local done = false
    local key = "agent_step_" .. sid .. "_" .. M.get_next_id()

    -- 超时定时器（30 秒）
    vim.defer_fn(function()
        if not done then
            done = true
            coroutine.resume(co)
        end
    end, 30000)

    -- stopped 监听器：命中断点/单步完成时唤醒
    dap.listeners.after.event_stopped[key] = function(s, body)
        if not done and s.id == sid then
            done = true
            stopped_body = body
            coroutine.resume(co)
        end
    end

    -- terminated 监听器：调试目标退出时唤醒
    dap.listeners.after.event_terminated[key] = function(s)
        if not done and s.id == sid then
            done = true
            terminated = true
            coroutine.resume(co)
        end
    end

    M.disableSessionEvent[session.id] = true
    step_fn()

    -- 挂起协程，等待事件或超时
    coroutine.yield()
    M.disableSessionEvent[session.id] = false

    -- 清理
    dap.listeners.after.event_stopped[key] = nil
    dap.listeners.after.event_terminated[key] = nil

    if terminated then
        return { status = "terminated" }
    end
    if not stopped_body then
        error("step timeout: no stop event within 30s")
    end

    local tid = stopped_body.threadId or session.stopped_thread_id
    if not tid then
        return { status = "stopped", reason = stopped_body.reason, description = stopped_body.description }
    end

    local frames, total, error = M.build_frames(session, tid, 4)

    return {
        status = "stopped",
        reason = stopped_body.reason,
        description = stopped_body.description,
        allThreadsStopped = stopped_body.allThreadsStopped,
        thread_id = tid,
        frames = frames,
        error = error,
        totalFrames = total
    }
end

--- 进入函数（协程等待命中断点，返回停止位置信息）
---@param opts? {thread_id?: integer, single_thread?: boolean, granularity?: '"statement"|"line"|"instruction"'}
---@return table
function M.dap_step_into(opts)
    opts = unwrap_opts(opts) or {}
    return step_and_wait(function()
        require("dap").step_into(opts)
    end)
end

--- 单步跳过（协程等待命中断点，返回停止位置信息）
---@param opts? {thread_id?: integer, single_thread?: boolean, granularity?: '"statement"|"line"|"instruction"'}
---@return table
function M.dap_step_over(opts)
    opts = unwrap_opts(opts) or {}
    return step_and_wait(function()
        require("dap").step_over(opts)
    end)
end

--- 跳出函数（协程等待命中断点，返回停止位置信息）
---@param opts? {thread_id?: integer, single_thread?: boolean}
---@return table
function M.dap_step_out(opts)
    opts = unwrap_opts(opts) or {}
    return step_and_wait(function()
        require("dap").step_out(opts)
    end)
end

-- ── 运行到指定位置 ──

--- 运行到指定位置（设临时断点 → 继续 → 命中后移除并返回状态）
--- 复用 step_and_wait 处理协程等待，本函数只负责断点增删
---@param opts {file?: string, line?: number}
---@return table
function M.dap_run_to_location(opts)
    opts = unwrap_opts(opts) or {}
    local breakpoints = require("dap.breakpoints")
    local session = M.get_session()

    local bufnr = M.resolve_bufnr(opts.file)
    local lnum = opts.line or vim.api.nvim_win_get_cursor(0)[1]

    -- 保存该 buffer 的现有断点，添加临时断点
    local existing = breakpoints.get(bufnr)
    breakpoints.set({}, bufnr, lnum)

    -- 委托 step_and_wait：同步临时断点到适配器后继续执行、等待停止
    local result = step_and_wait(function()
        session:set_breakpoints(breakpoints.get(bufnr), function()
            require("dap").continue()
        end)
    end)

    -- 命中后清理：移除临时断点，恢复原有断点
    breakpoints.remove(bufnr, lnum)
    for _, buf_bps in pairs(existing) do
        for _, bp in pairs(buf_bps) do
            breakpoints.set({
                condition = bp.condition,
                hit_condition = bp.hitCondition,
                log_message = bp.logMessage,
            }, bufnr, bp.line)
        end
    end
    session:set_breakpoints(existing)

    return result
end

--- 运行到当前光标位置
---@return table
function M.dap_run_to_cursor()
    return M.dap_run_to_location({})
end

-- ── 会话/线程/监视 ──

--- 获取单行源码（不影响前台界面，不创建/切换 buffer）
---@param source_path string|nil
---@param lnum integer
---@return string?
local function get_source_line(source_path, lnum)
    if not source_path or lnum <= 0 then
        return nil
    end
    local bufnr = vim.fn.bufnr(source_path, false)
    if bufnr ~= -1 then
        local lines = vim.api.nvim_buf_get_lines(bufnr, lnum - 1, lnum, false)
        return lines[1] and vim.trim(lines[1])
    end
    -- buffer 未加载，用 readfile 读取磁盘文件（不创建 buffer）
    local ok, lines = pcall(vim.fn.readfile, source_path)
    if ok and lines then
        return lines[lnum] and vim.trim(lines[lnum])
    end
    return nil
end

--- 构建帧信息（含相对路径 + 源码行）
---@param frame dap.StackFrame
---@param cwd string
---@return table
local function build_frame(frame, cwd)
    local source = frame.source and frame.source.path or nil
    local source_rel = source
    if source and cwd and vim.startswith(source, cwd) then
        source_rel = "." .. source:sub(#cwd + 1)
    end
    return {
        id = frame.id,
        name = frame.name,
        line = frame.line,
        column = frame.column,
        source = source_rel,
        sourceLine = get_source_line(source, frame.line),
    }
end

--- 构造 frames 列表（封装 stackTrace 请求 + build_frame）
---@param session dap.Session
---@param thread_id integer
---@param limit? integer
---@return table[]|nil, integer, dap.ErrorResponse|nil
function M.build_frames(session, thread_id, limit)
    local err, resp = session:request("stackTrace", { threadId = thread_id })
    if err or not resp then
      return nil, 0, err
    end

    local cwd = vim.fn.getcwd()
    local frames = {}
    local all = resp.stackFrames or {}
    local n = limit or #all

    for i, f in ipairs(all) do
        if i > n then break end
        table.insert(frames, build_frame(f, cwd))
    end

    return frames, #all, nil
end

--- 获取调用栈
---@param opts? {limit?: number, thread_id?: number}
---@return {thread_id: number, frames: table[], totalFrames: number}
function M.dap_get_stack(opts)
    opts = unwrap_opts(opts) or {}
    local session = M.get_session()
    local thread_id = opts.thread_id or session.stopped_thread_id
    if not thread_id then
        error("no current thread")
    end

    local frames, total, error = M.build_frames(session, thread_id, opts.limit)
    return M.clean_table { thread_id = thread_id, frames = frames, error =error, totalFrames = total }
end

--- 获取线程列表
---@return table {threads: table[]}
function M.dap_get_threads()
    local session = M.get_session()
    local err, resp = session:request("threads", {})
    if err or not resp then
        error("threads error: " .. vim.inspect(err))
    end

    local threads = {}
    for _, thread in ipairs(resp.threads or {}) do
        table.insert(threads, {
            id = thread.id,
            name = thread.name,
            stopped = thread.stopped == true,
        })
    end

    return { threads = threads }
end

--- 切换线程
---@param opts {thread_id: number}
---@return table
function M.dap_switch_thread(opts)
    opts = unwrap_opts(opts) or {}
    local thread_id = tonumber(opts.thread_id)
    if not thread_id then
        error("thread_id must be a valid number")
    end

    local session = M.get_session()
    local frames, total, err = M.build_frames(session, thread_id, 1)
    local first = frames and frames[1]
    if not first then
        error("no frames for thread")
    end

    return  {
      thread_id = thread_id,
      frame = first,
      error = err,
      totalFrames = total
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
    opts = unwrap_opts(opts) or {}
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
    opts = unwrap_opts(opts) or {}
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
function M.resolve_bufnr(file)
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
    opts = unwrap_opts(opts) or {}
    if not opts.line then
        error("line is required")
    end
    local breakpoints = require("dap.breakpoints")
    local bufnr = M.resolve_bufnr(opts.file)

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

--- 添加函数断点
---@param opts {func: string}
---@return table
function M.dap_add_function_breakpoint(opts)
    opts = unwrap_opts(opts) or {}
    local func = opts.func
    if not func or func == "" then
        error("func is required")
    end
    local session = M.get_session()
    local err, result = session:request("setFunctionBreakpoints", {
      breakpoints = {{name = func}}
    })
    if err or not result then
        error("dap_add_function_breakpoint error: " .. vim.inspect(err))
    end
    return result
end

--- 通用 DAP 请求：调用任意 DAP 命令。
--- 若失败原因是适配器不支持该请求，返回适配器能力列表。
--- 若参数错误，提示如何查询请求参数类型。
---@param opts {command: string, arguments?: table}
---@return table
function M.dap_request(opts)
    opts = unwrap_opts(opts) or {}
    local command = opts.command
    if not command or command == "" then
        error("command is required")
    end
    local session = M.get_session()
    local arguments = opts.arguments or {}
    local err, result = session:request(command, arguments)
    if err then
        local err_msg = type(err) == "table" and (err.message or vim.inspect(err)) or tostring(err)
        local err_lower = err_msg:lower()

        -- 不支持该请求 → 返回适配器能力列表
        if err_lower:match("not supported")
            or err_lower:match("unsupported")
            or err_lower:match("unknown command")
            or err_lower:match("not found")
            or err_lower:match("unrecognized") then
            return  {
                error = err_msg,
                hint = "This request is not supported by the debug adapter. See capabilities for supported features.",
                capabilities = session.capabilities,
            }
        end

        -- 参数错误 → 提示查询方法
        if err_lower:match("invalid")
            or err_lower:match("missing")
            or err_lower:match("required")
            or err_lower:match("parameter")
            or err_lower:match("argument")
            or err_lower:match("type") then
            return  {
                error = err_msg,
                hint = "Parameter error. To query the expected parameter types for the '"
                    .. command
                    .. "' request, check the Debug Adapter Protocol specification "
                    .. "(https://microsoft.github.io/debug-adapter-protocol/specification) "
                    .. "or use nvim_dap_eval / nvim_dap_get_stack to inspect the current debug state.",
            }
        end

        error("dap_request error: " .. vim.inspect(err))
    end
    return (result)
end

--- 切换断点（有则删、无则加）
---@param opts {file?: string, line?: number, condition?: string, hit_condition?: string, log_message?: string}
---@return table
function M.dap_toggle_breakpoint(opts)
    opts = unwrap_opts(opts) or {}
    local breakpoints = require("dap.breakpoints")
    local bufnr = M.resolve_bufnr(opts.file)
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
    opts = unwrap_opts(opts) or {}
    if not opts.line then
        error("line is required")
    end
    local breakpoints = require("dap.breakpoints")
    local bufnr = M.resolve_bufnr(opts.file)
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
    opts = unwrap_opts(opts) or {}
    local dap = require("dap")
    local result = {}

    local langs = opts.lang and { opts.lang } or vim.tbl_keys(dap.configurations)
    for _, lang in ipairs(langs) do
        local configs = dap.configurations[lang]
        if configs then
            for _, cfg in ipairs(configs) do
                table.insert(result,  {
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

---@param path string
function M.reload_file(path)
    -- Get the buffer number for the given path
    local bufnr = vim.fn.bufnr(path)
    if bufnr == -1 then
        -- Buffer is not loaded, so nothing to reload
        return
    end
    -- Reload the buffer from disk
    vim.api.nvim_buf_call(bufnr, function()
        vim.cmd('edit!')
    end)
end

return M
