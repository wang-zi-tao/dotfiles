--- 用于CodeCode Agent调用neovim的接口

--- 支持热重载. vim命令: `LuaReload core.agent`
---@class AgentModule
---@field nextId integer
---@field disableSessionEvent table<integer, boolean>
local M = require("core.hotreload").init_module(function()
  return {
    nextId = 0,
    disableSessionEvent = {},
  }
end)

M.f = 10

function M.get_next_id()
  local id = M.nextId
  M.nextId = M.nextId + 1
  return id
end

function M.clean_table(tbl)
  if type(tbl) ~= "table" then
    return tbl
  end
  local clean = {}
  for k, v in pairs(tbl) do
    if type(v) == "table" then
      clean[k] = M.clean_table(v)
    elseif type(v) ~= "function" and type(v) ~= "thread" and type(v) ~= "userdata" then
      clean[k] = v
    else
      clean[k] = '<' .. type(v) .. '>'
    end
  end
  return clean
end

function M.rpcnotify(channel_id, method, argument)
  local ok, err = pcall(vim.rpcnotify, channel_id, method, M.clean_table(argument))
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

    M.rpcnotify(
      channel_id,
      "async_task_finish",
      M.clean_table({
        task_id = task_id,
        succ = succ,
        ret = succ and ret,
        error = not succ and ret,
      })
    )
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
    M.rpcnotify(
      channel_id,
      "dap_pause",
      {
        session = session.id,
        thread_id = event.threadId,
        config_name = session.config.name,
        stop_event = event,
        stacks_top_10 = stacks.frames,
      }
    )
  end

  ---@param session dap.Session
  dap.listeners.after.event_terminated[client_name] = function(session, event)
    if M.disableSessionEvent[session.id] then
      return
    end

    M.rpcnotify(
      channel_id,
      "event_terminated",
      {
        session = session.id,
        config_name = session.config.name,
        event = event,
      }
    )
  end

  ---@param session dap.Session
  dap.listeners.after.event_exited[client_name] = function(session, event)
    if M.disableSessionEvent[session.id] then
      return
    end

    M.rpcnotify(
      channel_id,
      "event_exited",
      {
        session = session.id,
        config_name = session.config.name,
        event = event,
      }
    )
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
  opts = opts or {} -- 文档标注 opts?（可选）
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
    table.insert(
      lines,
      string.format("%s:\t%s\t%s", ins.address or "", ins.instructionBytes or "??", ins.instruction or "??")
    )
  end

  return M.clean_table({
    memory_reference = pc,
    instruction_count = instruction_count,
    instruction_offset = instruction_offset,
    pc_index = pc_index,
    instructions = instructions,
    lines = lines,
  })
end

--- 启动调试（自定义参数会深度合并到预注册配置上）
--- 例如: dap_start({ config_name="cppdbg", config={ program="/path/to/exe", args={"--flag"} } })
---        dap_start({ config_name="cppdbg", config={ pid=12345, request="attach" } })
---@param opts {lang?: string, config_name: string, config?: table}
---@return table
function M.dap_start(opts)
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
  return M.clean_table({ ok = true, lang = lang, config_name = config_name, config = target })
end

--- 继续运行（协程阻塞等待命中断点，返回停止位置信息）
--- vsdbg attach 会话的 continue 必须带 threadId 否则失败，故优先用
--- session:request("continue", { threadId = ... }) 发送（threadId 取 opts.thread_id
--- 或 session.stopped_thread_id）；失败时回退 require("dap").continue()（无 threadId 的传统路径）。
---@param opts? {timeout_ms?: integer, thread_id?: integer} -- timeout_ms 等待命中的超时毫秒数（默认 30000）；thread_id 缺省自动用 session.stopped_thread_id
---@return table
function M.dap_continue(opts)
  opts = opts or {}
  return M.step_and_wait(function()
    local dap = require("dap")
    local session = dap.session()
    local tid = opts.thread_id
    if not tid and session then
      tid = session.stopped_thread_id
    end
    local sent = false
    if tid and session then
      local err = session:request("continue", { threadId = tid })
      if not err then
        sent = true
      end
    end
    if not sent then
      dap.continue()
    end
  end, opts)
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
---@param opts? {timeout_ms?: integer} -- 等待停止的超时毫秒数（默认 30000）
---@return table {status: string, reason?: string, thread_id?: number, frames?: table[], error?: dap.ErrorResponse, totalFrames?: integer}
function M.step_and_wait(step_fn, opts)
  opts = opts or {}
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

  -- 超时定时器（默认 30 秒，可通过 opts.timeout_ms 配置）
  local timeout_ms = opts.timeout_ms or 30000
  vim.defer_fn(function()
    if not done then
      done = true
      coroutine.resume(co)
    end
  end, timeout_ms)

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
    error(string.format("step timeout: no stop event within %dms", timeout_ms))
  end

  local tid = stopped_body.threadId or session.stopped_thread_id
  if not tid then
    return { status = "stopped", reason = stopped_body.reason, description = stopped_body.description }
  end

  local frames, total, err = M.build_frames(session, tid, 4)
  if not frames then
    -- vsdbg attach 会话 stackTrace 易失败（取消/超时）：降级返回停止信息而非抛错
    return {
      status = "stopped",
      reason = stopped_body.reason,
      description = stopped_body.description,
      thread_id = tid,
      error = err,
      frames = nil,
      totalFrames = 0,
    }
  end

  return {
    status = "stopped",
    reason = stopped_body.reason,
    description = stopped_body.description,
    allThreadsStopped = stopped_body.allThreadsStopped,
    thread_id = tid,
    frames = frames,
    error = err,
    totalFrames = total,
  }
end

--- 进入函数（协程等待命中断点，返回停止位置信息）
---@param opts? {thread_id?: integer, single_thread?: boolean, granularity?: '"statement"|"line"|"instruction"'}
---@return table
function M.dap_step_into(opts)
  return M.step_and_wait(function()
    require("dap").step_into(opts)
  end, opts)
end

--- 单步跳过（协程等待命中断点，返回停止位置信息）
---@param opts? {thread_id?: integer, single_thread?: boolean, granularity?: '"statement"|"line"|"instruction"'}
---@return table
function M.dap_step_over(opts)
  return M.step_and_wait(function()
    require("dap").step_over(opts)
  end, opts)
end

--- 跳出函数（协程等待命中断点，返回停止位置信息）
---@param opts? {thread_id?: integer, single_thread?: boolean}
---@return table
function M.dap_step_out(opts)
  return M.step_and_wait(function()
    require("dap").step_out(opts)
  end, opts)
end

-- ── 运行到指定位置 ──

--- 运行到指定位置（设临时断点 → 继续 → 命中后移除并返回状态）
--- 复用 step_and_wait 处理协程等待，本函数只负责断点增删
---@param opts {file?: string, line?: number}
---@return table
function M.dap_run_to_location(opts)
  local breakpoints = require("dap.breakpoints")
  local session = M.get_session()

  local bufnr = M.resolve_bufnr(opts.file)
  local lnum = opts.line or vim.api.nvim_win_get_cursor(0)[1]

  -- 保存该 buffer 的现有断点，添加临时断点
  local existing = breakpoints.get(bufnr)
  breakpoints.set({}, bufnr, lnum)

  -- 委托 step_and_wait：同步临时断点到适配器后继续执行、等待停止
  local result = M.step_and_wait(function()
    session:set_breakpoints(breakpoints.get(bufnr), function()
      require("dap").continue()
    end)
  end, opts)

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

--- 协程友好的延时。
--- 不能用 vim.wait 直接等：nvim 禁止在 fast event 上下文（luv 回调）里调用它
--- （E5560: vim.wait must not be called in a fast event context），而本文件多处代码运行在
--- 被 socket 回调唤醒的协程里。这里改用定时器 + resume：vim.defer_fn 的回调由
--- vim.schedule_wrap 投递（_editor.lua:612），必然在主循环上下文，因此 resume 之后
--- 的 vim.wait / nvim_win_set_cursor 等 API 调用都合法。
--- 主线程（非协程）调用时没有可 yield 的协程，退化为 vim.wait。
---@param ms integer 毫秒
local function sleep_ms(ms)
  local co, is_main = coroutine.running()
  if not co or is_main then
    vim.wait(ms)
    return
  end
  vim.defer_fn(function()
    coroutine.resume(co)
  end, ms)
  coroutine.yield()
end

--- 请求原始 stackTrace 帧（未做 build_frame 转换）
--- vsdbg attach 会话在断点命中瞬间的 stackTrace 请求常被取消（"The operation was canceled"），
--- 这里最多重试 4 次（共 5 次尝试），间隔 sleep_ms(300 * attempt) 退避：300/600/900/1200ms。
---@param session dap.Session
---@param thread_id integer
---@return dap.StackFrame[]|nil, dap.ErrorResponse|nil
function M.request_stack_frames(session, thread_id)
  local err, resp
  for attempt = 1, 5 do
    if attempt > 1 then
      sleep_ms(300 * (attempt - 1))
    end
    err, resp = session:request("stackTrace", { threadId = thread_id })
    if not err and resp then
      break
    end
  end
  if err or not resp then
    return nil, err
  end
  return resp.stackFrames or {}, nil
end

--- 构造 frames 列表（封装 stackTrace 请求 + build_frame）
---@param session dap.Session
---@param thread_id integer
---@param limit? integer
---@return table[]|nil, integer, dap.ErrorResponse|nil
function M.build_frames(session, thread_id, limit)
  local all, err = M.request_stack_frames(session, thread_id)
  if not all then
    return nil, 0, err
  end

  local cwd = vim.fn.getcwd()
  local frames = {}
  local n = limit or #all

  for i, f in ipairs(all) do
    if i > n then
      break
    end
    table.insert(frames, build_frame(f, cwd))
  end

  return frames, #all, nil
end

--- 获取调用栈
---@param opts? {limit?: number, thread_id?: number}
---@return {thread_id: number, frames: table[], totalFrames: number}
function M.dap_get_stack(opts)
  opts = opts or {}
  local session = M.get_session()
  local thread_id = opts.thread_id or session.stopped_thread_id
  if not thread_id then
    error("no current thread")
  end

  local frames, total, error = M.build_frames(session, thread_id, opts.limit)
  return M.clean_table({ thread_id = thread_id, frames = frames, error = error, totalFrames = total })
end

-- ── vsdbg 兼容辅助（幽灵锁调试） ──

--- 从 session 配置推断目标进程 PID（best-effort：pid / processId / process_id 字段）
---@param session dap.Session
---@return integer?
local function infer_pid(session)
  local cfg = session and session.config or {}
  for _, key in ipairs({ "pid", "processId", "process_id" }) do
    local v = cfg[key]
    if type(v) == "number" and v > 0 then
      return v
    end
    if type(v) == "string" then
      local n = tonumber(v)
      if n and n > 0 then
        return n
      end
    end
  end
  return nil
end

--- 读取目标进程 CPU（秒）与 Responding 状态（PowerShell Get-Process）。
--- 输出格式：首行为 "CPU|Responding"（如 "12.345|True"）；无法获取返回 nil, nil。
---@param pid integer
---@return number?, boolean? -- CPU 秒数, Responding（true/false）
local function read_process_cpu(pid)
  local cmd = string.format(
    'powershell -NoProfile -Command "Get-Process -Id %d -ErrorAction SilentlyContinue | ForEach-Object { Write-Output (\'{0}|{1}\' -f $_.CPU, $_.Responding) }"',
    pid
  )
  local ok, out = pcall(vim.fn.system, cmd)
  if not ok or type(out) ~= "string" or out == "" then
    return nil, nil
  end
  local line = out:match("([^\r\n]+)") or ""
  local cpu_s, resp_s = line:match("^([^|]+)|(%S+)$")
  if not cpu_s then
    return nil, nil
  end
  local cpu = tonumber(cpu_s)
  if not cpu then
    return nil, nil
  end
  if resp_s ~= "True" and resp_s ~= "true" and resp_s ~= "False" and resp_s ~= "false" then
    return nil, nil
  end
  return cpu, (resp_s == "True" or resp_s == "true")
end

--- 卡死检查（独立公开函数）：判定目标进程是否疑似死锁——两次采样 CPU 无增长
--- （delta < hang_delta，默认 0.01 秒）且 Responding=False → status="hang"。
--- 注意：须在目标**自由运行**时采样（暂停时 CPU delta=0 是假象）。
--- 采样失败（进程已退出等）或无法确定 PID → status="skipped"。
---@param opts? {
---    pid?: integer,          -- 目标进程 PID；缺省尝试从 session.config 推断，无法确定则 skipped
---    session?: dap.Session,  -- 用于 infer_pid 推断 PID；缺省 M.get_session()
---    interval_ms?: integer,  -- 两次采样间隔毫秒数（默认 300）
---    hang_delta?: number,    -- CPU 增量判定阈值（秒，默认 0.01）
--- }
---@return {status: '"hang"|"ok"|"skipped"', hang?: table, pid?: integer, cpu1?: number, cpu2?: number, responding1?: boolean, responding2?: boolean, delta?: number, note?: string}
function M.dap_check_hang(opts)
  opts = opts or {}
  local session = opts.session or M.get_session()
  local pid = opts.pid or infer_pid(session)
  if not pid then
    return { status = "skipped", note = "无法确定目标进程 PID（传 opts.pid 或配置 session.config.pid/processId/process_id）" }
  end

  local interval_ms = opts.interval_ms or 300
  local hang_delta = opts.hang_delta or 0.01
  local cpu1, resp1 = read_process_cpu(pid)
  vim.wait(interval_ms)
  local cpu2, resp2 = read_process_cpu(pid)
  if not cpu1 or not cpu2 or resp1 == nil or resp2 == nil then
    return { status = "skipped", pid = pid, note = "进程采样失败（可能已退出）" }
  end

  local delta = math.abs(cpu2 - cpu1)
  local base = {
    pid = pid,
    cpu1 = cpu1,
    cpu2 = cpu2,
    delta = delta,
    responding1 = resp1,
    responding2 = resp2,
  }
  if delta < hang_delta and resp1 == false and resp2 == false then
    base.status = "hang"
    base.hang = { pid = pid, note = "两次采样 CPU 无增长且 Responding=False，疑似死锁" }
    return base
  end
  base.status = "ok"
  return base
end

--- 轮询等待会话停止（帧名变化 或 stopped_thread_id 变化）。与 step_and_wait 不同：不依赖协程，
--- 可在普通调用中使用。基线为当前帧名与当前 stopped_thread_id；vsdbg 断点命中瞬间
--- current_frame 可能未刷新，故增加第二判据：stopped_thread_id 从 nil→非 nil / 值变化 = 停止。
--- 可选 CPU 死锁检测（also_respond）：两次采样 CPU 无增长且 Responding=False → { status = "hang" }。
---@param opts? {
---    timeout_ms?: integer,   -- 超时毫秒数（默认 30000）
---    poll_ms?: integer,      -- 轮询间隔毫秒数（默认 200）
---    also_respond?: boolean, -- true 时额外做 CPU 死锁检测（默认 false）
---    pid?: integer,          -- 目标进程 PID（also_respond 时使用；缺省尝试从 session.config 推断，无法确定则跳过死锁检测）
--- }
---@return {status: '"stopped"|"timeout"|"hang"', thread_id?: integer, stopped_thread_id?: integer, frame_name?: string, frame?: table, hang?: table}
function M.dap_wait_stop(opts)
  opts = opts or {}
  local session = M.get_session()
  local timeout_ms = opts.timeout_ms or 30000
  local poll_ms = opts.poll_ms or 200
  local hrtime = vim.uv and vim.uv.hrtime or vim.loop.hrtime
  local baseline = session.current_frame and session.current_frame.name or nil
  local baseline_stopped = session.stopped_thread_id
  -- 初始即处于运行态（stopped_thread_id 为 nil）时，第一次出现非 nil 即视为停止；
  -- 初始已停止时，需先观察到运行态（nil）再出现非 nil（或停止线程值变化）才视为新停止。
  local seen_running = baseline_stopped == nil
  local deadline = hrtime() + timeout_ms * 1e6

  -- 死锁检测（可选）：委托 M.dap_check_hang，需要 PID，每 1s 节流
  local pid = opts.pid or infer_pid(session)
  local hang_enabled = opts.also_respond == true and pid ~= nil
  local last_hang_check = 0
  local hang_check_interval_ms = 1000 -- 死锁检测节流：每 1s 检查一次

  while hrtime() < deadline do
    if hang_enabled and (hrtime() - last_hang_check) >= hang_check_interval_ms * 1e6 then
      last_hang_check = hrtime()
      local hres = M.dap_check_hang({ pid = pid, session = session })
      if hres.status == "hang" then
        return {
          status = "hang",
          thread_id = session.stopped_thread_id,
          stopped_thread_id = session.stopped_thread_id,
          hang = hres.hang,
        }
      end
    end

    vim.wait(poll_ms)
    local frame = session.current_frame
    local stopped = session.stopped_thread_id
    if stopped == nil then
      seen_running = true
    end
    local frame_changed = frame and frame.name ~= baseline
    local stopped_changed = (seen_running and stopped ~= nil)
        or (baseline_stopped ~= nil and stopped ~= nil and stopped ~= baseline_stopped)
    if frame_changed or stopped_changed then
      return {
        status = "stopped",
        thread_id = stopped,
        stopped_thread_id = stopped,
        frame_name = frame and frame.name,
        frame = frame and M.clean_table(frame),
      }
    end
  end
  return { status = "timeout" }
end

--- 规范化 vsdbg 寄存器值：16 位 hex 无 0x 前缀（vsdbg 特性）→ 自动补 "0x" 前缀。
--- 其余形态（已有 0x 前缀 / 符号值 / 非 hex 字符串）原样返回。
---@param value string?
---@return string?
local function normalize_reg_value(value)
  if type(value) ~= "string" or value == "" then
    return value
  end
  if not value:match("^0[xX]") and value:match("^[0-9a-fA-F]+$") and #value <= 16 then
    return "0x" .. value
  end
  return value
end

--- 手工解析 hex 字符串为整数。
--- 必须手算：LuaJIT 的 tonumber(s, 16) 对 >32 位 hex 会钳位到 0xFFFFFFFF
--- （实测 tonumber("7FF8FCC70000", 16) == 4294967295），会让 64 位地址全部落到 0xFFFFFFFF。
--- 超过 2^53 返回 nil（double 不再精确）。
---@param hex string
---@return integer?
local function hex_to_int(hex)
  if hex == "" or not hex:match("^[0-9a-fA-F]+$") then
    return nil
  end
  local acc = 0
  for i = 1, #hex do
    local d = tonumber(hex:sub(i, i), 16)
    if not d then
      return nil
    end
    acc = acc * 16 + d
  end
  if acc > 9007199254740992 then -- 2^53
    return nil
  end
  return acc
end

--- 整数 → "0x..." hex 字符串（手工转换）。
--- 不用 string.format("%x", n)：实测该格式化对部分大数会失真（如 2^47 → "0x800000000000"）。
---@param n integer
---@return string
local function format_addr(n)
  n = math.floor(n)
  if n < 0 then
    n = n + 2 ^ 53 -- 兜底：不做 64 位补码，仅避免负数下溢
  end
  if n == 0 then
    return "0x0"
  end
  local digits = "0123456789abcdef"
  local out = {}
  while n > 0 do
    local d = n % 16
    out[#out + 1] = digits:sub(d + 1, d + 1)
    n = (n - d) / 16
  end
  local rev = {}
  for i = #out, 1, -1 do
    rev[#rev + 1] = out[i]
  end
  return "0x" .. table.concat(rev)
end

--- 解析地址字符串/数值为整数：支持 "0x..."/"0X..."（hex）、纯十进制字符串、number。
--- 纯 hex 无 0x 前缀（vsdbg 原始寄存器值形态）作为兜底解析。
--- hex 一律走 hex_to_int（无 32 位钳位），十进制走 tonumber（strtod，2^53 内精确）。
---@param v string|number
---@return integer?
local function parse_addr(v)
  if type(v) == "number" then
    return v
  end
  if type(v) ~= "string" then
    return nil
  end
  local s = v:match("^%s*(.-)%s*$") -- 去首尾空白
  if s == "" then
    return nil
  end
  local hex = s:match("^0[xX]([0-9a-fA-F]+)$")
  if hex then
    return hex_to_int(hex)
  end
  local dec = tonumber(s)
  if dec then
    return dec
  end
  if s:match("^[0-9a-fA-F]+$") then
    return hex_to_int(s)
  end
  return nil
end

--- 当前时间（纳秒）
local function now_ns()
  return (vim.uv and vim.uv.hrtime or vim.loop.hrtime)()
end

--- 从指定帧读取寄存器：scopes → Registers → CPU 分组 → 展开后查找寄存器名。
--- vsdbg 的 evaluate 在 attach 会话中读寄存器/全局受限，改用 DAP variables 链。
---@param session dap.Session
---@param frame_id integer
---@param reg_name string
---@param deadline? integer -- 纳秒截止时间（now_ns() 形式）；超过后中止并返回 nil
---@return string?
local function read_register_from_frame(session, frame_id, reg_name, deadline)
  if deadline and now_ns() > deadline then
    return nil
  end
  local err, resp = session:request("scopes", { frameId = frame_id })
  if err or not resp or (deadline and now_ns() > deadline) then
    return nil
  end

  local reg_scope
  for _, scope in ipairs(resp.scopes or {}) do
    if scope.name == "Registers" then
      reg_scope = scope
      break
    end
  end
  if not reg_scope then
    return nil
  end

  err, resp = session:request("variables", { variablesReference = reg_scope.variablesReference })
  if err or not resp or (deadline and now_ns() > deadline) then
    return nil
  end

  local cpu_group
  for _, v in ipairs(resp.variables or {}) do
    if v.name == "CPU" and v.variablesReference then
      cpu_group = v
      break
    end
  end
  if not cpu_group then
    return nil
  end

  err, resp = session:request("variables", { variablesReference = cpu_group.variablesReference })
  if err or not resp or (deadline and now_ns() > deadline) then
    return nil
  end

  for _, v in ipairs(resp.variables or {}) do
    if v.name == reg_name then
      return v.value
    end
  end
  return nil
end

--- 读取寄存器值：scopes → Registers → CPU 分组 → 展开后查找寄存器名。
--- vsdbg 的 evaluate 在 attach 会话中读寄存器/全局受限，改用 DAP variables 链。
--- vsdbg 实测：f0 inline 帧（id 通常=1000）的 Registers scope 才精确，普通帧读取常返回 nil，
--- 因此 auto_inline=true（默认）时，若指定帧读取失败，自动经 stackTrace 找 f0 帧重试。
--- 返回值规范化：16 位 hex 无 0x 前缀（vsdbg 特性）自动补 "0x"。
--- 请求链每步有超时保护（nvim-dap 请求自带超时 + 8s 总截止检查，超过即中止）。
---@param frame_id? integer -- 帧 ID（来自 stackTrace，如 nvim_dap_get_stack 的 frames[].id）；auto_inline 时可省略
---@param reg_name string -- 寄存器名（大写，如 "RSP"/"RIP"/"RAX"/"RDI"/"RBP"）
---@param auto_inline? boolean -- 读取失败时自动经 stackTrace 找 f0 inline 帧重试（默认 true）
---@return string -- 寄存器值字符串（已规范化补 0x）；未找到返回 ""（空串，让工具层给出可读提示，而不是返回 nil 触发 DSH 的 "value is not lossless JSON"）
function M.dap_read_register(frame_id, reg_name, auto_inline)
  if not reg_name then
    return ""
  end
  local session = M.get_session()
  if auto_inline == nil then
    auto_inline = true
  end

  local deadline = now_ns() + 8e9 -- 8s 总截止

  local result
  if frame_id then
    result = read_register_from_frame(session, frame_id, reg_name, deadline)
  end

  -- 读取失败（frame_id 为空 / 非 inline 帧无 Registers / 未找到）：auto_inline 时找 f0 inline 帧重试
  if not result and auto_inline and session.stopped_thread_id then
    local err, resp = session:request("stackTrace", { threadId = session.stopped_thread_id })
    if
        not err
        and resp
        and resp.stackFrames
        and resp.stackFrames[1]
        and resp.stackFrames[1].id ~= frame_id
    then
      result = read_register_from_frame(session, resp.stackFrames[1].id, reg_name, deadline)
    end
  end

  return normalize_reg_value(result) or ""
end

--- base64 解码（纯 Lua 实现，不依赖 vim.base64——实测 nvim 报 "module 'vim.base64' not found"）。
--- 返回解码后的字节 string；输入含非法字符返回 nil。
---@param str string
---@return string?
function M.base64_decode(str)
  if type(str) ~= "string" then
    return nil
  end
  -- 过滤空白（CR/LF 等）
  str = str:gsub("%s", "")
  if str == "" then
    return ""
  end
  local b64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  local rev = {}
  for i = 1, 64 do
    rev[b64:sub(i, i)] = i - 1
  end
  local out = {}
  local n_out = 0
  local acc = 0
  local bits = 0
  for i = 1, #str do
    local c = str:sub(i, i)
    if c == "=" then
      break -- padding，忽略后续
    end
    local v = rev[c]
    if not v then
      return nil -- 含非法字符
    end
    acc = acc * 64 + v
    bits = bits + 6
    if bits >= 8 then
      bits = bits - 8
      local byte = math.floor(acc / (2 ^ bits)) % 256
      n_out = n_out + 1
      out[n_out] = string.char(byte)
      acc = acc % (2 ^ bits)
    end
  end
  return table.concat(out)
end

--- 读取目标进程内存（DAP readMemory 请求）。readMemory 返回 base64 编码字节，
--- 经 M.base64_decode 解码后做小端解析（u32/u64）+ hex 输出。
--- memoryReference 兼容两种形态：十六进制字符串（"0x..."）或十进制字符串。
---@param opts? {
---    memory_reference?: string, -- 内存引用（十六进制 "0x..." 或十进制字符串；缺省用当前帧 instructionPointerReference）
---    offset?: integer,         -- 相对 memoryReference 的字节偏移（默认 0）
---    count?: integer,          -- 读取字节数（默认 64）
---    auto_pause?: boolean,     -- true 时若请求报错含 "running"（目标运行中），先 pause 再重试一次（默认 false）
--- }
---@return {ok: boolean, address?: string, bytes?: string, u32?: integer[], u64?: {lo: integer, hi: integer, hex: string}[], hex?: string, error?: string}
function M.dap_read_memory(opts)
  opts = opts or {}
  local session = M.get_session()

  local memref = opts.memory_reference
  if not memref and session.current_frame and session.current_frame.instructionPointerReference then
    memref = session.current_frame.instructionPointerReference
  end
  if not memref then
    return { ok = false, error =
    "no memoryReference (provide opts.memoryReference or a current frame with instructionPointerReference)" }
  end

  -- 归一化 memoryReference 为 0x 前缀 hex（DAP/vsdbg 惯例）
  local addr = parse_addr(memref)
  if addr then
    memref = format_addr(addr)
  end

  local offset = type(opts.offset) == "number" and opts.offset or 0
  local count = type(opts.count) == "number" and opts.count > 0 and math.floor(opts.count) or 64

  local function do_read()
    return session:request("readMemory", { memoryReference = memref, offset = offset, count = count })
  end

  local err, resp = do_read()
  if err and opts.auto_pause then
    local msg = type(err) == "table" and (err.message or vim.inspect(err)) or tostring(err)
    if msg:lower():find("running", 1, true) then
      -- 目标运行中：先 pause 再重试一次
      pcall(function()
        session:request("pause", { threadId = session.stopped_thread_id })
      end)
      err, resp = do_read()
    end
  end

  if err or not resp then
    local msg = type(err) == "table" and (err.message or vim.inspect(err)) or tostring(err)
    return { ok = false, error = msg or "readMemory failed" }
  end

  local address = resp.address or memref
  local bytes = resp.data and M.base64_decode(resp.data) or ""
  if #bytes == 0 then
    -- vsdbg 在目标运行中/地址不可读时返回空 data：显式报错，避免 ok=true + 空 hex 的假成功
    return {
      ok = false,
      address = address,
      error = "readMemory 返回 0 字节（目标可能正在运行：改用 auto_pause=true；或该地址不可读）",
    }
  end
  local result = { ok = true, address = address, bytes = bytes }

  -- hex 串（全部字节）
  local hex_parts = {}
  local n_bytes = #bytes
  for i = 1, n_bytes do
    hex_parts[i] = string.format("%02x", bytes:byte(i))
  end
  result.hex = table.concat(hex_parts)

  -- 小端解析：u32 每 4 字节一组；u64 每 8 字节一组（8 字节对齐）
  local u32 = {}
  local u64 = {}
  local n32 = 0
  local n64 = 0
  local i = 1
  while i + 3 <= n_bytes do
    local b1, b2, b3, b4 = bytes:byte(i, i + 3)
    local lo = b1 + b2 * 256 + b3 * 65536 + b4 * 16777216
    n32 = n32 + 1
    u32[n32] = lo
    i = i + 4
  end
  local j = 1
  while j + 7 <= n_bytes do
    local b1, b2, b3, b4 = bytes:byte(j, j + 3)
    local lo = b1 + b2 * 256 + b3 * 65536 + b4 * 16777216
    local b5, b6, b7, b8 = bytes:byte(j + 4, j + 7)
    local hi = b5 + b6 * 256 + b7 * 65536 + b8 * 16777216
    n64 = n64 + 1
    u64[n64] = {
      lo = lo,
      hi = hi,
      hex = string.format("0x%08x%08x", hi, lo),
    }
    j = j + 8
  end
  result.u32 = u32
  result.u64 = u64

  return result
end

--- 读取当前栈上参数槽位（绕过 vsdbg evaluate 的寄存器/内存读取限制）。
--- 流程：读 RSP（M.dap_read_register，默认 auto_inline）→ addr = RSP + offset → M.dap_read_memory。
--- 用途：_CalcInterrupt 参数槽 [rsp+3C8h]=src / [rsp+3D0h]=dest（debug 版）。
---@param opts? {
---    offset?: integer,      -- 相对 RSP 的字节偏移（默认 0）
---    size?: integer,        -- 读取字节数（1|2|4|8，默认 8）
---    frame_id?: integer,    -- 读 RSP 使用的帧 ID（缺省自动找 f0 inline 帧）
---    auto_inline?: boolean, -- 读 RSP 时自动找 f0 inline 帧（默认 true）
--- }
---@return {ok: boolean, rsp?: integer, addr?: integer, bytes?: string, u32?: integer[], u64?: {lo: integer, hi: integer, hex: string}[], hex?: string, error?: string}
function M.dap_read_stack_slot(opts)
  opts = opts or {}
  local size = opts.size
  if size ~= 1 and size ~= 2 and size ~= 4 and size ~= 8 then
    size = 8
  end
  local offset = type(opts.offset) == "number" and opts.offset or 0

  local rsp_str = M.dap_read_register(opts.frame_id, "RSP", opts.auto_inline ~= false)
  if not rsp_str or rsp_str == "" then
    return { ok = false, error = "failed to read RSP (no inline frame or register not found)" }
  end
  local rsp = parse_addr(rsp_str)
  if not rsp then
    return { ok = false, error = "invalid RSP value: " .. tostring(rsp_str) }
  end

  local addr = rsp + offset
  local mem = M.dap_read_memory({ memoryReference = format_addr(addr), count = size })
  if not mem.ok then
    return { ok = false, rsp = rsp, addr = addr, error = mem.error }
  end

  return {
    ok = true,
    rsp = rsp,
    addr = addr,
    bytes = mem.bytes,
    u32 = mem.u32,
    u64 = mem.u64,
    hex = mem.hex,
  }
end

--- 判定内存地址属于栈还是堆（幽灵锁 BTS 失败断点判别 RCX：堆=首次失败有效现场，栈=回滚跳过）。
--- 判据：|addr - RSP| < threshold（默认 1MB）→ stack，否则 heap；无法读 RSP / 无法解析 → unknown。
---@param addr string|integer -- 地址（"0x..."、十进制字符串或数值）
---@param opts? {rsp?: integer, threshold?: integer} -- rsp 缺省自动读；threshold 默认 1048576（1MB）
---@return {kind: '"stack"|"heap"|"unknown"', delta: integer, rsp?: integer, addr?: integer}
function M.dap_address_classify(addr, opts)
  -- 兼容两种调用形态：位置参数 (addr, opts)，或工具层直接传整个 args 表（dsh-neovim: dap_address_classify(args)）
  if type(addr) == "table" then
    local t = addr
    addr = t.addr or t.address or t.memory_reference
    opts = t
  end
  opts = opts or {}
  local a = parse_addr(addr)
  if not a then
    return { kind = "unknown", delta = 0 }
  end

  local rsp = opts.rsp
  if not rsp then
    local rsp_str = M.dap_read_register(nil, "RSP")
    rsp = (rsp_str ~= nil and rsp_str ~= "") and parse_addr(rsp_str) or nil
  end
  if not rsp then
    return { kind = "unknown", delta = 0, addr = a }
  end

  local threshold = type(opts.threshold) == "number" and opts.threshold > 0 and opts.threshold or 1048576
  local delta = math.abs(a - rsp)
  if delta < threshold then
    return { kind = "stack", delta = delta, rsp = rsp, addr = a }
  end
  return { kind = "heap", delta = delta, rsp = rsp, addr = a }
end

--- 一次调用内完成 stackTrace→定位帧→scopes→variables（规避 vsdbg frameId 跨调用失效）。
---@param thread_id integer -- 线程 ID
---@param frame_match string -- 帧名匹配模式（frame.name:find(frame_match)）
---@param var_names? string[] -- 需要收集的变量名列表；省略则收集该帧全部 Locals 变量
---@return {[string]: string}|nil -- {变量名=值字符串}；找不到帧或请求失败返回 nil
function M.dap_frame_vars(thread_id, frame_match, var_names)
  local session = M.get_session()
  if not thread_id or not frame_match then
    return nil
  end

  local err, resp = session:request("stackTrace", {
    threadId = thread_id,
    startFrame = 0,
    levels = 30,
  })
  if err or not resp then
    return nil
  end

  local target
  for _, frame in ipairs(resp.stackFrames or {}) do
    if frame.name and frame.name:find(frame_match) then
      target = frame
      break
    end
  end
  if not target then
    return nil
  end

  err, resp = session:request("scopes", { frameId = target.id })
  if err or not resp then
    return nil
  end

  local want = type(var_names) == "table" and #var_names > 0
  local result = {}
  for _, scope in ipairs(resp.scopes or {}) do
    if want or scope.name == "Locals" then
      local serr, sresp = session:request("variables", { variablesReference = scope.variablesReference })
      if not serr and sresp then
        for _, v in ipairs(sresp.variables or {}) do
          if want then
            for _, name in ipairs(var_names or {}) do
              if v.name == name then
                result[v.name] = v.value
                break
              end
            end
          else
            result[v.name] = v.value
          end
        end
      end
    end
  end
  return result
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

--- 切换当前线程（nvim-dap 语义：改写会话的"停止线程 + 当前帧"）
---
--- nvim-dap 的 step / continue / eval / 帧变量一律以 session.stopped_thread_id +
--- session.current_frame 为准，nvim-dap-ui 的调用栈与 dap-virtual-text 读
--- session.threads[tid].frames。因此"切换线程"不能只发一次 stackTrace 请求，必须把这三处
--- 会话状态一并写回；否则后续 step/continue/eval 仍打在旧线程上（本函数此前的 bug）。
---@param opts {thread_id: number, limit?: number}
---@return table
function M.dap_switch_thread(opts)
  opts = opts or {}
  local session = M.get_session()

  local tid = tonumber(opts.thread_id)
  if not tid then
    error("thread_id is required")
  end
  if not session.stopped_thread_id then
    error("session is not stopped (no stopped thread); pause first")
  end

  local raw, err = M.request_stack_frames(session, tid)
  if not raw or #raw == 0 then
    error("stackTrace for thread " .. tid .. " failed: " .. vim.inspect(err))
  end

  -- 线程表可能过期（threads 请求是异步的），目标线程缺失时同步刷新一次
  if not (session.threads and session.threads[tid]) and type(session.update_threads) == "function" then
    local co, is_main = coroutine.running()
    local done, uerr = false, nil
    local function wake(e)
      if done then
        return
      end
      done = true
      uerr = e
      -- 回调来自 nvim-dap 的 socket 上下文（handle_body 已 schedule_wrap）；这里再过一道
      -- vim.schedule 是为了不依赖对方的实现细节：无论回调来自哪个上下文，续体都在主循环上
      -- 继续执行，_frame_set 里的 nvim API 才安全。
      if co and not is_main and coroutine.status(co) == "suspended" then
        vim.schedule(function()
          if coroutine.status(co) == "suspended" then
            coroutine.resume(co)
          end
        end)
      end
    end
    session:update_threads(wake)
    -- 超时兜底：update_threads 是回调式请求，不会自己返回
    vim.defer_fn(function()
      wake(nil)
    end, 3000)
    if co and not is_main then
      if not done then
        coroutine.yield()
      end
    else
      vim.wait(3000, function()
        return done
      end, 20)
    end
    if uerr then
      error("threads request failed: " .. vim.inspect(uerr))
    end
  end
  local thread = session.threads and session.threads[tid]
  if not thread then
    error("unknown thread id: " .. tid)
  end

  -- 写回 nvim-dap 的线程/帧状态（这一步才是"切换"）
  thread.frames = raw
  thread.stopped = true
  session.stopped_thread_id = tid

  -- 顶层帧：优先取带 source 的帧，与 nvim-dap 内部 get_top_frame 一致
  local top = raw[1]
  for _, f in ipairs(raw) do
    if f.source then
      top = f
      break
    end
  end

  -- _frame_set = current_frame + jump_to_frame + _request_scopes，
  -- 与断点命中时的跳转/作用域拉取等价（nvim-dap 内部方法）
  if type(session._frame_set) == "function" then
    session:_frame_set(top)
  else
    session.current_frame = top
    if type(session._request_scopes) == "function" then
      session:_request_scopes(top)
    end
  end

  local cwd = vim.fn.getcwd()
  local frames = {}
  local n = opts.limit or 20
  for i, f in ipairs(raw) do
    if i > n then
      break
    end
    table.insert(frames, build_frame(f, cwd))
  end

  return M.clean_table({
    thread_id = tid,
    thread_name = thread.name,
    frame = frames[1],
    frames = frames,
    totalFrames = #raw,
    error = err,
  })
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
  local func = opts.func
  if not func or func == "" then
    error("func is required")
  end
  local session = M.get_session()
  local err, result = session:request("setFunctionBreakpoints", {
    breakpoints = { { name = func } },
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
    if
        err_lower:match("not supported")
        or err_lower:match("unsupported")
        or err_lower:match("unknown command")
        or err_lower:match("not found")
        or err_lower:match("unrecognized")
    then
      return {
        error = err_msg,
        hint = "This request is not supported by the debug adapter. See capabilities for supported features.",
        capabilities = session.capabilities,
      }
    end

    -- 参数错误 → 提示查询方法
    if
        err_lower:match("invalid")
        or err_lower:match("missing")
        or err_lower:match("required")
        or err_lower:match("parameter")
        or err_lower:match("argument")
        or err_lower:match("type")
    then
      return {
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
  return result
end

--- 切换断点（有则删、无则加）
---@param opts {file?: string, line?: number, condition?: string, hit_condition?: string, log_message?: string}
---@return table
function M.dap_toggle_breakpoint(opts)
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
  opts = opts or {}
  local dap = require("dap")
  local result = {}

  local langs = opts.lang and { opts.lang } or vim.tbl_keys(dap.configurations)
  for _, lang in ipairs(langs) do
    local configs = dap.configurations[lang]
    if configs then
      for _, cfg in ipairs(configs) do
        table.insert(result, {
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
    vim.cmd("edit!")
  end)
end

return M
