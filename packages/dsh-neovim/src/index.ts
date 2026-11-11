/**
 * dsh-neovim — Neovim bridge for DeepSeek Harness.
 *
 * Port of the OpenCode `neovim-api` plugin onto the DSH plugin contract:
 *
 *   - `ctx.tools.register` exposes vim command / Lua execution / Lua
 *     evaluation plus the full nvim-dap toolset (`dap_start`, steps,
 *     breakpoints, watches, threads, sessions, configurations, disassembly)
 *     through the user's Neovim `core.agent` Lua bridge.
 *   - `nvim.subscribe` (`dap_pause` / `event_terminated` / `event_exited`)
 *     drives debugger event injection: formatted `<dap-event>` context is
 *     delivered with `agent.inject()` to every session that has used a DAP
 *     tool (`dapSessions`).
 *   - `tools/result` observes the file-writing tools (`write` / `edit`) and
 *     asks Neovim to `reload_file`, mirroring the OpenCode
 *     `tool.execute.after` hook.
 *
 * The connection is lazy and self-healing (unlike the OpenCode plugin, which
 * throws at load): the host is long-lived and Neovim may start, restart, or
 * stop at any time, so each tool call re-establishes the connection on
 * demand, and `/neovim` reports state or forces a reconnect.
 *
 * @module dsh-neovim
 */

import { resolveConfig, resolveSocket } from './config.js'
import type { NeovimConfig } from './config.js'
import { Neovim, probeSocket, tryConnectNvim } from './neovim.js'
import type { LuaArgs } from './neovim.js'
import {
  fmtBreakpoints,
  fmtConfigs,
  fmtDisasm,
  fmtSessions,
  fmtStack,
  fmtSwitchThread,
  fmtThreads,
} from './format.js'
import { createUserMessage } from '@deepseek-ai/dsh-llm'
import type { Context, JsonSchemaNode, Logger, ToolDefinition, ToolRunContext, ToolExecution, Agent } from './types.js'

export const name = 'dsh-neovim'
export const inject = ['tools', 'commands', 'agents']
export { resolveConfig, resolveSocket }
export type { NeovimConfig }
export { Neovim, probeSocket, tryConnectNvim } from './neovim.js'
export type { LuaArgs }

function makeLogger(ctx: Context): Logger {
  try {
    if (typeof ctx.logger === 'function') return ctx.logger('neovim')
  } catch {
    /* fall through */
  }
  // Fallback for hosts where ctx.logger is not the callable service; only the
  // four severity methods are structurally used, so the cast through unknown is
  // fine here (Logger is a class type with private members).
  return { name: 'neovim', debug() {}, info() {}, warn() {}, error() {} } as unknown as Logger
}

function errText(error: unknown): string {
  return error instanceof Error ? error.message : String(error)
}

/** JSON-Schema root for tool parameters (dsh-lsp convention). */
function objectSchema(
  properties: Record<string, Record<string, unknown>>,
  required: string[] = [],
): Record<string, unknown> {
  return { type: 'object', additionalProperties: false, properties, required }
}

const STRING = (description: string): Record<string, unknown> => ({ type: 'string', description })
const INTEGER = (description: string): Record<string, unknown> => ({ type: 'integer', description })
const BOOLEAN = (description: string): Record<string, unknown> => ({ type: 'boolean', description })
const OBJECT = (description: string): Record<string, unknown> => ({
  type: 'object',
  additionalProperties: true,
  description,
})

// ---------------------------------------------------------------------------
// Output schemas.
//
// dsh-tools enforces a strict JSON-Schema subset for tool outputs: one scalar
// `type`, object `properties`/`required`, boolean `additionalProperties`,
// array `items`, scalar `enum`/`const`, exact-one `oneOf`, and annotations.
// Nullable values must be written as `oneOf: [<schema>, { type: 'null' }]`.
// Lua-side objects are open (additionalProperties: true) because the bridge may
// grow new fields; `required` only lists fields that are always present.
// ---------------------------------------------------------------------------
const outString = (description?: string): JsonSchemaNode =>
  description === undefined ? { type: 'string' } : { type: 'string', description }
const outInteger = (description?: string): JsonSchemaNode =>
  description === undefined ? { type: 'integer' } : { type: 'integer', description }
const outNumber = (description?: string): JsonSchemaNode =>
  description === undefined ? { type: 'number' } : { type: 'number', description }
const outBoolean = (description?: string): JsonSchemaNode =>
  description === undefined ? { type: 'boolean' } : { type: 'boolean', description }
const outArray = (items: JsonSchemaNode, description?: string): JsonSchemaNode =>
  description === undefined ? { type: 'array', items } : { type: 'array', items, description }
/** Object with declared properties; extra Lua-side fields stay allowed. */
const outObject = (
  properties: Record<string, JsonSchemaNode>,
  required: string[] = [],
  description?: string,
): JsonSchemaNode => ({
  type: 'object',
  additionalProperties: true,
  properties,
  ...(required.length > 0 ? { required } : {}),
  ...(description === undefined ? {} : { description }),
})
/** Object whose properties are not enumerated. */
const outAnyObject = (description: string): JsonSchemaNode => ({
  type: 'object',
  additionalProperties: true,
  description,
})
const OUT_NULL: JsonSchemaNode = { type: 'null' }
const outNullable = (schema: JsonSchemaNode): JsonSchemaNode => ({ oneOf: [schema, OUT_NULL] })
const OUT_TRUE: JsonSchemaNode = { type: 'boolean', const: true }
const OUT_OK: JsonSchemaNode = outObject({ ok: OUT_TRUE }, ['ok'], '操作成功')
const OUT_DAP_ERROR: JsonSchemaNode = {
  oneOf: [outString(), outAnyObject('DAP 错误响应'), OUT_NULL],
  description: '错误信息（字符串）或 DAP 错误响应',
}

const OUT_FRAME: JsonSchemaNode = outObject(
  {
    id: outInteger('帧 ID'),
    name: outString('帧名'),
    line: outInteger('行号'),
    column: outInteger('列号'),
    source: outString('源码路径（相对 cwd）'),
    sourceLine: outString('源码行内容'),
  },
  ['id'],
  'DAP 栈帧',
)

/** Step/continue result: only `status` is unconditional. */
const OUT_STEP: JsonSchemaNode = outObject(
  {
    status: { type: 'string', enum: ['stopped', 'terminated', 'timeout'], description: '停止状态' },
    reason: outString('停止原因'),
    description: outString('停止描述'),
    allThreadsStopped: outBoolean('是否所有线程都已停止'),
    thread_id: outInteger('命中的线程 ID'),
    frames: outArray(OUT_FRAME, '栈帧列表'),
    error: OUT_DAP_ERROR,
    totalFrames: outInteger('总帧数'),
  },
  ['status'],
  '继续/步进后的停止状态',
)

const OUT_INSTRUCTION: JsonSchemaNode = outObject(
  {
    address: outString('指令地址'),
    instructionBytes: outString('机器码字节'),
    instruction: outString('反汇编文本'),
    symbol: outString('解析出的符号'),
  },
  [],
  '单条反汇编指令',
)

const OUT_U64: JsonSchemaNode = outObject(
  { lo: outInteger('低 32 位'), hi: outInteger('高 32 位'), hex: outString('0x 十六进制') },
  ['lo', 'hi', 'hex'],
  '小端 u64 分组',
)

const OUT_MEMORY_SUCCESS: JsonSchemaNode = outObject(
  {
    ok: OUT_TRUE,
    address: outString('内存地址'),
    bytes: outString('原始字节'),
    hex: outString('十六进制串'),
    u32: outArray(outInteger(), '小端 u32 分组'),
    u64: outArray(OUT_U64, '小端 u64 分组'),
  },
  ['ok', 'address', 'hex'],
  '读取成功',
)
const OUT_MEMORY_FAILURE: JsonSchemaNode = outObject(
  {
    ok: { type: 'boolean', const: false },
    address: outString('内存地址'),
    error: outString('错误信息'),
  },
  ['ok', 'error'],
  '读取失败',
)
const OUT_READ_MEMORY: JsonSchemaNode = { oneOf: [OUT_MEMORY_SUCCESS, OUT_MEMORY_FAILURE] }

const OUT_SLOT_SUCCESS: JsonSchemaNode = outObject(
  {
    ok: OUT_TRUE,
    rsp: outInteger('RSP 值'),
    addr: outInteger('槽位地址'),
    bytes: outString('原始字节'),
    hex: outString('十六进制串'),
    u32: outArray(outInteger(), '小端 u32 分组'),
    u64: outArray(OUT_U64, '小端 u64 分组'),
  },
  ['ok', 'rsp', 'addr'],
  '读取成功',
)
const OUT_SLOT_FAILURE: JsonSchemaNode = outObject(
  {
    ok: { type: 'boolean', const: false },
    rsp: outInteger('RSP 值'),
    addr: outInteger('槽位地址'),
    error: outString('错误信息'),
  },
  ['ok', 'error'],
  '读取失败',
)
const OUT_READ_STACK_SLOT: JsonSchemaNode = { oneOf: [OUT_SLOT_SUCCESS, OUT_SLOT_FAILURE] }

const OUT_CHECK_HANG: JsonSchemaNode = outObject(
  {
    status: { type: 'string', enum: ['skipped', 'ok', 'hang'], description: '判定结果' },
    pid: outInteger('目标进程 PID'),
    cpu1: outNumber('第一次采样 CPU 秒数'),
    cpu2: outNumber('第二次采样 CPU 秒数'),
    delta: outNumber('两次采样的 CPU 增量秒数'),
    responding1: outBoolean('第一次采样 Responding'),
    responding2: outBoolean('第二次采样 Responding'),
    note: outString('说明'),
    hang: outObject({ pid: outInteger('目标进程 PID'), note: outString('说明') }, [], '死锁信息'),
  },
  ['status'],
  '卡死检查结果',
)

const OUT_VAR_MAP: JsonSchemaNode = {
  oneOf: [outAnyObject('变量名 → 值字符串的映射'), OUT_NULL],
  description: '帧变量映射；找不到帧或请求失败时为 null',
}

const OUT_THREAD: JsonSchemaNode = outObject(
  {
    id: outInteger('线程 ID'),
    name: outString('线程名'),
    stopped: outBoolean('是否已停止'),
  },
  ['id'],
  'DAP 线程',
)

const OUT_SESSION: JsonSchemaNode = outObject(
  {
    name: outString('会话名'),
    id: outInteger('会话 ID'),
    type: outString('适配器类型'),
    root: outString('工作目录'),
  },
  [],
  'DAP 调试会话',
)

const OUT_BREAKPOINT: JsonSchemaNode = outObject(
  {
    file: outString('文件路径'),
    line: outInteger('行号'),
    condition: outString('条件表达式'),
    hitCondition: outString('命中次数条件'),
    logMessage: outString('日志消息'),
    verified: outBoolean('是否已验证'),
    message: outString('验证信息'),
  },
  ['file', 'line'],
  '断点',
)

const OUT_CONFIGURATION: JsonSchemaNode = outObject(
  {
    lang: outString('语言'),
    name: outString('配置名'),
    type: outString('适配器类型'),
    request: outString('launch/attach'),
    cwd: outString('工作目录'),
  },
  ['lang', 'name'],
  '调试配置',
)

const OUT_DAP_RESPONSE: JsonSchemaNode = {
  oneOf: [outAnyObject('DAP 响应体（结构由具体请求决定）'), OUT_NULL],
  description: 'DAP 请求响应；无响应时为 null',
}

interface ToolSpec {
  name: string
  description: string
  parameters: Record<string, unknown>
  /** JSON Schema (dsh-tools enforced subset) for this tool's canonical result. */
  output: JsonSchemaNode
  /** Fetch the raw tool result; rendered via format. */
  run(args: any, exec: ToolRunContext): Promise<unknown>
  /** Render a raw result as human-readable markdown. */
  format(result: any, args: any): string
}

export function apply(ctx: Context, rawConfig: Record<string, unknown> = {}): void {
  const config = resolveConfig(rawConfig)
  const logger = makeLogger(ctx)

  // ------------------------------------------------------------------
  // Connection lifecycle (lazy, self-healing)
  // ------------------------------------------------------------------
  let nvim: Neovim | null = null
  let connectPromise: Promise<Neovim | null> | null = null

  /** Sessions that used a DAP tool; debugger events are injected only there. */
  const dapSessions = new Set<string>()

  function inject(agent: Agent, prompt: string, auto_start: boolean = false) {
    try {
      const message = createUserMessage({content: [{type: 'text', text: prompt}], source: {kind: name}});
      if (auto_start)
        agent.steer(message)
      else
        agent.inject(message);
    } catch (error) {
      // The agent went away between list() and inject(); drop the stale entry.
      logger.error(`prompt event delivery failed for session ${agent.session.id}: ${errText(error)}`);
    }
  }

  function broadcastDapEvent(text: string): void {
    for (const agent of ctx.agents.list()) {
      const id = agent.session?.id
      if (!id || !dapSessions.has(String(id))) continue
      inject(agent, text)
    }
  }

  async function subscribeNvim(nv: Neovim): Promise<void> {
    const channelId = await nv.channelId();
    nv.lua(`agent.subscribe(${channelId})`).catch((error) => {
      logger.error(`subscribe error: ${errText(error)}`);
    });

    await nv.subscribe("prompt_inject", (args: any) => {
      const a = args?.[0] ?? args;
      let prompt: string = a.prompt;

      for (const agent of ctx.agents.roots()) {
        inject(agent, prompt, true)
      }
    });
  };

  async function subscribeDap(nv: Neovim): Promise<void> {
    const channelId = await nv.channelId()
    nv.lua(`agent.dap_subscribe(${channelId})`).catch((error) => {
      logger.error(`dap_subscribe error: ${errText(error)}`)
    })
    logger.info(`listening for neovim debugger events (channel ${channelId})`)

    await nv.subscribe('dap_pause', (args: any) => {
      const a = args?.[0] ?? args
      let text = `<dap-event type="stopped">\n`
      text += `## 调试器已暂停\n`
      text += `**会话:** ${a.config_name || a.session || '?'} (id: ${a.session ?? '?'})\n`
      const reason = a.stop_event?.reason
      if (reason) text += `**原因:** ${reason}\n`
      text += fmtStack({ frames: a.stacks_top_10 || [], thread_id: a.thread_id })
      text += `\n</dap-event>`
      logger.info(text)
      broadcastDapEvent(text)
    })

    await nv.subscribe('event_terminated', (args: any) => {
      const a = args?.[0] ?? args
      const text =
        `<dap-event type="terminated">\n## 调试会话已终止\n` +
        `**会话:** ${a.config_name || a.session || '?'} (id: ${a.session ?? '?'})\n</dap-event>`
      broadcastDapEvent(text)
    })

    await nv.subscribe('event_exited', (args: any) => {
      const a = args?.[0] ?? args
      const exitCode = a.event?.exitCode
      const text =
        `<dap-event type="exited">\n## 调试目标已退出\n` +
        `**会话:** ${a.config_name || a.session || '?'} (id: ${a.session ?? '?'})` +
        `${exitCode != null ? `\n**退出码:** ${exitCode}` : ''}\n</dap-event>`
      broadcastDapEvent(text)
    })
  }

  async function connect(): Promise<Neovim | null> {
    const socket = resolveSocket(config)
    if (!socket) {
      logger.warn('no neovim socket address configured (set config.socket, $NVIM_LISTEN_ADDRESS or $NVIM)')
      return null
    }
    try {
      // Fail fast on a dead address: the underlying package leaves socket
      // errors unhandled, so probing is what keeps the host process safe.
      if (!(await probeSocket(socket))) {
        logger.warn(`neovim not reachable at ${socket}`)
        return null
      }
      const nv = tryConnectNvim(socket, config.luaModule)
      if (!nv) return null
      // Touches the wire; re-subscribes debugger events on every reconnect.
      ctx.effect(()=>{
        subscribeNvim(nv)
        subscribeDap(nv)
        return ()=>{
          nv.close()
        }
      });
      return nv
    } catch (error) {
      logger.warn(`neovim connect failed (${socket}): ${errText(error)}`)
      return null
    }
  }

  async function ensureNvim(): Promise<Neovim> {
    if (nvim && !nvim.disconnected) return nvim
    nvim = null
    if (!connectPromise) connectPromise = connect()
    const nv = await connectPromise
    connectPromise = null
    if (!nv) throw new Error('nvim not connected')
    nvim = nv
    return nv
  }

  // Startup probe: connect if Neovim is already up; never blocks the mount.
  void ensureNvim().catch(() => {
    /* the failure was logged by connect(); tools retry on demand */
  })

  // ------------------------------------------------------------------
  // DAP call helpers
  // ------------------------------------------------------------------
  async function dapCall(code: string, args?: LuaArgs): Promise<any> {
    const nv = await ensureNvim()
    return await nv.luaAsyncEval(code, args) || null
  }

  function markSession(exec: ToolRunContext | undefined): void {
    const id = exec?.agent?.session?.id
    if (id) dapSessions.add(String(id))
  }

  async function dapStep(luaFn: string, args: any, exec: ToolRunContext): Promise<unknown> {
    markSession(exec)
    return await dapCall(`agent.${luaFn}(args)`, { args })
  }

  function fmtStep(action: string, ret: any): string {
    if (!ret || ret.status === 'terminated') return '调试目标已退出'
    let out = `## 调试器已暂停 (${action})\n`
    out += `**原因:** ${ret.reason || '?'}\n`
    out += fmtStack({ frames: ret.frames || [], thread_id: ret.thread_id })
    return out
  }

  // ------------------------------------------------------------------
  // Tool definitions
  // ------------------------------------------------------------------
  const specs: ToolSpec[] = [
    {
      name: 'nvim_command',
      output: outNullable(outString('Vim 命令的输出文本')),
      description: "执行 Vim 命令",
      parameters: objectSchema({
        cmd: STRING("Vim 命令，如 'w'、'bp'、'set tabstop=4'"),
      }, ['cmd']),
      async run(args) {
        const nv = await ensureNvim()
        return (await nv.command(args.cmd as string)) ?? null
      },
      format(result) {
        return String(result ?? '(ok)')
      },
    },
    {
      name: 'nvim_lua_command',
      output: OUT_OK,
      description: '在 Neovim 中执行 Lua 语句（无返回值，等价 `lua <cmd>`）。cmd 必须是语句：裸表达式（如 `1+1`、`vim.o.tabstop`）不是合法语句，请改用 nvim_lua_eval。',
      parameters: objectSchema({
        cmd: STRING('Lua 语句（非表达式），如 "vim.opt.tabstop = 4"；裸表达式请用 nvim_lua_eval'),
      }, ['cmd']),
      async run(args) {
        const nv = await ensureNvim()
        await nv.lua(args.cmd as string)
        return { ok: true }
      },
      format() {
        return '(ok)'
      },
    },
    {
      name: 'nvim_lua_eval',
      output: { description: 'Lua 表达式的任意 JSON 返回值（可为 null）' },
      description: '在 Neovim 中异步求值 Lua 表达式并返回结果。cmd 必须是 Lua 表达式：内部按 agent.run_async(function() return <cmd> end, ...) 求值. 多条语句请包成 (function() ... end)()，常用lua模块:[core.agent: 各种给agent提供的工具函数和集成式调试器辅助函数, dap: 调试器, overseer: 编译等运行器], 常用vim函数:[vim.inspect: lua值转字符串, vim.fn: 各种neovim内置函数]',
      parameters: objectSchema({
        cmd: STRING('Lua 表达式'),
      }, ['cmd']),
      async run(args) {
        const nv = await ensureNvim()
        return await nv.luaAsyncEval(args.cmd as string)
      },
      format(result) {
        return JSON.stringify(result)
      },
    },
    {
      name: 'nvim_dap_eval',
      output: outObject(
        {
          result: outString('求值结果文本'),
          type: outString('结果类型'),
          variablesReference: outInteger('可展开的变量引用'),
          namedVariables: outInteger('命名变量数'),
          indexedVariables: outInteger('索引变量数'),
          memoryReference: outString('内存引用'),
        },
        ['result'],
        'DAP evaluate 响应',
        ),
      description: '在当前调试会话中求值表达式',
      parameters: objectSchema({
        expr: STRING('要求值的表达式'),
      }, ['expr']),
      async run(args) {
        const ret = await dapCall(`agent.dap_eval(expr)`, { expr: args.expr as string })
        if (!ret) throw new Error('evaluate failed')
        return ret
      },
      format(result) {
        const val = result.result ?? '(nil)'
        const typ = result.type ?? '?'
        return `**${typ}**: ${val}${result.variablesReference ? ` (variablesReference: ${result.variablesReference})` : ''}`
      },
    },
    {
      name: 'nvim_dap_disasm',
      output: outObject(
        {
          memory_reference: outString('反汇编起始地址'),
          instruction_count: outInteger('请求的指令条数'),
          instruction_offset: outInteger('起始偏移'),
          pc_index: outInteger('PC 所在指令下标（1-based）'),
          instructions: outArray(OUT_INSTRUCTION, '反汇编指令'),
          lines: outArray(outString(), '预格式化的指令行'),
        },
        ['memory_reference', 'instruction_count', 'instruction_offset', 'lines'],
        '反汇编结果',
        ),
      description:
        '反汇编当前调试位置（围绕当前帧指令指针 PC 前后各 16 条指令），或用 memory_reference 反汇编任意地址的机器码',
      parameters: objectSchema({
        memory_reference: STRING('显式反汇编起始地址（缺省用当前帧的 instructionPointerReference）'),
        before: INTEGER('PC 之前的指令条数（默认 16）'),
        after: INTEGER('PC 之后的指令条数（默认 16）'),
        instruction_count: INTEGER('覆盖总指令条数（默认 before+1+after）'),
        instruction_offset: INTEGER('覆盖起始偏移（默认 -before）'),
        resolve_symbols: BOOLEAN('是否请求解析符号'),
      }),
      async run(args, exec) {
        markSession(exec)
        return await dapCall(`agent.dap_disasm(args)`, { args })
      },
      format: fmtDisasm,
    },
    {
      name: 'nvim_dap_start',
      output: outObject(
        {
          ok: OUT_TRUE,
          lang: outString('语言'),
          config_name: outString('配置名'),
          config: outAnyObject('合并后的调试配置'),
        },
        ['ok', 'lang', 'config_name', 'config'],
        '启动调试的结果',
        ),
      description: '启动调试会话。自定义 config 字段会深度合并到预注册配置模板上，可覆盖 program、pid、args 等',
      parameters: objectSchema({
        config_name: STRING('预注册的调试配置名称'),
        lang: STRING('语言（默认 cpp）'),
        config: OBJECT('深度合并到模板的覆盖字段，如 program、pid/processId（附加）、args、cwd、env、request、stopAtEntry'),
      }, ['config_name']),
      async run(args, exec) {
        const ret = await dapCall(`agent.dap_start(args)`, { args })
        markSession(exec)
        return ret
      },
      format(result, args) {
        const cfg = result.config || {}
        return (
          `调试会话已启动: **${result.config_name || args.config_name}** (${result.lang || args.lang || 'cpp'})\n` +
          `- request: ${cfg.request || '?'}\n` +
          `- type: ${cfg.type || '?'}\n` +
          `${cfg.program ? `- program: ${cfg.program}\n` : ''}` +
          `${cfg.cwd ? `- cwd: ${cfg.cwd}\n` : ''}`
        )
      },
    },
    {
      name: 'nvim_dap_continue',
      output: OUT_STEP,
      description: '继续执行。协程阻塞等待命中断点后返回停止位置（vsdbg attach 会话的 continue 须带 threadId，缺省自动用 stopped_thread_id）',
      parameters: objectSchema({
        timeout_ms: INTEGER('等待命中的超时毫秒数（默认 30000）'),
        thread_id: INTEGER('线程 ID（缺省自动用 session.stopped_thread_id）'),
      }),
      run: (args, exec) => dapStep('dap_continue', args, exec),
      format: (result) => fmtStep('continue', result),
    },
    {
      name: 'nvim_dap_stop',
      output: OUT_OK,
      description: '终止当前调试会话',
      parameters: objectSchema({}),
      async run() {
        await dapCall(`agent.dap_stop()`)
        return { ok: true }
      },
      format() {
        return '调试会话已终止'
      },
    },
    {
      name: 'nvim_dap_step_into',
      output: OUT_STEP,
      description: '步入当前函数。协程阻塞等待命中断点后返回停止位置',
      parameters: objectSchema({
        thread_id: INTEGER('线程 ID（可选，默认当前线程）'),
        single_thread: BOOLEAN('仅单步当前线程（默认 false）'),
        granularity: { type: 'string', enum: ['statement', 'line', 'instruction'], description: '粒度: statement（语句）、line（行）、instruction（指令），默认 statement' },
      }),
      run: (args, exec) => dapStep('dap_step_into', args, exec),
      format: (result) => fmtStep('step into', result),
    },
    {
      name: 'nvim_dap_step_over',
      output: OUT_STEP,
      description: '步过当前行。协程阻塞等待命中断点后返回停止位置',
      parameters: objectSchema({
        thread_id: INTEGER('线程 ID（可选，默认当前线程）'),
        single_thread: BOOLEAN('仅单步当前线程（默认 false）'),
        granularity: { type: 'string', enum: ['statement', 'line', 'instruction'], description: '粒度: statement（语句）、line（行）、instruction（指令），默认 statement' },
      }),
      run: (args, exec) => dapStep('dap_step_over', args, exec),
      format: (result) => fmtStep('step over', result),
    },
    {
      name: 'nvim_dap_step_out',
      output: OUT_STEP,
      description: '步出当前函数。协程阻塞等待命中断点后返回停止位置',
      parameters: objectSchema({
        thread_id: INTEGER('线程 ID（可选，默认当前线程）'),
        single_thread: BOOLEAN('仅单步当前线程（默认 false）'),
      }),
      run: (args, exec) => dapStep('dap_step_out', args, exec),
      format: (result) => fmtStep('step out', result),
    },
    {
      name: 'nvim_dap_run_to_cursor',
      output: OUT_STEP,
      description: '运行到当前光标位置。设临时断点→继续执行→命中后移除并返回停止状态',
      parameters: objectSchema({}),
      run: (_args, exec) => dapStep('dap_run_to_cursor', {}, exec),
      format: (result) => fmtStep('run to cursor', result),
    },
    {
      name: 'nvim_dap_run_to_location',
      output: OUT_STEP,
      description: '运行到指定 file:line。设临时断点→继续执行→命中后移除并返回停止状态',
      parameters: objectSchema({
        file: STRING('文件路径（省略则为当前缓冲区）'),
        line: INTEGER('行号（省略则为当前光标行）'),
      }),
      async run(args, exec) {
        markSession(exec)
        return await dapCall(`agent.dap_run_to_location(args)`, { args })
      },
      format: (result) => fmtStep('run to location', result),
    },
    {
      name: 'nvim_dap_get_stack',
      output: outObject(
        {
          thread_id: outInteger('线程 ID'),
          frames: outArray(OUT_FRAME, '栈帧列表'),
          totalFrames: outInteger('总帧数'),
          error: OUT_DAP_ERROR,
        },
        ['thread_id', 'totalFrames'],
        '调用栈结果',
        ),
      description: '获取当前调用栈',
      parameters: objectSchema({}),
      async run(_args, exec) {
        const ret = await dapCall(`agent.dap_get_stack()`)
        markSession(exec)
        return ret
      },
      format: fmtStack,
    },
    {
      name: 'nvim_dap_wait_stop',
      output: outObject(
        {
          status: { type: 'string', enum: ['stopped', 'timeout', 'hang'], description: '等待结果' },
          thread_id: outInteger('停止的线程 ID'),
          stopped_thread_id: outInteger('会话记录的停止线程 ID'),
          frame_name: outString('当前帧名'),
          frame: OUT_FRAME,
          hang: outObject({ pid: outInteger('目标进程 PID'), note: outString('说明') }, [], '死锁信息'),
        },
        ['status'],
        '等待停止的结果',
        ),
      description: '轮询等待调试会话停止（帧名变化 或 stopped_thread_id 变化），不依赖协程阻塞。vsdbg attach 会话 step 后 stackTrace 易被取消时使用；also_respond=true 时额外做 CPU 死锁检测',
      parameters: objectSchema({
        timeout_ms: INTEGER('超时毫秒数（默认 30000）'),
        poll_ms: INTEGER('轮询间隔毫秒数（默认 200）'),
        also_respond: BOOLEAN('true 时额外检测目标进程 CPU 死锁（两次采样 CPU 无增长 + Responding=False → 返回 status="hang"），默认 false'),
        pid: INTEGER('目标进程 PID（also_respond 时使用；缺省尝试从 session.config 推断，无法确定则跳过死锁检测）'),
      }),
      async run(args, exec) {
        markSession(exec)
        return await dapCall(`agent.dap_wait_stop(args)`, { args })
      },
      format(result) {
        if (!result || result.status === 'timeout') return '等待超时（未检测到停止）'
        if (result.status === 'hang') {
          const h = result.hang || {}
          return `疑似死锁: PID ${h.pid ?? '?'} 两次采样 CPU 无增长且 Responding=False`
        }
        return `已检测到停止: thread ${result.thread_id ?? '?'} (stopped_thread_id=${result.stopped_thread_id ?? '?'})\n当前帧: **${result.frame_name ?? '?'}**`
      },
    },
    {
      name: 'nvim_dap_frame_vars',
      output: OUT_VAR_MAP,
      description: '一次调用内完成 stackTrace→定位帧→scopes→variables 收集帧变量（规避 vsdbg frameId 跨调用失效）。frame_match 按帧名子串匹配第一帧',
      parameters: objectSchema({
        thread_id: INTEGER('线程 ID'),
        frame_match: STRING('帧名匹配模式（帧名包含该子串的第一帧）'),
        var_names: {
          type: 'array',
          items: { type: 'string' },
          description: '需要收集的变量名列表（省略则收集该帧全部 Locals 变量）',
        },
      }, ['thread_id', 'frame_match']),
      async run(args, exec) {
        markSession(exec)
        return await dapCall(
          `agent.dap_frame_vars(thread_id, frame_match, var_names)`,
          {
            thread_id: args.thread_id as number,
            frame_match: args.frame_match as string,
            var_names: (args.var_names as string[] | undefined) ?? null,
          }
        )
      },
      format(result) {
        if (result === null || result === undefined) return '(未找到匹配帧或请求失败)'
        const entries = Object.entries(result as Record<string, unknown>)
        if (entries.length === 0) return '(未收集到变量)'
        return entries.map(([name, value]) => `- **${name}** = ${String(value)}`).join('\n')
      },
    },
    {
      name: 'nvim_dap_read_register',
      output: outNullable(outString('寄存器值（16 位 hex 无前缀时已补 0x）')),
      description: '读取寄存器值：scopes→Registers→CPU 分组→variables 展开查找寄存器（vsdbg evaluate 读寄存器受限时使用）。vsdbg 实测 f0 inline 帧（id 通常=1000）的 Registers 才精确，auto_inline 时读取失败自动经 stackTrace 找 f0 帧重试；返回值 16 位 hex 无 0x 前缀自动补 "0x"',
      parameters: objectSchema({
        frame_id: INTEGER('帧 ID（来自 stackTrace，如 nvim_dap_get_stack 的 frames[].id）；省略且 auto_inline 时自动找 f0 inline 帧'),
        reg_name: STRING('寄存器名（大写，如 "RSP"/"RIP"/"RAX"/"RDI"/"RBP"）'),
        auto_inline: BOOLEAN('读取失败时自动经 stackTrace 找 f0 inline 帧重试（默认 true）'),
      }, ['reg_name']),
      async run(args, exec) {
        markSession(exec)
        return await dapCall(
          `agent.dap_read_register(frame_id, reg_name, auto_inline)`,
          {
            frame_id: (args.frame_id as number | undefined) ?? null,
            reg_name: args.reg_name as string,
            auto_inline: (args.auto_inline as boolean | undefined) ?? null,
          }
        )
      },
      format(result, args) {
        const value = String(result ?? '')
        if (!value) return `未找到寄存器 **${args.reg_name}**`
        return `**${args.reg_name}** = ${value}`
      },
    },
    {
      name: 'nvim_dap_read_memory',
      output: OUT_READ_MEMORY,
      description: '读取目标进程内存（DAP readMemory：base64 解码 + 小端 u32/u64 + hex）。readMemory 绕过 vsdbg evaluate 读内存受限问题；auto_pause=true 时目标运行中会自动 pause 后重试',
      parameters: objectSchema({
        memory_reference: STRING('内存引用（十六进制 "0x..." 或十进制字符串；缺省用当前帧 instructionPointerReference）'),
        offset: INTEGER('相对 memory_reference 的字节偏移（默认 0）'),
        count: INTEGER('读取字节数（默认 64）'),
        auto_pause: BOOLEAN('true 时若请求报错含 "running"（目标运行中）先 pause 再重试一次（默认 false）'),
      }),
      async run(args, exec) {
        markSession(exec)
        return await dapCall(`agent.dap_read_memory(args)`, { args })
      },
      format(result) {
        if (!result || !result.ok) return `读取失败: ${result?.error ?? '?'}`
        const n = (result.bytes as string | undefined)?.length ?? 0
        let out = `内存 @ ${result.address ?? '?'} (${n} bytes)\n`
        const hex = (result.hex as string | undefined) ?? ''
        if (hex) out += `hex: ${hex.length > 96 ? hex.slice(0, 96) + '…' : hex}\n`
        const u32 = (result.u32 as number[] | undefined) ?? []
        if (u32.length > 0) out += `u32[0]: 0x${u32[0]!.toString(16)}\n`
        const u64 = (result.u64 as Array<{ lo: number; hi: number; hex: string }> | undefined) ?? []
        if (u64.length > 0) out += `u64[0]: ${u64[0]!.hex}\n`
        return out
      },
    },
    {
      name: 'nvim_dap_read_stack_slot',
      output: OUT_READ_STACK_SLOT,
      description: '读取当前栈上参数槽位（读 RSP → RSP+offset → readMemory），绕过 vsdbg evaluate 限制。如 _CalcInterrupt 参数槽 [rsp+3C8h]=src / [rsp+3D0h]=dest（debug 版）',
      parameters: objectSchema({
        offset: INTEGER('相对 RSP 的字节偏移（默认 0）'),
        size: { type: 'integer', enum: [1, 2, 4, 8], description: '读取字节数（默认 8）' },
        frame_id: INTEGER('读 RSP 使用的帧 ID（缺省自动找 f0 inline 帧）'),
      }),
      async run(args, exec) {
        markSession(exec)
        return await dapCall(`agent.dap_read_stack_slot(args)`, { args })
      },
      format(result) {
        if (!result || !result.ok) return `读取失败: ${result?.error ?? '?'}`
        let out = `RSP = 0x${(result.rsp as number).toString(16)} → 槽位 @ 0x${(result.addr as number).toString(16)}\n`
        const u64 = (result.u64 as Array<{ lo: number; hi: number; hex: string }> | undefined) ?? []
        if (u64.length > 0) out += `u64[0]: ${u64[0]!.hex}\n`
        const u32 = (result.u32 as number[] | undefined) ?? []
        if (u32.length > 0) out += `u32[0]: 0x${u32[0]!.toString(16)}\n`
        if (result.hex) out += `hex: ${result.hex}\n`
        return out
      },
    },
    {
      name: 'nvim_dap_address_classify',
      output: outObject(
        {
          kind: { type: 'string', enum: ['stack', 'heap', 'unknown'], description: '地址归属' },
          delta: outInteger('与 RSP 的绝对差'),
          rsp: outInteger('RSP 值'),
          addr: outInteger('地址值'),
        },
        ['kind', 'delta'],
        '地址归属判定结果',
        ),
      description: '判定内存地址属于栈还是堆（|addr - RSP| < 1MB → stack，否则 heap）。用途：0x1D5D77 BTS 失败断点判别 RCX 堆/栈地址，区分首次失败（堆=有效现场）与回滚命中（栈=跳过）',
      parameters: objectSchema({
        addr: STRING('地址（十六进制 "0x..." 或十进制字符串）'),
      }, ['addr']),
      async run(args, exec) {
        markSession(exec)
        return await dapCall(`agent.dap_address_classify(args)`, { args })
      },
      format(result) {
        if (!result) return '(无结果)'
        const addr = result.addr != null ? `0x${(result.addr as number).toString(16)}` : '?'
        const rsp = result.rsp != null ? `0x${(result.rsp as number).toString(16)}` : '?'
        const kind =
          result.kind === 'stack' ? '栈（回滚跳过/局部）' : result.kind === 'heap' ? '堆（有效现场）' : '未知'
        return `地址 ${addr} → **${kind}** (delta=${result.delta}, RSP=${rsp})`
      },
    },
    {
      name: 'nvim_dap_check_hang',
      output: OUT_CHECK_HANG,
      description: '卡死检查（独立函数 M.dap_check_hang）：两次采样目标进程 CPU 无增长（delta < hang_delta，默认 0.01 秒）且 Responding=False 判定 status=hang。须在目标自由运行时调用（暂停时 CPU delta=0 是假象）。PID 缺省从 session.config 推断，无法确定返回 skipped',
      parameters: objectSchema({
        pid: INTEGER('目标进程 PID（缺省尝试从 session.config 推断）'),
        interval_ms: INTEGER('两次采样间隔毫秒数（默认 300）'),
        hang_delta: { type: 'number', description: 'CPU 增量判定阈值秒数（默认 0.01）' },
      }),
      async run(args, exec) {
        markSession(exec)
        return await dapCall(`agent.dap_check_hang(args)`, { args })
      },
      format(result) {
        if (!result) return '(无结果)'
        if (result.status === 'skipped') return '跳过: ' + (result.note || '无法判定')
        if (result.status === 'hang') {
          const h = result.hang || {}
          return '疑似死锁 (PID=' + result.pid + '): CPU ' + result.cpu1 + '->' + result.cpu2 + ' delta=' + result.delta + ' 两次 Responding=False\n' + (h.note || '')
        }
        return '未死锁 (PID=' + result.pid + '): CPU ' + result.cpu1 + '->' + result.cpu2 + ' delta=' + result.delta + ' Responding=' + result.responding2
      },
    },
    {
      name: 'nvim_dap_get_threads',
      output: outObject(
        { threads: outArray(OUT_THREAD, '线程列表') },
        ['threads'],
        '线程列表',
        ),
      description: '获取所有线程',
      parameters: objectSchema({}),
      async run() {
        return await dapCall(`agent.dap_get_threads()`)
      },
      format: fmtThreads,
    },
    {
      name: 'nvim_dap_switch_thread',
      output: outObject(
        {
          thread_id: outInteger('线程 ID'),
          thread_name: outString('线程名'),
          frame: OUT_FRAME,
          frames: outArray(OUT_FRAME, '栈帧列表'),
          totalFrames: outInteger('总帧数'),
          error: OUT_DAP_ERROR,
        },
        ['thread_id', 'frames', 'totalFrames'],
        '切换线程的结果',
        ),
      description: '切换到指定线程',
      parameters: objectSchema({
        thread_id: INTEGER('线程 ID'),
      }, ['thread_id']),
      async run(args, exec) {
        markSession(exec)
        return await dapCall(`agent.dap_switch_thread(args)`, { args })
      },
      format(result) {
        return fmtSwitchThread(result)
      },
    },
    {
      name: 'nvim_dap_get_sessions',
      output: outObject(
        { sessions: outArray(OUT_SESSION, '调试会话列表') },
        ['sessions'],
        '调试会话列表',
        ),
      description: '列出所有活跃调试会话',
      parameters: objectSchema({}),
      async run() {
        return await dapCall(`agent.dap_get_sessions()`)
      },
      format: fmtSessions,
    },
    {
      name: 'nvim_dap_switch_session',
      output: outObject(
        {
          session: outObject(
            {
              name: outString('会话名'),
              id: outInteger('会话 ID'),
              type: outString('适配器类型'),
            },
            ['name'],
            '当前调试会话',
            ),
        },
        ['session'],
        '切换会话的结果',
        ),
      description: '按名称切换调试会话',
      parameters: objectSchema({
        session_name: STRING('会话名称'),
      }, ['session_name']),
      async run(args, exec) {
        markSession(exec)
        return await dapCall(`agent.dap_switch_session(args)`, { args })
      },
      format(result) {
        const s = result.session
        return `已切换到会话: **${s.name}** (id: ${s.id}, type: ${s.type || '?'})`
      },
    },
    {
      name: 'nvim_dap_add_watch',
      output: outObject({ expression: outString('监视表达式') }, ['expression'], '添加监视的结果'),
      description: '添加监视表达式到调试 UI',
      parameters: objectSchema({
        expr: STRING('监视表达式'),
      }, ['expr']),
      async run(args) {
        return await dapCall(`agent.dap_add_watch(args)`, { args })
      },
      format(result, args) {
        return `已添加监视: **${result.expression || args.expr}**`
      },
    },
    {
      name: 'nvim_dap_add_breakpoint',
      output: outObject(
        {
          file: outString('文件路径'),
          line: outInteger('行号'),
          condition: outString('条件表达式'),
        },
        ['file', 'line'],
        '添加断点的结果',
        ),
      description: '在 file:line 添加断点（支持条件、命中次数、日志）',
      parameters: objectSchema({
        file: STRING('文件路径（省略则为当前缓冲区）'),
        line: INTEGER('行号（1-based）'),
        condition: STRING('条件表达式'),
        hit_condition: STRING('命中次数条件（如 "3" = 第3次命中时中断）'),
        log_message: STRING('日志消息（logpoint，支持 {var} 插值）'),
      }, ['line']),
      async run(args, exec) {
        markSession(exec)
        return await dapCall(`agent.dap_add_breakpoint(args)`, { args })
      },
      format(result) {
        const file = result.file ? result.file.replace(/^.*[\\/]/, '') : '?'
        let out = `断点已添加: ${file}:${result.line}`
        if (result.condition) out += ` (条件: ${result.condition})`
        return out
      },
    },
    {
      name: 'nvim_dap_add_function_breakpoint',
      output: outObject(
        {
          breakpoints: outArray(
            outObject(
              {
                id: outInteger('断点 ID'),
                verified: outBoolean('是否已验证'),
                message: outString('验证信息'),
                line: outInteger('行号'),
                source: outAnyObject('断点来源'),
              },
              [],
              '函数断点',
            ),
            '函数断点列表',
            ),
        },
        [],
        'DAP setFunctionBreakpoints 响应',
        ),
      description: '添加函数断点（按函数名，无需指定文件和行号）',
      parameters: objectSchema({
        func: STRING('函数名'),
      }, ['func']),
      async run(args, exec) {
        markSession(exec)
        return await dapCall(`agent.dap_add_function_breakpoint(args)`, { args })
      },
      format(result, args) {
        const bps = (result as any)?.breakpoints
        if (bps && bps.length > 0) {
          const b = bps[0]
          return `函数断点已添加: **${args.func}**${b.verified ? ' (已验证)' : ` (未验证: ${b.message || '?'})`}`
        }
        return `函数断点已添加: **${args.func}**`
      },
    },
    {
      name: 'nvim_dap_toggle_breakpoint',
      output: outObject(
        { ok: OUT_TRUE, file: outString('文件路径'), line: outInteger('行号') },
        ['ok', 'file', 'line'],
        '切换断点的结果',
        ),
      description: '切换断点（有则删、无则加）',
      parameters: objectSchema({
        file: STRING('文件路径（省略则为当前缓冲区）'),
        line: INTEGER('行号（省略则为当前光标行）'),
        condition: STRING('条件表达式（仅添加时）'),
        hit_condition: STRING('命中次数条件（仅添加时）'),
        log_message: STRING('日志消息（仅添加时）'),
      }),
      async run(args, exec) {
        markSession(exec)
        return await dapCall(`agent.dap_toggle_breakpoint(args)`, { args })
      },
      format(result) {
        const file = result.file ? result.file.replace(/^.*[\\/]/, '') : '?'
        return `断点已切换: ${file}:${result.line}`
      },
    },
    {
      name: 'nvim_dap_remove_breakpoint',
      output: outObject(
        { removed: outBoolean('是否删除了断点'), file: outString('文件路径'), line: outInteger('行号') },
        ['file', 'line'],
        '删除断点的结果',
        ),
      description: '删除指定位置的断点',
      parameters: objectSchema({
        file: STRING('文件路径（省略则为当前缓冲区）'),
        line: INTEGER('行号（1-based）'),
      }, ['line']),
      async run(args) {
        return await dapCall(`agent.dap_remove_breakpoint(args)`, { args })
      },
      format(result) {
        const file = result.file ? result.file.replace(/^.*[\\/]/, '') : '?'
        return `断点已删除: ${file}:${result.line}${result.removed ? '' : ' (该位置无断点)'}`
      },
    },
    {
      name: 'nvim_dap_list_breakpoints',
      output: outObject(
        { breakpoints: outArray(OUT_BREAKPOINT, '断点列表') },
        ['breakpoints'],
        '断点列表',
        ),
      description: '列出所有断点及其属性',
      parameters: objectSchema({}),
      async run() {
        return await dapCall(`agent.dap_list_breakpoints()`)
      },
      format: fmtBreakpoints,
    },
    {
      name: 'nvim_dap_clear_breakpoints',
      output: OUT_OK,
      description: '清除所有断点',
      parameters: objectSchema({}),
      async run() {
        await dapCall(`agent.dap_clear_breakpoints()`)
        return { ok: true }
      },
      format() {
        return '所有断点已清除'
      },
    },
    {
      name: 'nvim_dap_request',
      output: OUT_DAP_RESPONSE,
      description:
        '发送任意 DAP 请求到调试适配器。若请求不被支持，返回适配器能力列表；若参数错误，提示查询 DAP 协议文档。' +
        '常用命令: evaluate, stackTrace, threads, scopes, variables, disassemble, setBreakpoints, setFunctionBreakpoints, setExceptionBreakpoints, source, modules, loadedSources, goto, stepIn, stepOut, next, continue, pause, terminate, restart, initialize, launch, attach 等',
      parameters: objectSchema({
        command: STRING('DAP 请求命令名（如 "evaluate"、"stackTrace"、"threads"、"disassemble"、"setFunctionBreakpoints"）'),
        arguments: OBJECT('请求参数（JSON 对象），如 {"expression": "x", "frameId": 0}'),
      }, ['command']),
      async run(args, exec) {
        markSession(exec)
        return await dapCall(`agent.dap_request(args)`, { args })
      },
      format(result) {
        if (result == null) return '(无响应体)'
        if (result.error) {
          let out = `**DAP 请求错误:** ${result.error}

`
          if (result.hint) out += `${result.hint}

`
          if (result.capabilities) {
            out += `**适配器支持的能力:**
`
            const caps: Record<string, unknown> = result.capabilities
            const supported = Object.entries(caps).filter(([, v]) => v)
            if (supported.length === 0) {
              out += `(无能力信息)
`
            } else {
              for (const [k] of supported) out += `- ${k}
`
            }
          }
          return out
        }
        return JSON.stringify(result, null, 2)
      },
    },
    {
      name: 'nvim_dap_get_configurations',
      output: outObject(
        { configurations: outArray(OUT_CONFIGURATION, '调试配置列表') },
        ['configurations'],
        '调试配置列表',
        ),
      description: '列出可用的调试配置（可按语言筛选）',
      parameters: objectSchema({
        lang: STRING('按语言筛选（如 cpp、python），省略则列出全部'),
      }),
      async run(args) {
        return await dapCall(`agent.dap_get_configurations(args)`, { args })
      },
      format: fmtConfigs,
    },
  ]

  for (const spec of specs) {
    const definition: ToolDefinition = {
      name: spec.name,
      description: spec.description,
      parameters: spec.parameters,
      output: {
        schema: spec.output,
        render: (args, value) => [{ type: 'text', text: spec.format(value, args) }],
      },
      async execute(args, exec) {
        const ret = await spec.run(args, exec)
        return ret || null
      },
    }
    ctx.tools.register(definition)
  }

  // ------------------------------------------------------------------
  // File-write observation (OpenCode `tool.execute.after` equivalent)
  // ------------------------------------------------------------------
  ctx.on('tools/result', (exec: Readonly<ToolExecution>) => {
    if (exec?.name !== 'write' && exec?.name !== 'edit') return
    const args = exec?.arguments as { path?: unknown; file_path?: unknown } | undefined
    const filePath = args?.path ?? args?.file_path
    if (typeof filePath !== 'string' || filePath === '') return
    const nv = nvim
    if (!nv || nv.disconnected) return
    nv.lua(`agent.reload_file(path)`, { path: filePath }).catch((error) => {
      logger.error(`reload_file failed for ${filePath}: ${errText(error)}`)
    })
  })

  // ------------------------------------------------------------------
  // /neovim command
  // ------------------------------------------------------------------
  ctx.commands.register({
    name: 'neovim',
    description: 'Inspect or reconnect the Neovim bridge: status | reconnect',
    input: { hint: 'status | reconnect' },
    async handler(invocation) {
      const raw = invocation.rawInput.trim()
      const [verb, ...rest] = raw.split(/\s+/).filter(Boolean)
      const effectiveVerb = verb === undefined || verb === '' ? 'status' : verb
      if (rest.length > 0) {
        return { kind: 'error', text: `/neovim ${effectiveVerb} takes no arguments` }
      }
      try {
        switch (effectiveVerb) {
          case 'status': {
            const socket = resolveSocket(config)
            const connected = nvim !== null && !nvim.disconnected
            let text = `Neovim bridge:\n- socket: ${socket ?? '(unset)'}\n- connected: ${connected}`
            if (connected && nvim) {
              try {
                text += `\n- channel: ${await nvim.channelId()}`
              } catch {
                text += `\n- channel: (unknown)`
              }
              text += `\n- lua module: ${config.luaModule}\n- dap sessions tracked: ${dapSessions.size}`
            }
            return { kind: 'success', text }
          }
          case 'reconnect': {
            nvim = null
            const nv = await ensureNvim()
            return { kind: 'success', text: `reconnected, channel ${await nv.channelId()}` }
          }
          default:
            return { kind: 'error', text: `unknown /neovim subcommand '${effectiveVerb}'; expected status | reconnect` }
        }
      } catch (error) {
        return { kind: 'error', text: `/neovim failed: ${errText(error)}` }
      }
    },
  })
}
