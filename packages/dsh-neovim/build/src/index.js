/**
 * dsh-neovim — Neovim bridge for DeepSeek Harness.
 *
 * Port of the OpenCode `neovim-api` plugin onto the DSH plugin contract:
 *
 *   - `ctx.tools.register` exposes vim command / Lua execution / Lua
 *     evaluation plus the full nvim-dap toolset (`dap_start`, steps,
 *     breakpoints, watches, threads, sessions, configurations) through the
 *     user's Neovim `core.agent` Lua bridge.
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
import { resolveConfig, resolveSocket } from './config.js';
import { probeSocket, tryConnectNvim } from './neovim.js';
import { fmtBreakpoints, fmtConfigs, fmtFrame, fmtSessions, fmtStack, fmtThreads, } from './format.js';
export const name = 'dsh-neovim';
export const inject = ['tools', 'commands', 'agents'];
export { resolveConfig, resolveSocket };
export { Neovim, probeSocket, tryConnectNvim } from './neovim.js';
function makeLogger(ctx) {
    try {
        if (typeof ctx?.logger === 'function')
            return ctx.logger('neovim');
        if (ctx?.logger)
            return ctx.logger;
    }
    catch {
        /* fall through */
    }
    return { debug() { }, info() { }, warn() { }, error() { } };
}
function errText(error) {
    return error instanceof Error ? error.message : String(error);
}
/** JSON-Schema root for tool parameters (dsh-lsp convention). */
function objectSchema(properties, required = []) {
    return { type: 'object', additionalProperties: false, properties, required };
}
const STRING = (description) => ({ type: 'string', description });
const INTEGER = (description) => ({ type: 'integer', description });
const OBJECT = (description) => ({
    type: 'object',
    additionalProperties: true,
    description,
});
export function apply(ctx, rawConfig = {}) {
    const config = resolveConfig(rawConfig);
    const logger = makeLogger(ctx);
    // ------------------------------------------------------------------
    // Connection lifecycle (lazy, self-healing)
    // ------------------------------------------------------------------
    let nvim = null;
    let connectPromise = null;
    /** Sessions that used a DAP tool; debugger events are injected only there. */
    const dapSessions = new Set();
    function broadcastDapEvent(text) {
        for (const agent of ctx.agents.list()) {
            const id = agent.session?.id;
            if (!id || !dapSessions.has(String(id)))
                continue;
            try {
                agent.inject({ content: text, source: { kind: 'plugin', plugin: name } });
            }
            catch (error) {
                // The agent went away between list() and inject(); drop the stale entry.
                dapSessions.delete(String(id));
                logger.error(`dap event delivery failed for session ${id}: ${errText(error)}`);
            }
        }
    }
    async function subscribeDap(nv) {
        const channelId = await nv.channelId();
        nv.lua(`require("${config.luaModule}").dap_subscribe(${channelId})`).catch((error) => {
            logger.error(`dap_subscribe error: ${errText(error)}`);
        });
        logger.info(`listening for neovim debugger events (channel ${channelId})`);
        await nv.subscribe('dap_pause', (args) => {
            const a = args?.[0] ?? args;
            let text = `<dap-event type="stopped">\n`;
            text += `## 调试器已暂停\n`;
            text += `**会话:** ${a.config_name || a.session || '?'} (id: ${a.session ?? '?'})\n`;
            const reason = a.stop_event?.reason;
            if (reason)
                text += `**原因:** ${reason}\n`;
            text += fmtStack({ frames: a.stacks_top_10 || [], thread_id: a.thread_id });
            text += `\n</dap-event>`;
            logger.info(text);
            broadcastDapEvent(text);
        });
        await nv.subscribe('event_terminated', (args) => {
            const a = args?.[0] ?? args;
            const text = `<dap-event type="terminated">\n## 调试会话已终止\n` +
                `**会话:** ${a.config_name || a.session || '?'} (id: ${a.session ?? '?'})\n</dap-event>`;
            broadcastDapEvent(text);
        });
        await nv.subscribe('event_exited', (args) => {
            const a = args?.[0] ?? args;
            const exitCode = a.event?.exitCode;
            const text = `<dap-event type="exited">\n## 调试目标已退出\n` +
                `**会话:** ${a.config_name || a.session || '?'} (id: ${a.session ?? '?'})` +
                `${exitCode != null ? `\n**退出码:** ${exitCode}` : ''}\n</dap-event>`;
            broadcastDapEvent(text);
        });
    }
    async function connect() {
        const socket = resolveSocket(config);
        if (!socket) {
            logger.warn('no neovim socket address configured (set config.socket, $NVIM_LISTEN_ADDRESS or $NVIM)');
            return null;
        }
        try {
            // Fail fast on a dead address: the underlying package leaves socket
            // errors unhandled, so probing is what keeps the host process safe.
            if (!(await probeSocket(socket))) {
                logger.warn(`neovim not reachable at ${socket}`);
                return null;
            }
            const nv = tryConnectNvim(socket);
            if (!nv)
                return null;
            // Touches the wire; re-subscribes debugger events on every reconnect.
            await subscribeDap(nv);
            return nv;
        }
        catch (error) {
            logger.warn(`neovim connect failed (${socket}): ${errText(error)}`);
            return null;
        }
    }
    async function ensureNvim() {
        if (nvim && !nvim.disconnected)
            return nvim;
        nvim = null;
        if (!connectPromise)
            connectPromise = connect();
        const nv = await connectPromise;
        connectPromise = null;
        if (!nv)
            throw new Error('nvim not connected');
        nvim = nv;
        return nv;
    }
    // Startup probe: connect if Neovim is already up; never blocks the mount.
    void ensureNvim().catch(() => {
        /* the failure was logged by connect(); tools retry on demand */
    });
    // ------------------------------------------------------------------
    // DAP call helpers
    // ------------------------------------------------------------------
    async function dapCall(code, args) {
        const nv = await ensureNvim();
        return await nv.luaAsyncEval(code, config.luaModule, args);
    }
    function markSession(exec) {
        const id = exec?.agent?.session?.id;
        if (id)
            dapSessions.add(String(id));
    }
    async function dapStep(action, luaFn, exec) {
        markSession(exec);
        const ret = await dapCall(`require("${config.luaModule}").${luaFn}()`);
        if (ret.status === 'terminated')
            return '调试目标已退出';
        let out = `## 调试器已暂停 (${action})\n`;
        out += `**原因:** ${ret.reason || '?'}\n`;
        out += fmtStack({ frames: ret.frames || [], thread_id: ret.thread_id });
        return out;
    }
    // ------------------------------------------------------------------
    // Tool definitions
    // ------------------------------------------------------------------
    const specs = [
        {
            name: 'nvim_command',
            description: "执行 Vim 命令",
            parameters: objectSchema({ cmd: STRING("Vim 命令，如 'w'、'bp'、'set tabstop=4'") }, ['cmd']),
            async run(args) {
                const nv = await ensureNvim();
                const ret = await nv.command(args.cmd);
                return String(ret ?? '(ok)');
            },
        },
        {
            name: 'nvim_lua_command',
            description: '在 Neovim 中执行 Lua 语句（无返回值）',
            parameters: objectSchema({ cmd: STRING("Lua 语句，如 'vim.opt.tabstop = 4'") }, ['cmd']),
            async run(args) {
                const nv = await ensureNvim();
                await nv.lua(args.cmd);
                return '(ok)';
            },
        },
        {
            name: 'nvim_lua_eval',
            description: '在 Neovim 中求值 Lua 表达式并返回结果',
            parameters: objectSchema({
                cmd: STRING("Lua 表达式，如 'vim.o.tabstop'、'vim.api.nvim_get_current_buf()'"),
            }, ['cmd']),
            async run(args) {
                const nv = await ensureNvim();
                const ret = await nv.luaEval(args.cmd);
                return JSON.stringify(ret);
            },
        },
        {
            name: 'nvim_dap_eval',
            description: '在当前调试会话中求值表达式',
            parameters: objectSchema({ expr: STRING('要求值的表达式') }, ['expr']),
            async run(args) {
                const ret = await dapCall(`require("${config.luaModule}").dap_eval(expr)`, { expr: args.expr });
                if (!ret)
                    throw new Error('evaluate failed');
                const val = ret.result ?? '(nil)';
                const typ = ret.type ?? '?';
                return `**${typ}**: ${val}${ret.variablesReference ? ` (variablesReference: ${ret.variablesReference})` : ''}`;
            },
        },
        {
            name: 'nvim_dap_start',
            description: '启动调试会话。自定义 config 字段会深度合并到预注册配置模板上，可覆盖 program、pid、args 等',
            parameters: objectSchema({
                config_name: STRING('预注册的调试配置名称'),
                lang: STRING('语言（默认 cpp）'),
                config: OBJECT('深度合并到模板的覆盖字段，如 program、pid/processId（附加）、args、cwd、env、request、stopAtEntry'),
            }, ['config_name']),
            async run(args, exec) {
                const ret = await dapCall(`require("${config.luaModule}").dap_start(args)`, { args });
                markSession(exec);
                const cfg = ret.config || {};
                return (`调试会话已启动: **${ret.config_name || args.config_name}** (${ret.lang || args.lang || 'cpp'})\n` +
                    `- request: ${cfg.request || '?'}\n` +
                    `- type: ${cfg.type || '?'}\n` +
                    `${cfg.program ? `- program: ${cfg.program}\n` : ''}` +
                    `${cfg.cwd ? `- cwd: ${cfg.cwd}\n` : ''}`);
            },
        },
        {
            name: 'nvim_dap_continue',
            description: '继续执行。协程阻塞等待命中断点后返回停止位置',
            parameters: objectSchema({}),
            run: (_args, exec) => dapStep('continue', 'dap_continue', exec),
        },
        {
            name: 'nvim_dap_stop',
            description: '终止当前调试会话',
            parameters: objectSchema({}),
            async run() {
                await dapCall(`require("${config.luaModule}").dap_stop()`);
                return '调试会话已终止';
            },
        },
        {
            name: 'nvim_dap_step_into',
            description: '步入当前函数。协程阻塞等待命中断点后返回停止位置',
            parameters: objectSchema({}),
            run: (_args, exec) => dapStep('step into', 'dap_step_into', exec),
        },
        {
            name: 'nvim_dap_step_over',
            description: '步过当前行。协程阻塞等待命中断点后返回停止位置',
            parameters: objectSchema({}),
            run: (_args, exec) => dapStep('step over', 'dap_step_over', exec),
        },
        {
            name: 'nvim_dap_step_out',
            description: '步出当前函数。协程阻塞等待命中断点后返回停止位置',
            parameters: objectSchema({}),
            run: (_args, exec) => dapStep('step out', 'dap_step_out', exec),
        },
        {
            name: 'nvim_dap_run_to_cursor',
            description: '运行到当前光标位置。设临时断点→继续执行→命中后移除并返回停止状态',
            parameters: objectSchema({}),
            run: (_args, exec) => dapStep('run to cursor', 'dap_run_to_cursor', exec),
        },
        {
            name: 'nvim_dap_run_to_location',
            description: '运行到指定 file:line。设临时断点→继续执行→命中后移除并返回停止状态',
            parameters: objectSchema({
                file: STRING('文件路径（省略则为当前缓冲区）'),
                line: INTEGER('行号（省略则为当前光标行）'),
            }),
            async run(args, exec) {
                markSession(exec);
                const ret = await dapCall(`require("${config.luaModule}").dap_run_to_location(args)`, { args });
                if (ret.status === 'terminated')
                    return '调试目标已退出';
                let out = `## 调试器已暂停 (run to location)\n`;
                out += `**原因:** ${ret.reason || '?'}\n`;
                out += fmtStack({ frames: ret.frames || [], thread_id: ret.thread_id });
                return out;
            },
        },
        {
            name: 'nvim_dap_get_stack',
            description: '获取当前调用栈',
            parameters: objectSchema({}),
            async run(_args, exec) {
                const ret = await dapCall(`require("${config.luaModule}").dap_get_stack()`);
                markSession(exec);
                return fmtStack(ret);
            },
        },
        {
            name: 'nvim_dap_get_threads',
            description: '获取所有线程',
            parameters: objectSchema({}),
            async run() {
                const ret = await dapCall(`require("${config.luaModule}").dap_get_threads()`);
                return fmtThreads(ret);
            },
        },
        {
            name: 'nvim_dap_switch_thread',
            description: '切换到指定线程',
            parameters: objectSchema({ thread_id: INTEGER('线程 ID') }, ['thread_id']),
            async run(args, exec) {
                markSession(exec);
                const ret = await dapCall(`require("${config.luaModule}").dap_switch_thread(args)`, { args });
                const f = ret.frame;
                return `已切换到线程 ${ret.thread_id}\n当前帧: ${fmtFrame(f)}`;
            },
        },
        {
            name: 'nvim_dap_get_sessions',
            description: '列出所有活跃调试会话',
            parameters: objectSchema({}),
            async run() {
                const ret = await dapCall(`require("${config.luaModule}").dap_get_sessions()`);
                return fmtSessions(ret);
            },
        },
        {
            name: 'nvim_dap_switch_session',
            description: '按名称切换调试会话',
            parameters: objectSchema({ session_name: STRING('会话名称') }, ['session_name']),
            async run(args, exec) {
                markSession(exec);
                const ret = await dapCall(`require("${config.luaModule}").dap_switch_session(args)`, { args });
                const s = ret.session;
                return `已切换到会话: **${s.name}** (id: ${s.id}, type: ${s.type || '?'})`;
            },
        },
        {
            name: 'nvim_dap_add_watch',
            description: '添加监视表达式到调试 UI',
            parameters: objectSchema({ expr: STRING('监视表达式') }, ['expr']),
            async run(args) {
                const ret = await dapCall(`require("${config.luaModule}").dap_add_watch(args)`, { args });
                return `已添加监视: **${ret.expression || args.expr}**`;
            },
        },
        {
            name: 'nvim_dap_add_breakpoint',
            description: '在 file:line 添加断点（支持条件、命中次数、日志）',
            parameters: objectSchema({
                file: STRING('文件路径（省略则为当前缓冲区）'),
                line: INTEGER('行号（1-based）'),
                condition: STRING('条件表达式'),
                hit_condition: STRING('命中次数条件（如 "3" = 第3次命中时中断）'),
                log_message: STRING('日志消息（logpoint，支持 {var} 插值）'),
            }, ['line']),
            async run(args, exec) {
                markSession(exec);
                const ret = await dapCall(`require("${config.luaModule}").dap_add_breakpoint(args)`, { args });
                const file = ret.file ? ret.file.replace(/^.*[\\/]/, '') : '?';
                let out = `断点已添加: ${file}:${ret.line}`;
                if (ret.condition)
                    out += ` (条件: ${ret.condition})`;
                return out;
            },
        },
        {
            name: 'nvim_dap_toggle_breakpoint',
            description: '切换断点（有则删、无则加）',
            parameters: objectSchema({
                file: STRING('文件路径（省略则为当前缓冲区）'),
                line: INTEGER('行号（省略则为当前光标行）'),
                condition: STRING('条件表达式（仅添加时）'),
                hit_condition: STRING('命中次数条件（仅添加时）'),
                log_message: STRING('日志消息（仅添加时）'),
            }),
            async run(args, exec) {
                markSession(exec);
                const ret = await dapCall(`require("${config.luaModule}").dap_toggle_breakpoint(args)`, { args });
                const file = ret.file ? ret.file.replace(/^.*[\\/]/, '') : '?';
                return `断点已切换: ${file}:${ret.line}`;
            },
        },
        {
            name: 'nvim_dap_remove_breakpoint',
            description: '删除指定位置的断点',
            parameters: objectSchema({
                file: STRING('文件路径（省略则为当前缓冲区）'),
                line: INTEGER('行号（1-based）'),
            }, ['line']),
            async run(args) {
                const ret = await dapCall(`require("${config.luaModule}").dap_remove_breakpoint(args)`, { args });
                const file = ret.file ? ret.file.replace(/^.*[\\/]/, '') : '?';
                return `断点已删除: ${file}:${ret.line}${ret.removed ? '' : ' (该位置无断点)'}`;
            },
        },
        {
            name: 'nvim_dap_list_breakpoints',
            description: '列出所有断点及其属性',
            parameters: objectSchema({}),
            async run() {
                const ret = await dapCall(`require("${config.luaModule}").dap_list_breakpoints()`);
                return fmtBreakpoints(ret);
            },
        },
        {
            name: 'nvim_dap_clear_breakpoints',
            description: '清除所有断点',
            parameters: objectSchema({}),
            async run() {
                await dapCall(`require("${config.luaModule}").dap_clear_breakpoints()`);
                return '所有断点已清除';
            },
        },
        {
            name: 'nvim_dap_get_configurations',
            description: '列出可用的调试配置（可按语言筛选）',
            parameters: objectSchema({ lang: STRING('按语言筛选（如 cpp、python），省略则列出全部') }),
            async run(args) {
                const ret = await dapCall(`require("${config.luaModule}").dap_get_configurations(args)`, { args });
                return fmtConfigs(ret);
            },
        },
    ];
    for (const spec of specs) {
        const definition = {
            name: spec.name,
            description: spec.description,
            parameters: spec.parameters,
            output: {
                schema: { type: 'string' },
                render: (_args, value) => [{ type: 'text', text: String(value) }],
            },
            async execute(args, exec) {
                return await spec.run(args, exec);
            },
        };
        ctx.tools.register(definition);
    }
    // ------------------------------------------------------------------
    // File-write observation (OpenCode `tool.execute.after` equivalent)
    // ------------------------------------------------------------------
    ctx.on('tools/result', (exec) => {
        if (exec?.name !== 'write' && exec?.name !== 'edit')
            return;
        const args = exec?.arguments;
        const filePath = args?.path ?? args?.file_path;
        if (typeof filePath !== 'string' || filePath === '')
            return;
        const nv = nvim;
        if (!nv || nv.disconnected)
            return;
        nv.lua(`require("${config.luaModule}").reload_file(path)`, { path: filePath }).catch((error) => {
            logger.error(`reload_file failed for ${filePath}: ${errText(error)}`);
        });
    });
    // ------------------------------------------------------------------
    // /neovim command
    // ------------------------------------------------------------------
    ctx.commands.register({
        name: 'neovim',
        description: 'Inspect or reconnect the Neovim bridge: status | reconnect',
        input: { hint: 'status | reconnect' },
        async handler(invocation) {
            const raw = invocation.rawInput.trim();
            const [verb, ...rest] = raw.split(/\s+/).filter(Boolean);
            const effectiveVerb = verb === undefined || verb === '' ? 'status' : verb;
            if (rest.length > 0) {
                return { kind: 'error', text: `/neovim ${effectiveVerb} takes no arguments` };
            }
            try {
                switch (effectiveVerb) {
                    case 'status': {
                        const socket = resolveSocket(config);
                        const connected = nvim !== null && !nvim.disconnected;
                        let text = `Neovim bridge:\n- socket: ${socket ?? '(unset)'}\n- connected: ${connected}`;
                        if (connected && nvim) {
                            try {
                                text += `\n- channel: ${await nvim.channelId()}`;
                            }
                            catch {
                                text += `\n- channel: (unknown)`;
                            }
                            text += `\n- lua module: ${config.luaModule}\n- dap sessions tracked: ${dapSessions.size}`;
                        }
                        return { kind: 'success', text };
                    }
                    case 'reconnect': {
                        nvim = null;
                        const nv = await ensureNvim();
                        return { kind: 'success', text: `reconnected, channel ${await nv.channelId()}` };
                    }
                    default:
                        return { kind: 'error', text: `unknown /neovim subcommand '${effectiveVerb}'; expected status | reconnect` };
                }
            }
            catch (error) {
                return { kind: 'error', text: `/neovim failed: ${errText(error)}` };
            }
        },
    });
}
