# dsh-neovim 架构

## 模块

| 文件 | 职责 |
|---|---|
| `src/types.ts` | 内部类型：harness 结构契约（`ctx.tools`/`ctx.commands`/`ctx.on`/`ctx.agents`/`ctx.effect` 的最小结构面）、配置、工具定义形状 |
| `src/config.ts` | 配置解析：`resolveConfig`（行 config → `{ socket, luaModule }`）+ `resolveSocket`（显式 → `$NVIM_LISTEN_ADDRESS` → `$NVIM` → 平台默认） |
| `src/format.ts` | 纯函数 markdown 渲染：栈帧/调用栈（`location` 拼写已修正）/断点/配置/会话/线程表格 |
| `src/neovim.ts` | `Neovim` 客户端封装（neovim 包）：聚合通知订阅、`command`/`lua`/`luaEval`/`luaAsyncEval`（`run_async` 协程桥 + `async_task_finish` 通知）、断开检测与 pending 任务结算、`probeSocket` 探活 |
| `src/index.ts` | `apply`：懒连接生命周期 + DAP 事件注入 + 25 个工具注册 + `tools/result` 写后重载 + `/neovim` 命令 |

## 与源插件（OpenCode `neovim-api`）的映射

| OpenCode | DSH |
|---|---|
| `Plugin = async ({ client })` | `apply(ctx, config)`（bundle 行 config） |
| 加载时 `tryConnectNvim()` 失败即 `throw` | 懒连接：挂载只做后台探测，工具调用时按需连接；失败抛 `nvim not connected`（工具错误，不拖垮插件） |
| `client.app.log({ service:'neovim', ... })` | `ctx.logger('neovim')` |
| `client.session.promptAsync(...)` × `dap_sessions` | `agent.inject({ content, source:{kind:'plugin',plugin:'dsh-neovim'} })` × `dapSessions: Set<sessionId>`，经 `ctx.agents.list()` 匹配在线 agent；agent 已销毁时清退注册项 |
| `tool.execute.after`（write） | `ctx.on('tools/result', ...)`：观察 `write`/`edit` 的 `path`/`file_path`，fire-and-forget `reload_file`（观察者不修改结果） |
| zod 参数校验 | 手工 JSON Schema（本仓库 dsh-lsp 惯例，`defineTool` 同源契约） |
| bun:test | node:test（`t.after` 关闭连接，进程可自退；无 nvim 时干净 skip） |

## 关键决策

1. **连接自愈而非加载即败**。DSH 宿主长驻，Neovim 的启停与宿主无关；OpenCode 的「加载失败即 throw」会让 bundle 行永久 FAILED。改为：工具调用前 `ensureNvim()` 按需连接（探测 → attach → `dap_subscribe` → 订阅三事件），`disconnect` 事件置位后下次调用自动重连，重连后 channel id 变化、事件订阅在**新实例**上重建。

2. **探活先行（`probeSocket`）**。`neovim` 包不给 transport socket 挂 `error` 监听：直连死地址会 uncaught `ENOENT`/`ECONNREFUSED` 且 `channelId()` 永远 pending。attach 前用 `net.createConnection` 探测（5s 超时），成功即销毁探测连接再正式 attach；`Neovim` 构造内另对底层 reader/writer 补挂 error 监听，兜住连接建立后的中途重置。

3. **DAP 事件只回注「用过 DAP 的会话」**。保留源插件 `dap_sessions` 语义：仅记录执行过 `nvim_dap_*` 工具的 session id；事件广播时用 `ctx.agents.list()` 找在线 agent 逐个 `inject()`。注入不是唤醒（空闲 agent 保持空闲），与 harness 的 inject 语义一致；不采用 `session.append('user/message')` 裸写日志路径（绕过 agent 队列与来源归属）。

4. **`luaAsyncEval` 挂起治理**。源实现中 `commandOutput` 失败时 Promise 永不 settle；DSH 版把失败路由到 task 的 reject，并在 `disconnect` 时结算全部 pending task，避免工具调用永久挂起。

5. **工具面 1:1 保留**。25 个工具名、参数、返回文案与源插件一致（模型习惯/提示词即行为）；仅 `localtion` 拼写修正为 `location`。`output.schema` 均为 `{ type:'string' }`，canonical value 即 markdown 文本，Code Mode 无需解析散文取 id。

6. **零 harness 包依赖**。仅运行时依赖 `neovim`；harness 面全部结构契约（仿 dsh-hindsight / dsh-lsp），cordis 无 peerDependency 身份问题。

## 生命周期

- `apply` → `resolveConfig` → 注册 25 工具 + `/neovim` 命令 + `tools/result` 观察者（均为注册即 effect，卸载自动清理）→ 后台 `ensureNvim()` 探测（失败仅日志）
- 工具调用 → `ensureNvim()`（无连接则 connect：probe → attach → `dap_subscribe` → subscribe `dap_pause`/`event_terminated`/`event_exited`）→ RPC/luaAsyncEval → 返回字符串
- nvim 断开 → `Neovim._disconnected = true` + pending task 全 reject → 下次调用重连重建订阅
- 宿主卸载 → 无资源需主动回收（socket 随 client 断开；neovim 包无优雅关闭句柄，`/neovim reconnect` 强制换新）

## 测试

- `test/neovim.test.ts`：自包含冒烟——`test.before` 里 spawn `nvim --embed --clean`（stdio 管道，`attach({ proc })`，spawn/error 事件竞速防 ENOENT 挂起，`channelId` 带超时），注入最小 `core.agent.run_async` stub（单行 `:lua`），9 个用例共享单例连接；`test.after` 关闭连接 + kill 子进程，进程自退。`nvim` 不在 PATH 时全部 `t.skip`，exit 0
- 全量：`npm run check`（build + test）
