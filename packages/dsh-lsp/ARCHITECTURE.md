# dsh-lsp 架构

## 模块

| 文件 | 职责 |
|---|---|
| `src/types.ts` | 内部类型：harness 结构契约（`ctx.tools`/`ctx.commands`/`ctx.subprocess`/`ctx.effect` 的最小结构面）、配置、查询词汇、结果 |
| `src/config.ts` | 配置解析：内置 server 表 + 行配置合并、扩展名独占校验、禁用（`enabled:false`）、默认值 |
| `src/root.ts` | LSP 根目录查找：从起始文件向上找 root marker → `.git`/`.hg` 兜底 → 起始目录；`RootResolver` 缓存 |
| `src/protocol.ts` | 协议映射：`file:` URI ↔ 路径、1-based↔0-based UTF-16、location 投影（根内相对路径 / 根外绝对路径）、符号/位置渲染、诊断条目映射 |
| `src/client.ts` | `LspClient`：一个 server 子进程 + 一条 JSON-RPC 连接，initialize/shutdown 生命周期、注册 `publishDiagnostics` 通知并交给 inbox、capabilities 留存（pull 判定）、`ensureOpen`（didOpen 惰性打开文档）、`refreshDocument`（全量 didChange 同步磁盘内容）、各查询方法、`waitForDiagnostics`/`cachedDiagnostics`/`onPublish` |
| `src/registry.ts` | `ServerRegistry`：扩展名→server 独占路由、每 server 单实例缓存、`inFlight` 去重启动（并发调用共享一个子进程，且调用方的 abort 不会杀掉共享 server）、start/stop/restart、status、`onPublishedDiagnostics` 转发、`openFile`/`refreshDocument`/`diagnosticsFor`（按 push/pull/off 模式分派）、`diagnosticsMode` |
| `src/inbox.ts` | `DiagnosticInbox`：推送式诊断的唯一落点——版本闸门（陈旧推送整条丢弃）、等待者（超时/取消结算为 `null`）、最近一次推送缓存、订阅分发 |
| `src/watch.ts` | `DiagnosticWatcher`：写入→推送→注入的全部语义（登记先于刷新、空集不注入、去重窗口、同 agent 覆盖、超时仅丢弃监听） |
| `src/diagnostics.ts` | 纯函数：严重级别过滤、诊断格式化、`agent.inject` 消息构造 |
| `src/index.ts` | `apply`：注册 `lsp` 工具 + `/lsp` 命令 + 自动启动（挂载补漏 + `agent/session-start`）+ 推送订阅 + `tools/post-execute` 文件访问联动 + 生命周期 disposer |

## 关键决策

1. **自包含，不依赖官方 LSP seam**。本机 harness（npm 部署 v0.1.0-rc.7）没有 `ctx.lsp`；`@deepseek-ai/dsh-lsp` 等 scope 包是仿制生态产物（非官方），且版本链不匹配。插件只依赖 `vscode-languageserver-protocol`（运行时）+ 结构契约消费 harness。

2. **进程管理走 `ctx.subprocess`**。harness 的 `dsh-subprocess-local` 已在 base bundle 激活，提供树级 spawn/terminate（Windows 走 taskkill /T）、凭证清洗环境、collect 模式诊断尾。LSP server 的全部生命周期挂在此 seam 上。

3. **`vscode-jsonrpc` + `vscode-languageserver-protocol`** 复用成熟实现：`createProtocolConnection(handle.stdout, handle.stdin)` 直接对 `ctx.subprocess` 的 pipe 流做 JSON-RPC 编解码，request type 用 protocol 包的 type-safe 常量，省去手写协议。

4. **`didOpen` 是硬前提**。clangd 对未打开文档的 hover/typeDefinition/documentSymbol/diagnostics 返回 `trying to get AST for non-added document`。`ensureOpen` 在首次位置查询前把文件内容推给 server（惰性、常驻）。`didOpen` 同时是**工程索引唯一的激活触发器**：实测（临时工程，clangd 17.0.6）只起进程、compile_commands.json 与磁盘 `.idx` 齐备时，`workspace/symbol` 全程 0 命中、后台索引也不启动；打开 1 个文件后，整个工程（含从未打开的其它文件）的符号立即可查。

5. **单工具 + 单命令**。工具面 = 1（`lsp`），满足「工具面精简、首轮只露核心」铁律；所有能力通过 `operation` 枚举路由，避免一堆独立工具污染工具目录与请求前缀。

6. **根目录查找独立于工作目录**。混合语言工程里 LSP 根 ≠ 会话 cwd；从被查文件向上找 marker（WPS 的 `compile_commands.json` 在 `D:\branch-master\wpsmain`，源文件可能在十几层之下）。

7. **文件访问联动**。监听 `tools/post-execute`（global）：
   - AI 用 `read` 读文件 → 服务器已热时同步 `didOpen`（读结果返回前文件即可查询）；冷服务器后台预热，绝不拖慢读取。
   - AI 用 `write`/`edit` 写文件 → 异步 `refreshDocument`（全量 didChange）+ 拉取诊断，达到 `diagnosticsMinSeverity` 的结果经 `Agent.inject` 排入 agent 下一次 pre-step 上下文（不唤醒 idle driver）。
   - 防过期污染：按 agent 的 session id 记录在途运行（改一个文件可能让其他文件出现报错，跨文件诊断互相牵连），新写入 abort 旧控制器（合并进诊断请求的 CancellationToken），只注入最新结果。
   - 全部 fail-open：任何异常只写日志，绝不影响工具调用本身。

8. **自动启动只负责「进程 + 握手」，不负责索引**。`agent/session-start` 取会话 cwd 起进程，挂载时再用 `ctx.agents.list()` 对已存在会话补漏（覆盖 HMR 后不会再有 session-start 的情况）；`findRoot` 之后必须 `hasRootMarker`，否则只有 `.git` 兜底的目录也会白起进程。启动路径不 await、失败只落 debug 日志；`registry` 的 `inFlight` 让自动启动、读钩子与查询共享同一次 spawn。

9. **诊断走推送，超时只结束等待**。clangd 的 `textDocument/diagnostic` 回 `method not found`，所以主路径是 `textDocument/publishDiagnostics`：client 在 `connection.listen()` 后注册通知 → 版本闸门（低于本端最后发送版本者整条丢弃）→ `DiagnosticInbox` → `registry` 转发 → `DiagnosticWatcher` 在「有写入登记 + 达到 `diagnosticsMinSeverity` + 非重复」时 `agent.inject`（不唤醒 driver）。等待由 `diagnosticsTimeoutMs` 兜底，到期只结算为 `null`（`lsp diagnostics` 回退用最近一次推送）：**任何超时/取消都不 stop server、不 didClose**。`diagnosticsMode: auto` 仅在 server 声明 `diagnosticProvider` 时改走拉取路径。

## 生命周期

- `apply` → 注册工具/命令 + 自动启动 + 推送订阅 + `tools/post-execute` 监听 → `ctx.effect` 返回 disposer
- 查询 → `registry.resolve` → 懒 `client.start`（spawn + initialize）→ `ensureOpen` → request
- 读文件 → `tools/post-execute` → 热服务器同步 `didOpen` / 冷服务器后台预热
- 写文件 → `tools/post-execute` → push 模式：`watcher.arm` 后 `refreshDocument` → server 推送 → 过滤/去重 → `Agent.inject`（pull-capable server 保持原请求式路径；同一 agent 的新写入覆盖旧监听/旧运行）
- 卸载/HMR → `ctx.effect` disposer → 退订推送 + `watcher.abortAll` + abort 在途诊断 + `registry.stopAll` → 每 client `shutdown`/`exit`/`terminate`（树级，同步回收）
- 进程意外退出 → client 标记 `failed`，下次查询自动重建

## 测试

- 纯函数单测（`node --test`，60 项全绿）：config 合并/冲突/禁用/枚举校验、URI 往返、UTF-16 位置转换、根目录上溯与 `hasRootMarker`（临时目录 + 各 marker 组合）、诊断过滤/格式化/注入消息、`DiagnosticInbox`（投递/超时/取消/版本闸门/缓存/订阅/clear）、`DiagnosticWatcher`（注入/空集/严重级/超时/abort/覆盖/去重/截断/非本文件/未登记推送/跨 agent）、`ServerRegistry` 启动去重
- 真实 LSP 端到端（临时工程 + 本机 clangd 17.0.6，脚本 `%TEMP%\dsh-lsp-e2e\e2e.mjs`）：挂载即自动启动（`/lsp status` 显示 running + pid，仅 rooted server 起）→ 模拟 AI 写坏文件 → 500ms 内收到注入的 `<dsh-lsp-diagnostics>`（真实 `Use of undeclared identifier`）→ 再次写入后 server 仍为同一 pid（超时/写入不会中止 server）。已通过。
