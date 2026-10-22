# dsh-lsp 架构

## 模块

| 文件 | 职责 |
|---|---|
| `src/types.ts` | 内部类型：harness 结构契约（`ctx.tools`/`ctx.commands`/`ctx.subprocess`/`ctx.effect` 的最小结构面）、配置、查询词汇、结果 |
| `src/config.ts` | 配置解析：内置 server 表 + 行配置合并、扩展名独占校验、禁用（`enabled:false`）、默认值（含 `symbolFallback*` 兜底开关） |
| `src/progress.ts` | 纯函数：`$/progress` 参数解析、`IndexProgress` 折叠（begin/report/end、`a/b` 计数与百分比、clamp）、进度渲染、索引完成注入消息构造 |
| `src/candidates.ts` | `workspaceSymbol` 空结果兜底：从查询串派生符号名提示，`rg` 先按文件名再按内容收敛候选，按文件名相似度/头文件/路径深度排序并 cap（全路径 fail-open） |
| `src/root.ts` | LSP 根目录查找：从起始文件向上找 root marker → `.git`/`.hg` 兜底 → 起始目录；`RootResolver` 缓存 |
| `src/protocol.ts` | 协议映射：`file:` URI ↔ 路径、1-based↔0-based UTF-16、location 投影（根内相对路径 / 根外绝对路径）、符号/位置渲染、诊断条目映射 |
| `src/client.ts` | `LspClient`：一个 server 子进程 + 一条 JSON-RPC 连接，initialize/shutdown 生命周期、注册 `publishDiagnostics` 通知并交给 inbox、capabilities 留存（pull 判定）、`ensureOpen`（didOpen 惰性打开文档）、`refreshDocument`（全量 didChange 同步磁盘内容）、各查询方法、`waitForDiagnostics`/`cachedDiagnostics`/`onPublish`；声明 `window.workDoneProgress`、应答 `window/workDoneProgress/create`、订阅 `$/progress` 折叠为 `IndexProgress` 并扇出给监听者；`warmDocuments` 并发预热一批文件 |
| `src/registry.ts` | `ServerRegistry`：扩展名→server 独占路由、每 server 单实例缓存、`inFlight` 去重启动（并发调用共享一个子进程，且调用方的 abort 不会杀掉共享 server）、start/stop/restart、status、`onPublishedDiagnostics` 转发、`openFile`/`refreshDocument`/`diagnosticsFor`（按 push/pull/off 模式分派）、`diagnosticsMode`；`onIndexProgress`/`indexProgress` 进度转发（监听在 client 创建时就挂上，避免建完即结束的进度丢失） |
| `src/inbox.ts` | `DiagnosticInbox`：推送式诊断的唯一落点——版本闸门（陈旧推送整条丢弃）、等待者（超时/取消结算为 `null`）、最近一次推送缓存、订阅分发 |
| `src/watch.ts` | `DiagnosticWatcher`：写入→推送→注入的全部语义（登记先于刷新、空集不注入、去重窗口、同 agent 覆盖、超时仅丢弃监听） |
| `src/diagnostics.ts` | 纯函数：严重级别过滤、诊断格式化、`agent.inject` 消息构造 |
| `src/index.ts` | `apply`：注册 `lsp` 工具 + `/lsp` 命令 + 自动启动（挂载补漏 + `agent/created`）+ 推送订阅 + `tools/post-execute` 文件访问联动 + 后台索引完成通知注入 + `workspaceSymbol` 空结果候选兜底（`convergeSymbols`）+ 生命周期 disposer |

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

8. **自动启动只负责「进程 + 握手」，不负责索引**。`agent/created` 取会话 cwd 起进程（串行监听器：异常会被吞掉，绝不拖垮 agent 创建），挂载时再用 `ctx.agents.list()` 对已存在会话补漏（覆盖 HMR 后不会再有 agent/created 的情况）；`findRoot` 之后必须 `hasRootMarker`，否则只有 `.git` 兜底的目录也会白起进程。启动路径不 await、失败只落 debug 日志；`registry` 的 `inFlight` 让自动启动、读钩子与查询共享同一次 spawn。

9. **诊断走推送，超时只结束等待**。clangd 的 `textDocument/diagnostic` 回 `method not found`，所以主路径是 `textDocument/publishDiagnostics`：client 在 `connection.listen()` 后注册通知 → 版本闸门（低于本端最后发送版本者整条丢弃）→ `DiagnosticInbox` → `registry` 转发 → `DiagnosticWatcher` 在「有写入登记 + 达到 `diagnosticsMinSeverity` + 非重复」时 `agent.inject`（不唤醒 driver）。等待由 `diagnosticsTimeoutMs` 兜底，到期只结算为 `null`（`lsp diagnostics` 回退用最近一次推送）：**任何超时/取消都不 stop server、不 didClose**。`diagnosticsMode: auto` 仅在 server 声明 `diagnosticProvider` 时改走拉取路径。

10. **后台索引进度是客户端唯一能观测的就绪信号**。clangd 只在 client 声明 `window.workDoneProgress` 时才发 `$/progress`（token `backgroundIndexProgress`），且可能先发 `window/workDoneProgress/create` 请求等 null 应答；未声明则状态永久停在 Unsupported，一条通知都不发。插件因此：声明能力 → 应答 create → 折叠 `$/progress`（`Completed == Enqueued` 即完成）→ 完成时按 root 找该目录下的 agent，注入一条 `<dsh-lsp-index>`（每 `server+root` 只注入一次，去重表防重复）。这是「索引现在可以查询了」的唯一提示，因为 `workspace/symbol` 本身从不等待索引（`FindSymbols.cpp` 里 `if (!Index) return {}`）。

11. **`workspaceSymbol` 空结果时有候选兜底，而不是靠等索引**。clangd 的 `workspace/symbol` 只查 Dex 索引（dynamic + background），冷启动时就是空；`--background-index` 默认低优先级线程，机器忙时可能长时间不调度。插件在「结果为空 + query 非空 + `symbolFallback`」时：从符号名派生提示（`KAlgFinancial::PMT` → 各级片段）→ `rg --files` 命中文件名的候选 → `rg --files-with-matches` 命内容的候选 → 排序 cap 到 `symbolFallbackMaxCandidates` → `client.warmDocuments`（逐个 `documentSymbol`，`didOpen` + 强制定 AST 解析填充 dynamic index）→ 重查。整体预算 `symbolFallbackTimeoutMs × 3`，任一步失败一律 fail-open 返回空。结果里带 `fallback` 说明与 `indexing` 提示，便于模型判断是「真没有」还是「索引还没好」。

12. **不给 clangd 传 `--compile-commands-dir`**。该 flag 把数据库钉死在进程 cwd，而 registry 按 `spec.id` 缓存单实例（一个 clangd 服务所有 root）——于是只有首个 root 的 TU 能被索引，其它工程即便有 `compile_commands.json` 也拿到 foreign/stale 库，症状正是那些树 `workspaceSymbol` 恒为 `No symbols.`。clangd 默认行为（从每个源文件向上找最近的 `compile_commands.json`）天然是 per-file 正确的，多工程共用一进程也能各自解析到自己的数据库。

## 生命周期

- `apply` → 注册工具/命令 + 自动启动 + 推送订阅 + `tools/post-execute` 监听 → `ctx.effect` 返回 disposer
- 查询 → `registry.resolve` → 懒 `client.start`（spawn + initialize）→ `ensureOpen` → request
- 读文件 → `tools/post-execute` → 热服务器同步 `didOpen` / 冷服务器后台预热
- 写文件 → `tools/post-execute` → push 模式：`watcher.arm` 后 `refreshDocument` → server 推送 → 过滤/去重 → `Agent.inject`（pull-capable server 保持原请求式路径；同一 agent 的新写入覆盖旧监听/旧运行）
- 索引建完 → client `$/progress` → `foldIndexProgress` → registry 转发 → `index.ts` 订阅按 root 找 agent（`relative()` 判前缀）→ `Agent.inject`，同一 `server+root` 只注入一次
- 卸载/HMR → `ctx.effect` disposer → 退订推送 + `watcher.abortAll` + abort 在途诊断 + `registry.stopAll` → 每 client `shutdown`/`exit`/`terminate`（树级，同步回收）
- 进程意外退出 → client 标记 `failed`，下次查询自动重建

## 测试

- 纯函数单测（`node --test`，74 项全绿；本机沙箱禁 `spawn` 时改跑 `npx tsc -p tsconfig.test.json` + `node --test --test-isolation=none <build/test/*.test.js>` 同进程执行）：config 合并/冲突/禁用/枚举校验（含 clangd 参数不含 `--compile-commands-dir`、含 `--background-index`）、URI 往返、UTF-16 位置转换、根目录上溯与 `hasRootMarker`（临时目录 + 各 marker 组合）、诊断过滤/格式化/注入消息、`DiagnosticInbox`（投递/超时/取消/版本闸门/缓存/订阅/clear）、`DiagnosticWatcher`（注入/空集/严重级/超时/abort/覆盖/去重/截断/非本文件/未登记推送/跨 agent）、`ServerRegistry` 启动去重、`progress`（token 解析 / begin 重置 / report 计数与百分比 / clamp / end 完成 / 外来 token / 非法参数 / 渲染 / 注入文案）
- 真实 LSP 端到端（临时工程 + 本机 clangd 17.0.6，脚本 `%TEMP%\dsh-lsp-e2e\e2e.mjs`）：挂载即自动启动（`/lsp status` 显示 running + pid，仅 rooted server 起）→ 模拟 AI 写坏文件 → 500ms 内收到注入的 `<dsh-lsp-diagnostics>`（真实 `Use of undeclared identifier`）→ 再次写入后 server 仍为同一 pid（超时/写入不会中止 server）。已通过。
- 候选兜底端到端（临时工程 + clangd 17.0.6）：目标文件仅用 shell 写入、从未被 `didOpen`；`workspaceSymbol{query:"HiddenWidget"}` 首次索引 miss → `fallback: {used:true, strategy:"content", candidates:4, warmed:4, matched:4}` 并命中 `HiddenOwner.h:2:7`；同一次调用也验证了去掉 `--compile-commands-dir` 后同一 clangd 实例能按文件解析到该临时工程自己的 `compile_commands.json`。已通过。
- 索引完成通知：真实 lua-language-server 后台索引建完后，本会话收到注入 `<dsh-lsp-index> 后台索引已完成 (lua-language-server, root=C:/dotfiles-copy)`。已通过。
