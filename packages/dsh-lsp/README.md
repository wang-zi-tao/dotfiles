# dsh-lsp

LSP 语义代码导航插件（DeepSeek Harness）：一组 `lsp_*` 工具 + 一个 `/lsp` 命令，通过受管语言服务器子进程提供定义 / 引用 / 实现 / 类型 / 悬停 / 全局符号 / 文档符号 / 诊断，以及聚合式 `lsp_explore` 查询。

## 能力

- **`lsp_*` 工具目录**（一个工具一个意图，参数只含该意图需要的字段）：
  - `lsp_explore` — 一次调用返回一个符号的定义 + 类型定义 + 悬停文档 + 实现 + 引用（对齐 codegraph 的 `codegraph_node` 效果）
  - `lsp_definition` / `lsp_type_definition` / `lsp_implementation` / `lsp_hover` — 单点查询，需 `filePath` + `line` + `character`（1-based UTF-16）
  - `lsp_references` — 全工作区引用（恒含声明，无需 flag）
  - `lsp_symbols` — `scope: "document"` 取当前文件大纲；`scope: "workspace"` 按 `query` 全局查符号（支持只给符号名一部分，clangd/rust-analyzer 原生模糊匹配）；workspace 命中为空时用 `rg` 收敛候选文件（先按文件名、再按内容）并预热解析后重查，结果附 `fallback`（策略/候选数/解析数/命中数）与 `indexing`（后台索引进度）说明
  - `lsp_diagnostics` — 诊断：以服务器推送（`textDocument/publishDiagnostics`）为主，只有声明了 pull 能力的 server 才走 LSP 3.17 `textDocument/diagnostic`（clangd 会回 `method not found`）；`severity` 按最低级别过滤（`error` / `warning` / `information` / `hint` / `all`，默认 `all`）
  - `lsp_status` — 列出已配置与已启动的 server（只读，不会启动新进程）
- **`/lsp` 命令**：`status` / `start [id...]` / `stop [id...]` / `restart`
- **文件访问联动**：AI 用 `read` 读文件时，LSP 同步把该文件 didOpen（服务器已热时阻塞到加载完成，冷启动则在后台打开，不拖慢读取）；AI 用 `write`/`edit` 写文件后把新内容推给服务器（`didChange`），让重解析与跨文件连带报错尽早发生
- **诊断常驻订阅（不依赖写入）**：每个 server 的每一次 `textDocument/publishDiagnostics` 都被观测，按文件做**多重集差分**，只把新增与消失（已修复）的诊断经 `agent.inject` 注入到该 server root 下的所有 agent 会话；一次重解析产生的多文件突发推送由合并窗口（`diagnosticsFlushMs`）压成一条消息。同一 agent 会话的注入排队到下一次 pre-step，不唤醒 idle driver
- **自动启动**：会话启动（以及挂载时对已存在会话的补漏）即为该 cwd 下有 root marker 的 server 起进程，首个查询/首次读文件不再付 spawn+initialize 延迟；`/lsp status` 立即可见失败原因。注意：自动启动**不等于索引就绪**——clangd 只在首次 didOpen 时激活工程索引（读文件钩子天然完成这一步）
- **后台索引进度 → 通知模型**：client 声明 `window.workDoneProgress` 并应答 `window/workDoneProgress/create`，订阅 `$/progress`（clangd 的 `backgroundIndexProgress`）；索引建完（`Completed == Enqueued`）时把一条 `<dsh-lsp-index>` 通知注入该 root 下的 agent——此前返回 `No symbols.` 的查询此时可以重试。`/lsp status` 同时显示当前进度
- **防过期污染**：推送版本低于本端最后发送版本的诊断整条丢弃（不入缓存、不转发、不注入）；同一 agent 的新写入取消上一轮在途的拉取；注入是差分式的——结论没变就不注入（没有变化就没有消息，因此不会因为重复推送产生噪音）
- **懒启动兜底 + 手动覆盖**：`autoStart: off` 时首次查询仍按需 spawn；`/lsp` 可显式 start/stop/restart
- **混合语言工程**：按文件扩展名独占路由到不同 server；LSP 根目录自动从被查文件向上查找，不依赖工作目录

## 内置语言服务器

| id | 语言 | 扩展名 | root marker |
|---|---|---|---|
| `clangd` | C/C++ | c cc cpp cxx c++ h hpp hh hxx h++ inl | compile_commands.json / compile_flags.txt / .clangd / wps_3rdparty_list.cmake |
| `rust-analyzer` | Rust | rs | Cargo.toml / Cargo.lock / rust-project.json |
| `gopls` | Go | go | go.mod / go.work |
| `pyright` | Python | py pyi | pyproject.toml / setup.py / requirements.txt / .python-version |
| `typescript-language-server` | TS/JS | ts tsx js jsx mjs cjs | package.json / tsconfig.json / jsconfig.json |
| `lua-language-server` | Lua | lua | .luarc.json / .luarc.jsonc / .stylua.toml |

## 安装（Windows 本地）

```powershell
cd C:\dotfiles-copy\packages\dsh-lsp
npm install
npm run build
```

然后编辑 `~/.dsh/profiles/web/package.json`：

1. `dependencies` 加 `"dsh-lsp": "file:C:/dotfiles-copy/packages/dsh-lsp/"`
2. `dsh.profile.bundles` 数组加 `"dsh-lsp"`

重启 web profile 后，会话中出现 `lsp_*` 工具与 `/lsp` 命令。

## 配置

行 `config` 里可覆盖 `servers`（内置表为缺省）、`autoStart`（`mount` / `session` / `off`，默认 `session`）、`autoStartServers`（参与自动启动的 id，空 = 全部 enabled）、`autoStartRoots`、`maxLocations`、`maxResultChars`、`timeoutMs`（单次 LSP 请求与 initialize 握手的硬上限，默认 `60000`；超时只取消该请求，不中止 server）、`syncLoadOnRead`（读文件时同步加载到 LSP，默认 `true`）、`diagnosticsInject`（是否把诊断变化注入 agent 上下文，默认 `true`；关掉即只留 `lsp_diagnostics` 的按需诊断）、`diagnosticsMinSeverity`（注入的最低严重级别：`error` / `warning` / `information` / `hint`，默认 `warning`）、`diagnosticsMode`（`auto` / `push` / `pull` / `off`，默认 `auto`）、`diagnosticsTimeoutMs`（等一次推送的上限，默认 `15000`；**超时只结束等待，绝不中止 server**）、`diagnosticsFlushMs`（诊断变更合并窗口，默认 `400`；`0` = 每次推送立即结算）、`maxDiagnostics`（单条注入消息的诊断上限，默认 `50`）、`symbolFallback`（`workspaceSymbol` 空结果时是否收敛候选文件兜底，默认 `true`）、`symbolFallbackMaxCandidates`（最多预热多少个候选，默认 `12`）、`symbolFallbackTimeoutMs`（单次候选搜索预算，默认 `15000`）、`fallbackSearchCommand`（收敛用的搜索命令，默认 `rg`）。`servers` 按 id 覆盖内置项；`enabled: false` 用纯 id 即可禁用某个内置 server；重复扩展名、缺失 command/extensions/languageId、非法 `autoStart`/`diagnosticsMode` 会在加载期抛错（挂载审计会暴露）。

## 约束

- 只读查询：不做 rename / format / code-action（涉及写权限与预览，另立工具）
- 位置为 1-based UTF-16（工具面）↔ 0-based UTF-16（LSP 面），插件内部统一转换
- `lsp_references` 恒含声明（无需模型传 flag）
- 一个扩展名独占一个 server；LSP 根目录解析结果按 `server + 起始目录` 缓存，`/lsp restart` 强制重建
- 内置 clangd 参数为 `--background-index --clang-tidy --log=error`，**不带 `--compile-commands-dir`**：clangd 默认从每个源文件向上找最近的 `compile_commands.json`，同一进程因此能服务多个工程；钉死该 flag 会让所有 TU 用首个 root 的数据库，其它工程的符号永远进不了索引
- 自动启动只对「cwd 向上能找到该 server root marker」的目录起进程；只有 `.git`/`.hg` 兜底、没有 marker 的目录不起（避免起一个没有工程可索引的进程）
- 诊断超时/取消只结算等待，不 stop server、不 didClose；server 生命周期只由 `/lsp stop|restart` 与插件卸载控制
- 任何请求都不会无限等待：pull 诊断请求与 initialize 握手受 `timeoutMs` 硬约束，push 等待受 `diagnosticsTimeoutMs` 约束；超时后用最近一次推送兜底并在 `lsp_diagnostics` 结果里注明原因（`note`）
- 取消永不写入已关闭的连接：`$/cancelRequest` 在 `Connection is closed` 的连接上会抛 `ConnectionError`，而该异常由信号监听器抛出、会被 Node 当作 `uncaughtException` 直接打死宿主进程（热更新时复现过）。因此 token 的取消监听先判活（`connection != null && state` 为 `running`/`starting`），再 `try/catch` 包住监听器调用；连接已关闭时丢弃这次取消
- 卸载/重启有界：`shutdown`/`exit` 握手受 `SHUTDOWN_TIMEOUT_MS`（1000ms）约束，server 不答也会照常 dispose + 终止进程树，不会卡住插件卸载
