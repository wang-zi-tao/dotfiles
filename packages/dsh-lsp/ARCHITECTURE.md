# dsh-lsp 架构

## 模块

| 文件 | 职责 |
|---|---|
| `src/types.ts` | 内部类型：harness 结构契约（`ctx.tools`/`ctx.commands`/`ctx.subprocess`/`ctx.effect` 的最小结构面）、配置、查询词汇、结果 |
| `src/config.ts` | 配置解析：内置 server 表 + 行配置合并、扩展名独占校验、禁用（`enabled:false`）、默认值 |
| `src/root.ts` | LSP 根目录查找：从起始文件向上找 root marker → `.git`/`.hg` 兜底 → 起始目录；`RootResolver` 缓存 |
| `src/protocol.ts` | 协议映射：`file:` URI ↔ 路径、1-based↔0-based UTF-16、location 投影（根内相对路径 / 根外绝对路径）、符号/位置渲染 |
| `src/client.ts` | `LspClient`：一个 server 子进程 + 一条 JSON-RPC 连接，initialize/shutdown 生命周期、`ensureOpen`（didOpen 惰性打开文档）、各查询方法 |
| `src/registry.ts` | `ServerRegistry`：扩展名→server 独占路由、每 server 单实例缓存、start/stop/restart、status |
| `src/index.ts` | `apply`：注册 `lsp` 工具 + `/lsp` 命令 + 生命周期 disposer |

## 关键决策

1. **自包含，不依赖官方 LSP seam**。本机 harness（npm 部署 v0.1.0-rc.7）没有 `ctx.lsp`；`@deepseek-ai/dsh-lsp` 等 scope 包是仿制生态产物（非官方），且版本链不匹配。插件只依赖 `vscode-languageserver-protocol`（运行时）+ 结构契约消费 harness。

2. **进程管理走 `ctx.subprocess`**。harness 的 `dsh-subprocess-local` 已在 base bundle 激活，提供树级 spawn/terminate（Windows 走 taskkill /T）、凭证清洗环境、collect 模式诊断尾。LSP server 的全部生命周期挂在此 seam 上。

3. **`vscode-jsonrpc` + `vscode-languageserver-protocol`** 复用成熟实现：`createProtocolConnection(handle.stdout, handle.stdin)` 直接对 `ctx.subprocess` 的 pipe 流做 JSON-RPC 编解码，request type 用 protocol 包的 type-safe 常量，省去手写协议。

4. **`didOpen` 是硬前提**。clangd 对未打开文档的 hover/typeDefinition/documentSymbol/diagnostics 返回 `trying to get AST for non-added document`。`ensureOpen` 在首次位置查询前把文件内容推给 server（惰性、常驻）。

5. **单工具 + 单命令**。工具面 = 1（`lsp`），满足「工具面精简、首轮只露核心」铁律；所有能力通过 `operation` 枚举路由，避免一堆独立工具污染工具目录与请求前缀。

6. **根目录查找独立于工作目录**。混合语言工程里 LSP 根 ≠ 会话 cwd；从被查文件向上找 marker（WPS 的 `compile_commands.json` 在 `D:\branch-master\wpsmain`，源文件可能在十几层之下）。

## 生命周期

- `apply` → 注册工具/命令 → `ctx.effect` 返回 disposer
- 查询 → `registry.resolve` → 懒 `client.start`（spawn + initialize）→ `ensureOpen` → request
- 卸载/HMR → `ctx.effect` disposer → `registry.stopAll` → 每 client `shutdown`/`exit`/`terminate`（树级，同步回收）
- 进程意外退出 → client 标记 `failed`，下次查询自动重建

## 测试

- 纯函数单测（`node --test`）：config 合并/冲突/禁用、URI 往返、UTF-16 位置转换、根目录上溯（临时目录 + 各 marker 组合）
- 真实 LSP 冒烟：clangd spawn → initialize → didOpen → documentSymbol → shutdown 全链路（见开发历史，已通过）
