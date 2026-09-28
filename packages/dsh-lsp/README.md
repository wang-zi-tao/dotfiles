# dsh-lsp

LSP 语义代码导航插件（DeepSeek Harness）：一个 `lsp` 工具 + 一个 `/lsp` 命令，通过受管语言服务器子进程提供定义 / 引用 / 实现 / 类型 / 悬停 / 全局符号 / 文档符号 / 诊断，以及聚合式 `explore` 查询。

## 能力

- **单工具 `lsp`**，`operation` 枚举：
  - `explore` — 一次调用返回一个符号的定义 + 类型定义 + 悬停文档 + 实现 + 引用（对齐 codegraph 的 `codegraph_node` 效果）
  - `goToDefinition` / `goToImplementation` / `typeDefinition` / `findReferences` / `hover`
  - `workspaceSymbol` — 全局符号查找，支持只给符号名一部分（`query`，clangd/rust-analyzer 原生模糊匹配）
  - `documentSymbol` — 当前文件大纲
  - `diagnostics` — 诊断：以服务器推送（`textDocument/publishDiagnostics`）为主，只有声明了 pull 能力的 server 才走 LSP 3.17 `textDocument/diagnostic`（clangd 会回 `method not found`）
- **`/lsp` 命令**：`status` / `start [id...]` / `stop [id...]` / `restart`
- **文件访问联动**：AI 用 `read` 读文件时，LSP 同步把该文件 didOpen（服务器已热时阻塞到加载完成，冷启动则在后台打开，不拖慢读取）；AI 用 `write`/`edit` 写文件后，先登记推送监听再刷新服务器副本，服务器推送的诊断达到严重级别时通过 `agent.inject` 注入 agent 的下一次 pre-step 上下文
- **自动启动**：会话启动（以及挂载时对已存在会话的补漏）即为该 cwd 下有 root marker 的 server 起进程，首个查询/首次读文件不再付 spawn+initialize 延迟；`/lsp status` 立即可见失败原因。注意：自动启动**不等于索引就绪**——clangd 只在首次 didOpen 时激活工程索引（读文件钩子天然完成这一步）
- **防过期污染**：推送版本低于本端最后发送版本的诊断整条丢弃（不入缓存、不转发、不注入）；同一 agent 的新写入取消上一轮在途的监听/拉取；相同结论在去重窗口内只注入一次
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

重启 web profile 后，会话中出现 `lsp` 工具与 `/lsp` 命令。

## 配置

行 `config` 里可覆盖 `servers`（内置表为缺省）、`autoStart`（`mount` / `session` / `off`，默认 `session`）、`autoStartServers`（参与自动启动的 id，空 = 全部 enabled）、`autoStartRoots`、`maxLocations`、`maxResultChars`、`timeoutMs`、`syncLoadOnRead`（读文件时同步加载到 LSP，默认 `true`）、`diagnosticsOnWrite`（写文件后诊断并注入，默认 `true`）、`diagnosticsMinSeverity`（注入的最低严重级别：`error` / `warning` / `information` / `hint`，默认 `warning`）、`diagnosticsMode`（`auto` / `push` / `pull` / `off`，默认 `auto`）、`diagnosticsTimeoutMs`（等一次推送的上限，默认 `15000`；**超时只结束等待，绝不中止 server**）、`diagnosticsDedupeMs`（默认 `60000`）、`maxDiagnostics`（单次注入上限，默认 `50`）、`diagnosticsOnAnyPublish`（无写入登记时是否也注入，默认 `false`）。`servers` 按 id 覆盖内置项；`enabled: false` 用纯 id 即可禁用某个内置 server；重复扩展名、缺失 command/extensions/languageId、非法 `autoStart`/`diagnosticsMode` 会在加载期抛错（挂载审计会暴露）。

## 约束

- 只读查询：不做 rename / format / code-action（涉及写权限与预览，另立工具）
- 位置为 1-based UTF-16（工具面）↔ 0-based UTF-16（LSP 面），插件内部统一转换
- `findReferences` 恒含声明（无需模型传 flag）
- 一个扩展名独占一个 server；LSP 根目录解析结果按 `server + 起始目录` 缓存，`/lsp restart` 强制重建
- 自动启动只对「cwd 向上能找到该 server root marker」的目录起进程；只有 `.git`/`.hg` 兜底、没有 marker 的目录不起（避免起一个没有工程可索引的进程）
- 诊断超时/取消只结算等待，不 stop server、不 didClose；server 生命周期只由 `/lsp stop|restart` 与插件卸载控制
