# dsh-lsp

LSP 语义代码导航插件（DeepSeek Harness）：一个 `lsp` 工具 + 一个 `/lsp` 命令，通过受管语言服务器子进程提供定义 / 引用 / 实现 / 类型 / 悬停 / 全局符号 / 文档符号 / 诊断，以及聚合式 `explore` 查询。

## 能力

- **单工具 `lsp`**，`operation` 枚举：
  - `explore` — 一次调用返回一个符号的定义 + 类型定义 + 悬停文档 + 实现 + 引用（对齐 codegraph 的 `codegraph_node` 效果）
  - `goToDefinition` / `goToImplementation` / `typeDefinition` / `findReferences` / `hover`
  - `workspaceSymbol` — 全局符号查找，支持只给符号名一部分（`query`，clangd/rust-analyzer 原生模糊匹配）
  - `documentSymbol` — 当前文件大纲
  - `diagnostics` — 拉取式诊断（LSP 3.17 `textDocument/diagnostic`）
- **`/lsp` 命令**：`status` / `start [id...]` / `stop [id...]` / `restart`
- **懒启动 + 手动覆盖**：首次查询自动 spawn 对应 server；`/lsp` 显式控制
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

行 `config` 里可覆盖 `servers`（内置表为缺省）、`lazyStart`、`maxLocations`、`maxResultChars`、`timeoutMs`。`servers` 按 id 覆盖内置项；`enabled: false` 用纯 id 即可禁用某个内置 server；重复扩展名、缺失 command/extensions/languageId 会在加载期抛错（挂载审计会暴露）。

## 约束

- 只读查询：不做 rename / format / code-action（涉及写权限与预览，另立工具）
- 位置为 1-based UTF-16（工具面）↔ 0-based UTF-16（LSP 面），插件内部统一转换
- `findReferences` 恒含声明（无需模型传 flag）
- 一个扩展名独占一个 server；LSP 根目录解析结果按 `server + 起始目录` 缓存，`/lsp restart` 强制重建
