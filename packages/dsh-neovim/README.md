# dsh-neovim

Neovim 桥接插件（DeepSeek Harness）：通过 RPC socket 连接本机 Neovim，提供 vim 命令 / Lua 执行 / Lua 求值 + 完整 nvim-dap 调试工具集，并把调试器事件（暂停 / 终止 / 退出）注入回 agent 会话；agent 写文件后自动通知 Neovim 重载。由 OpenCode `opencode-plugin-neovim-api` 插件改写而来。

## 能力

- **nvim 远程调用**（`nvim_*`）：
  - `nvim_command` — 执行 Vim 命令
  - `nvim_lua_command` — 执行 Lua 语句（无返回值）
  - `nvim_lua_eval` — 求值 Lua 表达式并返回 JSON 结果
- **nvim-dap 调试工具集**（`nvim_dap_*`，经用户 Neovim 配置里的 `core.agent` Lua 桥）：
  - 会话控制：`nvim_dap_start`（深度合并配置覆盖）、`nvim_dap_continue`、`nvim_dap_stop`、`nvim_dap_get_sessions`、`nvim_dap_switch_session`
  - 单步：`nvim_dap_step_into` / `step_over` / `step_out` / `run_to_cursor` / `run_to_location`（协程阻塞等待命中断点后返回停止位置）
  - 状态：`nvim_dap_get_stack` / `nvim_dap_get_threads` / `nvim_dap_switch_thread` / `nvim_dap_eval` / `nvim_dap_add_watch`
  - 断点：`nvim_dap_add_breakpoint` / `toggle_breakpoint` / `remove_breakpoint` / `list_breakpoints` / `clear_breakpoints`（支持条件、命中次数、日志断点）
  - 配置：`nvim_dap_get_configurations`（可按语言筛选）
- **调试事件注入**：订阅 `dap_pause` / `event_terminated` / `event_exited`，格式化为 `<dap-event>` 上下文，`agent.inject()` 给所有使用过 DAP 工具的会话（下一次模型请求可见）
- **写后重载**：`tools/result` 观察 `write` / `edit` 工具，成功后通知 Neovim `reload_file`
- **`/neovim` 命令**：`status` 查看 socket / 连接 / 频道 / 追踪会话数；`reconnect` 强制重连
- **懒连接 + 自愈**：插件加载不阻塞宿主；每次工具调用按需重连，Neovim 重启后自动恢复

## 安装（Windows 本地）

```powershell
cd C:\dotfiles-copy\packages\dsh-neovim
npm install --include=dev
npm run build
```

然后编辑 `~/.dsh/profiles/<profile>/package.json`：

1. `dependencies` 加 `"dsh-neovim": "file:C:/dotfiles-copy/packages/dsh-neovim/"`
2. `dsh.profile.bundles` 数组加 `"dsh-neovim"`

重启 profile 后，会话中出现 `nvim_*` / `nvim_dap_*` 工具与 `/neovim` 命令。

## 配置（cordis.patch.yml 行 config）

| 键 | 缺省 | 说明 |
|---|---|---|
| `socket` | `''`（环境解析） | 显式 RPC 地址；为空时按 `$NVIM_LISTEN_ADDRESS` → `$NVIM` → `\\.\pipe\nvim`（win32）→ `$XDG_RUNTIME_DIR/nvim.$USER` 解析 |
| `luaModule` | `core.agent` | 承载调试桥函数的 Lua 模块（`dap_subscribe` / `dap_*` / `reload_file` / `run_async`） |

## 前置条件

- 本机 Neovim 需以 `--listen <地址>`（或设置 `$NVIM_LISTEN_ADDRESS` / `$NVIM`）启动，例如 `nvim --listen \\.\pipe\nvim`
- Neovim 配置需提供 `core.agent` 模块（原 OpenCode 插件同款 Lua 桥），提供 `dap_subscribe(channel_id)`、`dap_*`、`reload_file(path)`、`run_async(fn, channel_id, task_id)`（后者通过 `async_task_finish` 通知返回结果）

## 测试

```powershell
npm test
```

`test/neovim.test.ts`（node:test）自启动一个 `nvim --embed --clean` 子进程并通过其 stdio（msgpack-rpc）连接，验证 command / eval / luaEval / luaAsyncEval（含错误传播），结束后关闭连接并终止子进程。不依赖任何外部 Neovim 实例或监听地址；`nvim` 不在 PATH 时全部干净跳过（exit 0）。`luaAsyncEval` 依赖的 `core.agent` 模块由测试自行注入最小 `run_async` stub（同步 pcall + `async_task_finish` 通知）。

## 约束

- 连接探活先行（`probeSocket`）：`neovim` 包不给 transport socket 挂 `error` 监听，直连死地址会 uncaught + 永久挂起，因此 attach 前先探测
- DAP 事件仅注入给使用过 DAP 工具的会话（`dapSessions` 注册表）；会话销毁时自动清退
- 工具返回值均为 markdown/纯文本字符串（canonical value = string），表格为原插件格式
