# dsh-tui TUI 扩展点 — 现有扩展枚举报告（只读考察）

> 考察对象：`@deepseek-harness-tui/dsh-tui` v0.10.0-beta.4（基于移植 Ink 的 DSH 交互式 TUI 前端）
> 考察范围：profile 根 `C:\Users\wps\.dsh\profiles\dsh-tui`、已装包 `dsh-working-activity@0.4.0`、dsh-tui 包内 `presets/`、`~/.dsh` 其他 profile 的 patch 层、`~/.dsh/.agent-presets` 用户根
> 性质：**只读考察，未改任何代码**

---

## 0. 结论速览

1. dsh-tui 的插件扩展面分两层：**UI 接缝服务**（`ctx.tuiPrompt`/`tuiStatus`/`tuiDialogs`/`tuiShortcuts`/`tuiRenderers`/`tuiToast`/`tuiThemes`，由 `dsh-tui-extensions` 行挂载）与 **agent 平面扩展**（`dsh-tui-agent-presets` 预设名册、`dsh-tui-scenes`/`workspaces`/`command-trees`/`settings-sections`、decision 事件）。
2. 唯一「真正以独立第三方包身份接入 TUI UI 接缝」的现有扩展是 **`dsh-working-activity@0.4.0`**——它挂在 **prompt 区**（`ctx.tuiPrompt` 的 `${activity}` 模板值），**不是** 状态行 slot（`ctx.tuiStatus`）。且它已被 dsh-tui 吸收为直接依赖并以 `@deepseek-harness-tui/dsh-tui/working-activity` 子路径挂载（issue #60 解析锚点、#153 publish 关闭）。
3. 预设类扩展是第二大类：dsh-tui **包内自带** `presets/liangshen`（anchored-standard）；用户级 `~/.dsh/.agent-presets/` 下另有 liangshen（order 4）、router-spec、router-standard 三个预设；真实用户 profile（`tui`/`web`）还装了独立插件 **`@linxin666/dsh-liangshen@0.1.20`** 把同一类两阶段锚定预设做成可安装 bundle。
4. 真实运行 profile 是 `tui` 与 `web`（17 个 bundle 全量组合）；`dsh-tui` profile 是仅含 `@deepseek-harness-tui/dsh-tui` 的最小骨架，从未真正 boot（`node_modules/@deepseek-ai` 不存在）。

---

## 1. 扩展点清单（dsh-tui 暴露的接入面）

### 1.1 UI 接缝服务（`dsh-tui-extensions` 插件行挂载，`lib/types/extensions.d.ts` + `lib/types/dsh-adapter/extensions.d.ts` 导出）

| 扩展点 | 服务句柄 | 能力 | 消费方 |
|---|---|---|---|
| 提示符模板值 | `ctx.tuiPrompt`（`register(name, initial?) → {set, dispose}`） | 注册 `${name}` 模板值供 `theme.leftPrompt` 渲染；未注册值被模板渲染器省略 | TUI 主题渲染器（用户把 `${activity}` 等写进 leftPrompt） |
| 状态行贡献 | `ctx.tuiStatus` / `TuiStatusStore`（`TuiStatusEntry`） | keyed 状态行条目 | TUI 状态行 |
| 托管对话框 | `ctx.tuiDialogs` / `TuiDialogStore` | 受管 select/confirm/input 对话框 | channel/Chat |
| 键盘快捷键 | `ctx.tuiShortcuts`（`matchShortcut`, `parseShortcutCombo`, `TuiShortcutKey/Options`） | 快捷键注册表 | TUI 键盘层 |
| 自定义会话条目渲染器 | `ctx.tuiRenderers`（`TuiEntryRenderer/TuiEntryRenderResult`） | 会话条目自定义文本渲染 | 会话列表 |
| Toast | `ctx.tuiToast`（`TuiToastDelivery/Options/Sink`） | 瞬时通知 | 任意插件 |
| 运行时主题 | `ctx.tuiThemes`（`TuiThemeBase/Descriptor/Registration`） | 主题声明（host 可读） | 主题系统 |

决策事件（不经服务，由 channel 直接发 cordis bus）：`tui/input`、`tui/rewind-prompt`、`tui/session-switch`、`tui/compact`（含 `TuiInputEvent`/`TuiRewindPromptEvent`/`TuiSessionSwitchEvent`/`TuiCompactEvent`/`Decision` 类型、`DECISION_EVENT_PERMISSIONS`、`DECISION_HANDLER_TIMEOUT_MS`/`DECISION_TOTAL_TIMEOUT_MS`）。**默认 deny**：需 `~/.dsh-tui/extension-grants.json` 显式 grant。消费方 `channel.ts`/`Chat.tsx` 用 `ctx.get` 软读。

### 1.2 插件互操作与组合平面（dsh-tui 包内 `cordis.yml`/`cordis.patch.yml` 的行）

| 行 id | 作用 |
|---|---|
| `dsh-tui-plugin-host` | 插件互操作锚点：统一 grant store、runtime generation id、Host Descriptor |
| `dsh-tui-extensions` | 上述六个 UI 服务 + decision 事件通道（issue #183：服务只在 entry-level inject 保序，插件代码级不 inject，防死锁） |
| `dsh-tui-scenes` | 全屏场景注册表 |
| `dsh-tui-workspaces` | 工作区注册表 |
| `dsh-tui-command-trees` | 斜杠命令补全树 |
| `dsh-tui-settings-sections` | 设置区块声明 |
| `dsh-tui-agent-presets` | 挂 `@deepseek-ai/dsh-agent-presets`：预设名册 = 官方包 presets 目录（`{default:'standard'}`）+ 用户根 `~/.dsh/.agent-presets/`（roots trust system）；官方行在场 self-disable |
| `dsh-tui-auth`（= `@deepseek-harness-tui/dsh-tui/oauth`，inject `[llm, commands]`） | OAuth 登录流 |
| `dsh-tui-storage/-storage-json/-storage-domain/-workspace` | 持久化栈（scoped id；官方同名已启用行在场时以 JS 表达式 self-disable） |
| `dsh-tui-code-runtime` | PTC 呈现等待的 Host 服务（code-runtime worker-thread） |
| `dsh-tui-cordis-host-runner` | cordis 预设实验工具 |
| `dsh-tui-subagent-model-selection-settings` | 子 agent 模型选择设置区块 |
| 主行 `dsh-tui` | config：`provider`/`model`/`effort`/`fullscreen`/`modes`、`sessionId`（`DSH_TUI_RESUME_SESSION`/`DSH_CC_RESUME_SESSION`）、`workspace`（`DSH_TUI_WORKSPACE_TARGET`）、`preset`（`DSH_TUI_PRESET`）、`inject: [agents, tuiWorkspaces, tuiScenes, tuiDialogs, tuiStatus, tuiShortcuts, tuiRenderers, tuiThemes]` |

### 1.3 配置型扩展面（profile patch 层，id 定向覆盖）

profile `cordis.patch.yml` 可用 `- id: <行id>` 覆盖任意上述行的 config/name/disabled，或用 `- insert:` 注入新行——这是 `tui`/`web` profile 实际使用的方式。

---

## 2. 现有扩展清单

### 2.1 随 dsh-tui 内置（bundle 自带）

1. **`working-activity`（挂载 `@deepseek-harness-tui/dsh-tui/working-activity` 子路径）** —— 见 §2.2 的实现说明；挂载点与两处 issue 决策如下：
   - issue #60：dsh Loader 从 profile 目录解析行名，pnpm isolated 布局不会把传递依赖链进 profile `node_modules`，裸名 `dsh-working-activity` 会 `ERR_MODULE_NOT_FOUND` 崩整个 app → 走 dsh-tui 自身 subpath 保解析锚点。
   - issue #153：本地 `apply` 遮蔽星号重导出，强制 `publish` 关（防 ≤0.6.x 旧 launcher patch `publish:true` 污染共享 JSONL）。
   - config：`publish: false, publishIntervalMs: 500`；TUI 进程内自算 working line，从不持久化 activity/status（保 Web 可读）。
2. **`presets/liangshen`（打包预设）**：`.dsh-tui-managed.json`（owner `@deepseek-harness-tui/dsh-tui`，preset `liangshen`，revision `liangshen-toolcall-full-catalog-subagents-durable-hint-v5`）+ `agent.cordis.yml`（391 行 anchored-standard：`tool-bootstrap` `bootstrapTools:[bash, str_replace_editor]`，首工具调用后开放完整目录，压缩后重新锚定；persona 一行 `You are a helpful software engineer assistant.`；isolate realm 组：persistent-shell(win32 disabled)/custom-bash.mjs(win32 only)/str-replace-editor/tool-fs/tool-fs-search/tool-jobs/skill-filesystem/tool-skill/tool-goal/planning 组/compaction 组/delegation 组/workflow/tool-ask-user/tool-todo/tool-web）+ `preset.yml`（name 梁神模式，order 5，description「主 Agent 与子 Agent 首轮均保持 Minimal 双工具，首次工具调用后开放完整目录，压缩后重新锚定。」）+ `custom-bash.mjs`/`compaction-epoch.mjs`/`instruction-hint.mjs`/`skill-search.mjs`/`tool-bootstrap.mjs`。
3. 主行内置的 `agent-spine-demo`（persona 读 `DSH_TUI_PERSONA`）、`sessions`（`DSH_TUI_SESSION_ROOT` 或 `USERPROFILE/.dsh-tui/sessions`）、`token-meter`、`compact`、`dsh-tui-command-trees` 等基础设施行（§1.2）。

### 2.2 第三方独立包扩展

**① `dsh-working-activity@0.4.0`（npm 独立包，但已被 dsh-tui 收为直接依赖 `^0.4.0`）**

- 包面：`exports` 提供 `.`(lib/types/index.js)、`./client`、`./config`、`./events`、`./frames`、`./invariant`、`./status`、`./cordis.patch.yml`、`./src/*`；`dsh.bundle.patch=./cordis.patch.yml`（仅一条 insert：`id: working-activity, name: 'dsh-working-activity'`）；`dsh.client.platform=web`、`dsh.client.inject=[@deepseek-ai/dsh-client-runtime, @deepseek-ai/dsh-client-ui-conversation, @deepseek-ai/dsh-client-ui-slots]`。
- **TUI 侧接入点 = prompt 区 `ctx.tuiPrompt`**：`apply(ctx)` 内 `const prompt = ctx.get('tuiPrompt', false)` → `prompt?.register('activity', undefined)` 得 `promptHandle` → `publish()` 里 `promptHandle?.set(line)`。无该 slot 时插件在 TUI 中惰性（渲染器省略未注册值）。用户需把 `${activity}` 加进 `theme.leftPrompt` 才显示（README 示例：`'${cwd}${git/worktree}${activity}${model}${token_meter/cache_hit_rate}${context}'`）。**不使用状态行 slot（`ctx.tuiStatus`）。**
- 另外两个 sink（非 TUI 状态行）：② session events（log-only `activity`/`status` 事件，经 `src/registration.ts` 注册进每个可达 dsh-session 副本的 `KNOWN_SESSION_EVENT_TYPES`，因 append() 无 ignorable 标志，未知类型会让日志不可恢复）；③ systemPrompt 注入 section `working-activity:narrate`（order 60，⏵ 自述 narration 契约）。
- Web 半边 `src/client/index.ts`：`inject=['slots']`，`ctx.slots.inject('conversation.input.dock', …)` 注册 id `activity` order 15 registrant `dsh-working-activity`，渲染 `WorkingLine.tsx`。
- 状态机：`src/status.ts` `ActivityTracker`（phase idle/waiting/thinking/tool/done），消费 `turn/start` `step/start` `assistant/chunk` `message tool/call` `tool/result` `turn/end` + `agent/status`，render 出 `ActivityState{phase,line,label,detail,phrase,toolCount,turnElapsedMs,phaseStartedAt}`；常量 `PHRASE_ROTATE_MS=4000, RARE_ROTATE_MS=7500, PENDING_MS=6000, COMBO_GAP_MS=10000, STREAM_BUFFER_CHARS=300, NARRATE_GRACE_MS=5000`。
- 配置：`phrases:true, publish:false, tickMs:500, publishIntervalMs:2000, detailLimit:40, narrate:true, lang:auto, frames:DEFAULT_PRESET, mode:lively, showTokPerSec:false, workRemindAt:0`；`src/config.ts` 兼容 pi extension 的 `~/.pi/agent/working-activity.json` 形状。

**② `@linxin666/dsh-liangshen@0.1.20`（独立 bundle，真实 profile `tui`/`web` 已装）**

- 接入面 = **agent-presets 预设名册**（非 TUI UI 接缝）：`dsh.bundle.patch=./cordis.patch.yml` → insert `{id: liangshen, name: '@linxin666/dsh-liangshen'}`；node 半边启动时把捆绑预设同步进 `~/.dsh/.agent-presets/liangshen`。
- 与 dsh-tui 打包 liangshen 同族：两阶段锚定 preset——phase1 仅 Minimal 双工具（persistent bash + str_replace_editor）、无运行时上下文、`anchorGate` 首块 minimal-like 判定、`promoteAfterFirstResponse`、`promotedPresentation: code`（PTC `run_code`）、promotion 后普通注入回归、压缩后回退 bootstrap+compactionTools。
- 说明：`tui` profile bundles 顺序为 `dsh-base → workflow → dsh-liangshen → … → dsh-tui → blue-fantasy skin → task-board → dsh-neovim`，即 dsh-liangshen 在 dsh-tui 之前加载。

**③ 用户级预设 `~/.dsh/.agent-presets/`（被 dsh-tui-agent-presets 名册消费）**

- `liangshen/`：preset.yml（name 梁神模式，order 4，花哨 description）+ NOTICE + custom-bash.mjs + tool-bootstrap.mjs + agent.cordis.yml（与 dsh-tui 打包版同源，order 4 vs 打包版 order 5）
- `router-spec/`、`router-standard/`（preset.yml：name Router Standard (experimental)，description「Task-aware routing — RL-interface restoration…」；均含 agent.cordis.yml + router-bootstrap-v1.mjs/router-bootstrap.mjs/router-core.mjs）

**④ profile patch 层的 TUI 相关接线（`tui`/`web` 相同）**

- `- id: dsh-tui` config：`{preset: code, provider: wpscodingplan, model: deepseek/deepseek-v4-flash-0731, effort: high}`（覆盖主行配置）
- `- id: dsh-tui-code-runtime` name `'@deepseek-ai/dsh-code-runtime-worker-thread'`（把 scoped 行重映射到官方包名，保解析锚点）
- 其余（agent-teams、distill、compaction-acp/billion-context-dsh、dsh-hindsight、skill-filesystem-opencode、7 个 MCP client）为 agent 平面配置，不触碰 TUI UI 接缝。

**⑤ 未接入的 profile**

- `dsh-tui`（最小骨架）：无 cordis.patch.yml，`node_modules/@deepseek-ai` 不存在（Test-Path False）→ 从未真正 boot。
- `cc-tui`：cordis.patch.yml 为空 `[]`。
- `~/.dsh/cordis.patch.yml`（home 级）：仅 dsh-skin managed 8 个 skin 行 disabled，与 TUI 无关。

---

## 3. 审查结论

1. **TUI 扩展点体系成熟但接入者极少**：UI 接缝服务（tuiStatus/tuiDialogs/tuiShortcuts/tuiRenderers/tuiToast/tuiThemes/tuiPrompt + decision 事件）全部就位，但**目前实际使用 `ctx.tuiPrompt` 的只有 dsh-working-activity 一家**；`ctx.tuiStatus`（状态行 slot）尚无任何现有扩展使用。decision 事件默认 deny（需 `~/.dsh-tui/extension-grants.json` grant），尚无已装扩展依赖它。
2. **第三方包接入 TUI 的正确姿势** = `dsh.bundle.patch` 里 insert 一行 + profile bundles 收录（dsh-working-activity 先例）；若包名会被 pnpm 隔离布局拆掉解析锚点，改走宿主（dsh-tui）的 subpath 重导出挂载（issue #60 决策）。
3. **dsh-working-activity 的挂载点是 prompt 区而非状态行**：它通过 `ctx.get('tuiPrompt', false)` 软读，注册 `${activity}`；这与任务预设「prompt 区 or 状态行 slot」的疑问对应——答案是 **prompt 区**（且经 dsh-tui 子路径挂载，config publish:false）。
4. **预设平面（dsh-tui-agent-presets）是目前扩展最活跃的平面**：包内打包 liangshen + 用户根三个预设 + 独立插件 @linxin666/dsh-liangshen，三者并存且同源演进（打包版 revision v5、order 5；用户根 order 4；插件版 0.1.20）。
5. **真实环境 profile 是 `tui`/`web`，不是 `dsh-tui`**：考察扩展时应以 `tui` profile 的 17-bundle 组合为准。

（考察时间戳：以本次会话为准；全部结论基于磁盘文件只读证据。）
