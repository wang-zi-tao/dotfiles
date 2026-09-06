# dsh-tui v0.10.0-beta.4 TUI 扩展点清单

> 考察人：tui-explorer（任务 t1）｜团队：dsh-tui-extension-survey
> 考察根目录 base = `C:\Users\wps\.dsh\profiles\dsh-tui\node_modules\@deepseek-harness-tui\dsh-tui`
> 本文所有相对路径均以 base 为基准；`.d.ts` 行号即 `lib/types/` 下声明文件的行号。
> 结论先行：dsh-tui 的扩展点分 **5 族**——①cordis Service 注册面（8 个 row / 12 个服务，面向第三方插件）；②决策事件面（6 个 `tui/*` 事件，cordis Events 接口合并 + D-7 权限守门）；③契约/权限面（admission registry + grants 文件）；④配置面（主插件 Config schema + cordis.yml/patch row）；⑤预设面（presets/*/agent.cordis.yml，agent-plane 组合，非 TUI 渲染扩展）。

---

## 0. 总览表

| # | 扩展点 | row / 服务 | 注册机制 | 面向对象 | 状态 |
|---|--------|-----------|----------|----------|------|
| 1 | 插件互操作锚点 | `dsh-tui-plugin-host` → `ctx.tuiPluginHost` | cordis Service（Context 合并） | 第三方插件 | 外部扩展面 |
| 2 | 决策事件 | 6 × `tui/*`（Events 接口合并，无 row） | cordis Events + host 中介 DecisionEvents registry | 第三方插件 | 外部扩展面 |
| 3 | 对话框 | `dsh-tui-extensions` → `ctx.tuiDialogs` | cordis Service | 第三方插件 | 外部扩展面 |
| 4 | 状态行贡献 | 同上 → `ctx.tuiStatus` | cordis Service | 第三方插件 | 外部扩展面 |
| 5 | 键盘快捷键 | 同上 → `ctx.tuiShortcuts` | cordis Service | 第三方插件 | 外部扩展面 |
| 6 | 会话条目渲染器 | 同上 → `ctx.tuiRenderers` | cordis Service | 第三方插件 | 外部扩展面 |
| 7 | Toast 通知 | 同上 → `ctx.tuiToast` | cordis Service | 第三方插件 | 外部扩展面 |
| 8 | 运行时主题 | 同上 → `ctx.tuiThemes` | cordis Service | 第三方插件 | 外部扩展面 |
| 9 | 命令树补全 | `dsh-tui-command-trees` → `ctx.tuiCommandTrees` | cordis Service | 第三方插件 | 外部扩展面 |
| 10 | 设置分区 | `dsh-tui-settings-sections` → `ctx.tuiSettingsSections` | cordis Service | 第三方插件 | 外部扩展面 |
| 11 | 全屏场景 | `dsh-tui-scenes` → `ctx.tuiScenes` | cordis Service | 第三方插件 | 外部扩展面 |
| 12 | 工作区 provider | `dsh-tui-workspaces` → `ctx.tuiWorkspaces` | cordis Service | 第三方插件 | 外部扩展面 |
| 13 | 消息观察 | plugin-host 挂载 → `ctx.tuiMessageObserver` | cordis Service | 第三方插件 | 外部扩展面 |
| 14 | 插件本地存储 | plugin-host 挂载 → `ctx.tuiPluginStorage` | cordis Service | 第三方插件 | 外部扩展面 |
| 15 | 效果台账 | plugin-host 挂载 → `ctx.tuiEffectLedger` | cordis Service | 第三方插件 | 外部扩展面 |
| 16 | 权限/守门 | grants + decision-guard + registry | `~/.dsh-tui/extension-grants.json` + 内嵌 registry | 第三方插件 | 外部扩展面（治理） |
| 17 | 主插件配置 | `dsh-tui` → Config schema | cordis row config | 部署者 | 配置面 |
| 18 | Agent 预设 | `presets/liangshen/`（preset.yml + agent.cordis.yml） | 文件目录 + dsh-agent-presets roster | 预设作者 | 预设面 |
| 19 | 挂载别名 | `./working-activity`、`./oauth` 子路径 | package.json exports re-export | bundle 作者 | 部署面 |
| 20 | API 类型聚合 | `./api`、`./jsx-runtime`、`./test-utils` | package.json exports | 插件开发者 | 开发面 |

---

## 1. package.json 导出面（扩展点的入口契约）

`package.json`（base 根）exports 子入口（均 types→`.d.ts`、import→`.js`）：

- `"./"` → `lib/types/index.d.ts`（3 行：import force-production-react + re-export `./dsh-adapter/index.js`）
- `"./working-activity"`、`"./oauth"`、`"./workspaces"`、`"./command-trees"`、`"./settings-sections"`、`"./scenes"`、`"./extensions"`、`"./plugin-host"`、`"./api"`、`"./test-utils"`、`"./jsx-runtime"`、`"./invariant"`、`"./cordis.patch.yml"`、`"./package.json"`
- imports 别名 `#dsh-ecosystem-spec/tui-channel` → `./dsh-ecosystem-spec/protocols/tui-channel.js`
- bin：`dsh-tui` 与 `dst` 均指向 `./bin/dsh-tui.js`
- dependencies：`dsh-working-activity ^0.4.0`、`react ^19.2.0`、`react-reconciler ^0.33.0`；bundledDependencies：`@dsh-std/{command,connection,core,manifest,messages,presentation,storage}` + `@deepseek-harness-tui/dsh-auth`
- peerDependencies：`@deepseek-ai/cordis ^4.0.1` 及整组 dsh-* 服务包（dsh-agent/dsh-commands/dsh-settings/dsh-storage/dsh-terminal/dsh-tool-subagent/dsh-user-approval…），全部 optional
- `verify:build` 脚本揭示自检契约面：verify:plugin-{spec,grants,storage,messages,ledger,commands,negotiation,lifecycle}、verify:liangshen-*、verify:inject-channel、verify:runtime-themes、verify:approval-visibility 等

**要点**：扩展点按「一个 row 一个子路径」组织；`./api` 是纯类型聚合（lib/types/api.d.ts），`./working-activity` 与 `./oauth` 是挂载别名 re-export（见 §6）。

---

## 2. 面向第三方插件的扩展点（核心）

### 2.1 dsh-tui-plugin-host —— 插件互操作锚点（C-050/C-010/C-041）

- **声明**：`lib/types/dsh-adapter/plugin-host.d.ts`
  - `TuiPluginHost` 接口：`plugin-host.d.ts:45-57` —— `generationId`（C-050，每次 row 激活的 UUID）、`grants`（GrantStore）、`hostDescriptor()`（C-010）、`describe()`、`subscribeDecision(pluginCtx, event, listener, {scope?, order?})`、`registerCommand(pluginCtx, def | contributionId, def)`（C-041 命令归属）、`selfCheck()`
  - `declare module '@deepseek-ai/cordis'` Context 合并：`plugin-host.d.ts:58-62`（`ctx.tuiPluginHost`）
  - 实现 `TuiPluginHostRuntime extends Service`：`plugin-host.d.ts:64`；row 常量/apply：`:134-135`（`name = "dsh-tui-plugin-host"`）
  - Loader-only 准入 `getHostAdmission`：`:133`（**刻意不导出**包面，插件不可直接 admit）
- **注册机制**：cordis Service；row 由 `cordis.yml:43-44` / `cordis.patch.yml:292-293` 挂载。消费方**禁止 inject**，必须 `ctx.get('tuiPluginHost', false)` 软探测（`plugin-host.d.ts:23-27` 注释，issue #183 纪律）
- **消费方**：channel/Chat 通过该 row 的 apply 挂载兄弟服务（message-observer/plugin-storage/effect-ledger）；命令注册经 C-041 attribution 打 verified Component identity 戳
- **示例**：插件侧 `ctx.tuiPluginHost.subscribeDecision(pluginCtx, 'tui/input', listener, { scope })`（plugin-host.d.ts:50）；`ctx.tuiPluginHost.registerCommand(pluginCtx, commandDefinition)`（:54）

### 2.2 决策事件面（tui/*，6 事件，无 row）

- **声明**：`lib/types/dsh-adapter/extension-events.d.ts`
  - Events 接口合并：`:184-190` —— `'tui/input'(event): TuiInputDecision`、`'tui/rewind-prompt'`、`'tui/rewind-done'`（返回 string→toast）、`'tui/session-switch'`、`'tui/session-switched'`（通知）、`'tui/compact'`
  - 事件名常量 `TUI_DECISION_EVENT_NAMES`（6 个）；超时 `DECISION_HANDLER_TIMEOUT_MS = 1000`（:25）、`DECISION_TOTAL_TIMEOUT_MS = 5000`（:26）
  - 调度函数 `dispatchTuiDecision`（:47）、`dispatchTuiNotification`（:50）、`normalizeCancelDecision`（:55）
  - payload/决策类型：TuiInputEvent{text, delivery:'followup'|'steer'}+TuiDecisionContext{sessionId,cwd}；TuiInputDecision={text}|{handled:true,notice?}|{cancel:true,reason?}|undefined；rewind-prompt→{cancel}|{modes[]}；session-switch→{cancel}|undefined；compact→{cancel}|undefined
- **注册机制**：**不是**裸 `ctx.on`——D-7 守门：拦截类事件订阅需显式 grant（默认 deny），见 §3.1；经 host 中介 DecisionEvents registry（`decision-guard.d.ts:71-79`：`decisionRegistryOf`/`registerDecisionHandler`/决策注册与 dispatch 链与 cordis 私有 `_hooks` 分离）
- **消费方**：channel 实现（编译产物 `lib/types/dsh-adapter/channel.js`）在 5 处触发：`:1207` tui/input、`:1258` tui/session-switch、`:2634` tui/rewind-prompt、`:2765` tui/rewind-done（通知）、`:5144` tui/compact，均包 `withDecisionPending` 串行化、首有效决策胜
- **示例**：订阅拦截用户输入并改写（steer/followup）：manifest 声明 + grant `session.input.intercept` 后 `ctx.on('tui/input', e => ({text: '...'}))`

### 2.3 dsh-tui-extensions —— 6 个 UI 接缝（一个 row 挂 6 服务）

- **声明**：`lib/types/dsh-adapter/extensions.d.ts`（row：`:33-34`，`name="dsh-tui-extensions"`，apply 无 config）；模块头注释 `:1-31` 说明「决策事件无需 row、ctx.on 只是兼容门面不能绕过 admission/scope/grant、消费方 ctx.get 软读」
- **子服务**（各在自己模块，均 Context 合并 + extends Service）：

| 服务 | 声明位置 | 关键 API / 常量 |
|------|----------|----------------|
| `ctx.tuiDialogs` | dialogs.d.ts:115-116（合并）,126（Runtime） | select/confirm/input 队列；TuiDialogStore（:87）；INPUT_CELLS=500（:80）；DIALOG_DEFAULT_TIMEOUT_MS=30000（:82） |
| `ctx.tuiStatus` | status.d.ts:39-40,48 | set(key, text|undefined, identity?): disposer；TuiStatusStore（:18），200 cells 上限 |
| `ctx.tuiShortcuts` | shortcuts.d.ts:49-50,54 | register(combo,{description,handler},identity?)；必须含 ctrl/alt；RESERVED 列表拒绝；parseShortcutCombo/matchShortcut |
| `ctx.tuiRenderers` | renderers.d.ts:39-40,44 | register(type 'plugin/event', renderer(payload)=>TuiEntryRenderResult{title?,lines[]}, identity?) |
| `ctx.tuiToast` | toast.d.ts:40-41,49 | show(text,{color?,timeoutMs?}):boolean；TuiToastStore（:34）；200 cells、20/min 限流 |
| `ctx.tuiThemes` | themes.d.ts:34-35,39 | register({name,displayName?,base:'light'|'dark'|'dark-ansi',colors?}, identity?)；host 侧 getSnapshot/resolve/subscribe |

- **注册机制**：cordis Service；row：`cordis.yml:49-50` / `cordis.patch.yml:301-302`；主 row inject 含 `tuiDialogs,tuiStatus,tuiShortcuts,tuiRenderers,tuiThemes`（仅 entry-level 排序保证，issue #183）
- **消费方**：channel.ts / Chat.tsx 用 `ctx.get` 软读；无 row 时降级（无 dialogs/status/shortcuts/renderers/themes），plugin.ts 记一次 skew 警告
- **示例**：状态行 `ctx.tuiStatus.set('my-plugin', 'running')`；快捷键 `ctx.tuiShortcuts.register('ctrl+alt+k', {description, handler})`；主题 `ctx.tuiThemes.register({name:'solarized', base:'dark', colors:{...}})`；toast `ctx.tuiToast.show('done', {color:'green'})`

### 2.4 dsh-tui-command-trees —— 嵌套斜杠命令补全

- **声明**：`lib/types/dsh-adapter/command-trees.d.ts`：row `:23`（`name="dsh-tui-command-trees"`）、`TuiCommandTreeRuntime extends Service`（:25）、host 门面 getter `getHostCommandTrees`（:31）
- **API**：`ctx.tuiCommandTrees.register(provider: TuiCommandTreeProvider{root, descriptions?, children(canonicalPath)}): disposer`；host 侧 TuiCommandTreeHost 合并所有 provider
- **注册机制**：cordis Service；row：`cordis.yml:30-31` / `cordis.patch.yml:273-274`（注释明确：**执行权与持久命令事件仍归 dsh-commands**，本面只做补全树贡献）
- **消费方**：channel 的斜杠菜单补全

### 2.5 dsh-tui-settings-sections —— 可编辑设置分区

- **声明**：`lib/types/dsh-adapter/settings-sections.d.ts`：row `:113`、`TuiSettingsSectionsRuntime extends Service`（:118）、host getter（:131）、裸 embedder 兜底 `getLocalSettingsSectionsHost`（:132）
- **API**：`ctx.tuiSettingsSections.register({ns, title, descriptions?, groups?, fields[]})`、`list()`、`section(ns)`、`subscribe`；字段 kind: 'text'|'number'|'boolean'|'select'，path 为 settings.mutate 词汇，secret{ref} 凭据控件，format/parse 钩子
- **注册机制**：cordis Service；row：`cordis.yml:35-36` / `cordis.patch.yml:277-278`（注释：**仅展示元数据，存储/校验/写入归 dsh settings 服务**，issue #165）
- **消费方**：TUI /settings 屏

### 2.6 dsh-tui-scenes —— 全屏场景

- **声明**：`lib/types/dsh-adapter/scenes.d.ts`：row `:52`、`TuiSceneRuntime extends Service`（:62）、host getter `getHostSceneRuntime`（:83）
- **API**：`ctx.tuiScenes.register({id, title?, component: React.ComponentType<TuiSceneProps>}, identity?)`、`open(id): boolean`、`close()`、`active`、`subscribe`；`TuiSceneProps{React, ui, channel, close()}`
- **注册机制**：cordis Service；row：`cordis.yml:38-39` / `cordis.patch.yml:283-284`（注释：渲染与输入权仍归 TUI Chat screen，场景由插件自己的命令打开）
- **消费方**：TUI 全屏视图宿主；**JSX 契约**：必须用宿主 React 19，插件 JSX 编译需 `jsxImportSource` 指向本包 `./jsx-runtime` 子路径

### 2.7 dsh-tui-workspaces —— 工作区 provider

- **声明**：`lib/types/dsh-adapter/workspaces.d.ts`：
  - `TuiWorkspaceProvider`：`:68-85`（schemes[]、list()、resolve(uri)、resolvePath?、describe(cwd)、commandShell?(cwd)、rename?、commands?[]）；`TuiWorkspaceCommand`（:43-50）；`TuiCommandShell`（:51-67）
  - host 门面 `TuiWorkspaceHost`：`:89-97`；`TuiWorkspaceRuntime extends Service`：`:107-119`（register/list/resolve/describe/commandShell/rename/commands/runCommand）；`getHostWorkspaceRuntime`：`:121`
  - 裸 embedder 兜底：`createLocalWorkspaceRuntime`（:124）、`localWorkspaceUri`（:125）、`parseLocalWorkspaceReference`（:127）；`WORKSPACE_PROVIDER_TIMEOUT_MS = 2000`（:105）
- **注册机制**：cordis Service；row：`cordis.yml:27-28` / `cordis.patch.yml:268-269`（注释：TUI 自身恒提供 LOCAL provider，无 provider 特定代码；可选插件加外部 scheme）
- **消费方**：TUI 前门 /workspace 选择器、/resume 分类、!command 执行路由；注意 `./workspaces` 子路径默认导出 `TuiWorkspaceRuntime`（lib/types/workspaces.d.ts:1-4）

### 2.8 宿主兄弟服务（由 plugin-host row 的 apply 挂载）

| 服务 | 声明 | 用途 / 注册机制 | 权限 |
|------|------|----------------|------|
| `ctx.tuiMessageObserver` | message-observer.d.ts（Runtime :122, host getter :172；常量 :91-109：OBSERVE_SUMMARY_CELLS=200、OBSERVE_CONTENT_MAX_CHARS=262144、OBSERVE_CALLBACK_TIMEOUT_MS=1500、OBSERVE_CALLBACK_QUEUE_LIMIT=32） | C-042 messages.observe：订阅 user/assistant 消息事件，信封 MessagesObserveEnvelope{eventType:'messages.observe', eventVersion:'0.15', eventId, scope, sequence, privacyClass, summary, payload}；user/message→message.received、assistant/message→message.sent；privacyClass 恒 sensitive | grant `messages.observe.read` 默认 deny，订阅时与投递时双重检查 |
| `ctx.tuiPluginStorage` | plugin-storage.d.ts（Runtime :87；STORAGE_MAX_KEYS=256 :39、STORAGE_MAX_BYTES :41、STORAGE_KEY_MAX_LENGTH=128 :43、PLUGIN_STORAGE_DIR="plugin-storage" :75、storageFileName :81；PluginStorageError :46） | C-040 storage.local：open(pluginCtx)=>TuiPluginStorage{get,set,delete}；命名空间由 verified identity 派生；配额 256 keys/256KiB；后端 `~/.dsh-tui/plugin-storage/<ns>.json` 走 dsh-atomic-write；错误码 PERMISSION_NOT_GRANTED|INVALID_KEY|INVALID_VALUE|QUOTA_EXCEEDED|STORAGE_UNAVAILABLE | 权限检查由 identity 派生 |
| `ctx.tuiEffectLedger` | effect-ledger.d.ts（Runtime :68；EFFECT_LEDGER_FILE :39；LEDGER_RESOURCE_KINDS :41 = ['command','scene','shortcut','status','renderer','theme','storage-namespace','subscription','permission']） | C-060 追加式 JSONL `~/.dsh-tui/effect-ledger.jsonl`；record(entry, identity?)；pluginId 取 verified identity，'undeclared' 兜底；schema 校验 fail-closed | — |

- **注册机制**：三者均为 cordis Service，**不在** cordis.yml 单独成 row——由 `dsh-tui-plugin-host` row 的 apply 统一挂载（plugin-host.d.ts:1-6 注释：patch 面只改一次）
- **消费方**：外部插件（观察消息流、持久化小 KV、审计自己的 UI 副作用）；host 侧 selfCheck/descriptor 纳入自检

---

## 3. 契约/权限面（治理型扩展点）

### 3.1 grants + decision-guard（D-7 强制）

- `lib/types/dsh-adapter/grants.d.ts`：`EXTENSION_GRANTS_FILE = "extension-grants.json"`（:3，位于 `~/.dsh-tui/`）；`parseGrantStore`（:18）、`readGrantStore`（:24）；GrantStore{allows(principal,permission,scope), defaultOf, knownPermissions, onChange?, corrupt}
- `lib/types/dsh-adapter/decision-guard.d.ts`：`DECISION_EVENT_PERMISSIONS`（:35）映射事件→'session.input.intercept'|'session.rewind.intercept'|'session.switch.intercept'|'session.compact.intercept'（即 TUI_EXTENSION_PERMISSION_NAMES）；`installDecisionGuard`（:83）用 cordis bail `internal/listener` 钩子对**每一次** `ctx.on` 强制检查；re-check 语义（订阅/撤销/scope 变更均读 live GrantStore，撤销即释放，:20-22 注释）
- **注册机制**：文件（`~/.dsh-tui/extension-grants.json`）+ 内嵌 registry 默认；decision-guard 的 D-7 不依赖 plugin-host row（extensions row 与 channel 各自安装，:28-30 注释）
- **消费方**：任何想订阅拦截类决策事件的插件（默认 deny，必须显式 grant）

### 3.2 admission profile registry

- `dsh-ecosystem-spec/registry/registry-0.15.json`（149 行）：
  - imports（:8-77）：commands（@dsh-std/command，commands.invoke）、storage.local（@dsh-std/storage）、messages.observe（@dsh-std/messages，event 信封）、presentation.{open-external,user-interaction,external-redirect}（@dsh-std/presentation）
  - 私有 definitions（:78-108）：`tui.decision-events`（DecisionEvents，权限 4 项 intercept）、`tui.channel`（Channel，权限空）
  - extensions（:109-145）：`workspace.provider`（@dsh-std/workspace）、`tui.settings-section`、`tui.scene`
  - facetApiVersions: ["v1alpha1"]（:146-148）
- `dsh-ecosystem-spec/registry/README.md`：注册表只是 admission profile 的 definition 集合（imports 引用 dsh-std、definitions 只收私有）；profile 用 SHA-256 固定；`tui.dsh/*` 是私有 namespace；**permission grant 与 protocol support 分开判断**（:14）
- `dsh-ecosystem-spec/schemas/`：host-descriptor.schema.json、effect-ledger-record.schema.json、conformance-claim.schema.json
- `dsh-ecosystem-spec/protocols/tui-channel.d.ts`：TUI_CHANNEL apiVersion 'tui.dsh/v1alpha1' kind 'Channel'（:5-8）、wire revision（:9）、TuiChannelInput 4 操作 open/subscribe/invoke/close（:42-46）；tui-contributions.d.ts / tui-channel-http.d.ts / profile-definitions.d.ts 同目录
- 插件侧入口：`lib/types/plugin-spec/`——types.d.ts（ContractCoordinate/HostDescriptor/NegotiationDecision/RegistryEntry）、tui-extension.d.ts（TUI_EXTENSION_API_VERSION="tui.dsh/v1alpha1"、createAdmissionCatalog）、negotiate.d.ts、registry.d.ts、permission-scope.d.ts、schema-check.d.ts、validate.d.ts

### 3.3 component-identity（verified 身份）

- `lib/types/dsh-adapter/component-identity.d.ts`：`VerifiedComponentIdentity{componentId, version, facet:'host', activationId, manifest, projection}`（:9-16）；`bindComponentIdentity`（:21）、`componentIdentityOf`（:22）、`requireComponentIdentity`（:23）、`declaresPermission`（:24）、`declaresCommand`（:25）、`declaresObserverScope`（:26）、`requiresDecisionEvents`（:27）、`requiresContract`（:32）；`ACTIVATION_ID_MAX_LENGTH = 128`（:20）
- **机制**：所有受管能力的调用方身份（命令归属、storage ns、ledger pluginId、grant principal 均派生自此）

### 3.4 host-descriptor

- `lib/types/dsh-adapter/host-descriptor.d.ts`：`HOST_SUPPORTED_CONTRACTS`（:3）、`HOST_FACET_API_VERSIONS`（:8）、`buildHostDescriptor({hostId, hostVersion, generationId, headless?, supported?, specDir?})`（:23）→ HostDescriptorBuild{descriptor, dropped, warnings}；drifted 契约 fail-closed 丢弃

---

## 4. 配置面（部署者扩展）

### 4.1 主插件 Config（lib/types/dsh-adapter/index.d.ts）

- `name = "dsh-tui"`（:14）、`inject: string[]`（:15）、`apply(ctx, Config): Promise<void>`（:125，动态 import 转 JSX 实现 plugin.tsx）
- Config 字段（:20-116）：sessionId/provider/model（路由原子解析，issue #67）/cwd/workspace/effort/activity/activityFrames/contextBar/fullscreen(默认 true)/lang/preset/diffLayout/thinkingFold/toolBackground/scrollGutter/foldTerminalCommand/promptSessionLabel/expandEditor/smoothStreaming/statusBar/shortcuts(按 ShortcutActionId)/modes(Shift+Tab 会话模式循环，SessionModeSpec[])
- **注册机制**：cordis row config；包内 cordis.yml:52-57（inject 8 服务 + provider/effort/sessionId/workspace），cordis.patch.yml:359-415（dsh-tui 主 row，inject 9 项含 workspaceRegistry，preset/workspace/sessionId 均 !!js env 可覆盖）

### 4.2 bundle patch 层（cordis.patch.yml，432 行）

- 禁用 base row（preset 接管模型面工具）：agent-instructions/command-compact/compaction-basic/plan-mode/skill-filesystem/tool-bash/tool-fs/tool-fs-search/command-goal(条件)/tool-goal/tool-jobs/tool-pwsh/tool-ralph/tool-result-pruner/tool-skill/tool-str-replace-editor/tool-subagent/tool-subagent-control/tool-subagent-fork/tool-subagent-list-agents/tool-todo/tool-web/tool-workflow/workflow-worker-thread
- insert 的 dsh-tui-* row：dsh-tui-storage/dsh-tui-storage-json/dsh-tui-storage-domain/dsh-tui-workspace/dsh-tui-code-runtime（均 !!js 自禁用检测，web-app 同 id/name 已启用则 disabled）、dsh-tui-subagent-model-selection-settings、dsh-tui-workspaces、dsh-tui-command-trees、dsh-tui-settings-sections、dsh-tui-scenes、dsh-tui-plugin-host、dsh-tui-extensions、dsh-tui-agent-presets（default standard）、dsh-tui-cordis-host-runner、dsh-tui-auth（oauth，inject:[llm,commands]）、dsh-tui 主 row、working-activity（publish:false）
- profile 根 `C:\Users\wps\.dsh\profiles\dsh-tui\cordis.yml` = `[]`、`cordis.patch.yml` = `[]`（纯 bundles 组合）

---

## 5. 预设面（agent-plane 扩展，非渲染扩展）

- `presets/liangshen/`（8 文件）：`preset.yml`（3 行：name 梁神模式 / description / order: 5）+ `agent.cordis.yml`（391 行，anchored-standard 实验预设）+ tool-bootstrap.mjs / custom-bash.mjs / instruction-hint.mjs / skill-search.mjs / compaction-epoch.mjs / .dsh-tui-managed.json
- agent.cordis.yml row 清单（grep 行号）：tool-bootstrap(:57)、persona(:71)、instruction-hint(:84)、tool-bash(:99)、tool-pwsh(:108)、persistent-shell(:122)、custom-bash(:162)、tool-fs(:170)、tool-fs-search(:173)、bootstrap-filesystem(:184)、tool-jobs(:209)、skill-filesystem(:223)、skill-search(:226)、tool-goal(:237)、planning(:244)、compaction(:277)、delegation(:314)、tool-ask-user(:377)、tool-todo(:380)、tool-web(:387)
- **机制**：文件目录预设，由 `dsh-tui-agent-presets`（@deepseek-ai/dsh-agent-presets，cordis.patch.yml:310-333）roster 挂载（default standard）；会话经 `/preset` 选择；preset row 必须处于 isolate realm（agent.cordis.yml:12-19 注释）；**它扩展的是「agent 的工具/提示词面」而非 TUI 渲染面**
- 用户侧：预设文件由插件维护于 `~/.dsh/.agent-presets` 或随包 `presets/`；新建会话预设选择器可选「梁神模式」

---

## 6. 子路径 re-export（部署/开发面）

- `lib/types/working-activity.d.ts`：re-export `dsh-working-activity`（^0.4.0），本地 apply 遮蔽星号 re-export 以强制 publish=false（issue #60 pnpm 布局 + #143/#153 日志污染）；row：cordis.yml:212-216 / cordis.patch.yml:428-432
- `lib/types/oauth.d.ts`（15 行）：re-export `@deepseek-harness-tui/dsh-auth`（bundled），供 patch 层以 `@deepseek-harness-tui/dsh-tui/oauth` 挂载（cordis.patch.yml:349-351）
- `lib/types/api.d.ts`（7 行）：纯类型聚合（extensions/plugin-host/scenes/settings-sections/command-trees/workspaces）
- `./jsx-runtime`：re-export 宿主 react/jsx-runtime（插件 JSX 编译用）；`./test-utils`：插件测试工具

---

## 7. 消费方链路小结（TUI 壳如何发现/加载扩展）

1. **Service 面**：各 `dsh-tui-*` row 由 cordis.yml/patch 挂载 → apply 内 `ctx.provide` 服务；TUI 壳（channel.ts/Chat.tsx）一律 `ctx.get` **软读**（不 inject），无 row 时降级而不是死锁（issue #183 贯穿所有 row 注释）
2. **决策事件面**：channel.js 在 5 处调用 `dispatchTuiDecision`（lib/types/dsh-adapter/channel.js:1207/1258/2634/2765/5144）→ host 中介 DecisionEvents registry 串行分发 → 插件监听；订阅经 decision-guard bail 钩子强制 grant 检查
3. **host 门面**：每个 Runtime 配 `getHost*Runtime` getter（如 getHostCommandTrees/getHostSceneRuntime/getHostWorkspaceRuntime/getHostSettingsSections/getHostMessageObserver），channel 用它们合并多插件贡献
4. **身份**：插件由 verified Component identity 标识（manifest+projection），grant/storage/ledger/命令归属均以此为准
5. **无 row 降级**：workspaces 有 createLocalWorkspaceRuntime 兜底、settings-sections 有 getLocalSettingsSectionsHost 兜底

---

## 8. 内部模块 vs 外部扩展标注

- **面向第三方插件（外部扩展面）**：§2 全部（plugin-host、6 决策事件、extensions 六服务、command-trees、settings-sections、scenes、workspaces、message-observer、plugin-storage、effect-ledger）+ §3 治理面（grants/decision-guard/registry/identity/descriptor）
- **TUI 内部模块（非扩展点，仅供 host 自用）**：channel.js（会话/渲染主线）、plugin.tsx（JSX 主实现）、ink/（移植渲染器）、screens/、components/、sessions/、themePrefs、keymap、inject-channel（IPC）、presets 加载器（dsh-adapter/presets.d.ts、packaged-presets.d.ts）
- **部署/开发面**：Config schema（§4.1）、cordis.patch.yml bundle 层（§4.2）、preset 文件（§5）、working-activity/oauth 别名与 api/jsx-runtime/test-utils（§6）

---

## 9. 备注（供 t3 审查交叉验证）

- 本清单以 **.d.ts 声明为准**（编译产物 lib/types/ 同时含 .js 实现与 .d.ts 声明）；行号已逐条 grep 验证
- 决策事件「无 row」是设计（extensions.d.ts:16-25 注释）；拦截类订阅的权限命名即 TUI_EXTENSION_PERMISSION_NAMES，与 registry-0.15.json 的 permissions 一致
- 现有扩展接入方式（如 dsh-working-activity）属 t2 范围；本清单给出其挂载机制线索（§6 working-activity re-export + cordis.yml:212-216）
- 未发现 `registerExtension` 式通用扩展槽；扩展面统一为「cordis Service + Events + grants/registry 治理」三件套
