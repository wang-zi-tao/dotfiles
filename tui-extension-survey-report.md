# dsh-tui TUI 扩展点交叉审查报告（t3 终稿）

> 审查人：tui-reviewer ｜ 考察对象：`@deepseek-harness-tui/dsh-tui` v0.10.0-beta.4（dsh-tui profile 与 tui profile 均此版本）
> 输入：t1 `tui-extension-points.md`（227 行，tui-explorer 考察）+ t2 `C:\dotfiles-copy\existing-tui-extensions.md`（113 行）
> 复核方式：只读核对 package.json / lib/types/dsh-adapter/*.d.ts / cordis.yml / cordis.patch.yml / registry-0.15.json / presets / 各 profile 安装状态
> 工作区：`C:\dotfiles-copy`（t1/t2 产物同根）

---

## 0. 结论速览

1. **t1 准确度高**：20 个扩展点、5 族划分、关键行号几乎全部与源码一致（下方逐项核实表）。仅一处小遗漏：channel.js 的 dispatch 实为 **6 处**（t1 记 5 处，漏 :1281 的 `tui/session-switched` 通知）。
2. **t2 存在 3 处实质错误**：
   - **`ctx.tuiPrompt` / `theme.leftPrompt` 在宿主 v0.10.0-beta.4 不存在**（lib 全 grep 零匹配；extensions.d.ts 头注释仅 6 服务；Theme 类型为纯颜色键 ~120 字段无模板字段；主行 inject 无 tuiPrompt）。这是 dsh-working-activity **插件单方面软探测契约**（`ctx.get('tuiPrompt', false)` 返回 undefined 时静默跳过），**TUI 中 activity 实际不渲染**。t2 把它当作「已接入的 prompt 区扩展点」属于以 README 代源码。
   - **决策事件只列 4 个**（tui/input、tui/rewind-prompt、tui/session-switch、tui/compact），漏 `tui/rewind-done` 与 `tui/session-switched`（实际 6 个，t1 正确）。
   - **dsh-tui profile「无 cordis.patch.yml」**：实际文件存在且内容为 `[]`（4 行注释+空数组），t1 的「cordis.yml=[]、cordis.patch.yml=[]」正确。
3. **两报告核心一致性**：现有扩展面 = 预设平面（liangshen 三源并存）+ 配置面（profile patch）+ working-activity 子路径挂载；UI 接缝服务（tuiStatus 等 6 个）与 decision 事件**目前零真实接入**。

---

## 1. TUI 扩展点总表（名称/机制/消费方/现有扩展或空缺）

来源：t1 清单，逐项经源码复核（行号 = 本次实测）。

### 1.1 Cordis Service 注册面（8 个 row / 12+ 服务）

| # | 扩展点（row → ctx 句柄） | 注册机制 | 消费方 | 现有扩展 | 复核 |
|---|---|---|---|---|---|
| 1 | `dsh-tui-plugin-host` → `ctx.tuiPluginHost` | plugin-host.d.ts:45-57 `TuiPluginHost{generationId,grants,hostDescriptor(),describe(),subscribeDecision(pluginCtx,event,listener,{scope?,order?}),registerCommand(pluginCtx,def|contributionId,def),selfCheck()}`；Context 合并 :58-62；row name :134-135；cordis.yml:43-44 / patch:292-293 | 插件经 host 中介订阅决策事件/注册命令；身份 verified identity | **无**（无插件调用） | ✅ 全符 |
| 2-7 | `dsh-tui-extensions` → `tuiDialogs` | dialogs.d.ts:115-116/126；INPUT_CELLS=500(:80)、DIALOG_DEFAULT_TIMEOUT_MS=30000(:82) | 插件 ask select/confirm/input | **无** | ✅ |
| | → `tuiStatus` | status.d.ts:39-40/48；set(key,text,identity):disposer、200 cells 上限 | TUI 状态行 | **无**（t2 断言 working-activity 用 prompt 区而非状态行，属实——但它两边都没真生效） | ✅ |
| | → `tuiShortcuts` | shortcuts.d.ts:49-50/54；register(combo,{description,handler},identity)，组合必须含 ctrl/alt | 键盘层 | **无** | ✅ |
| | → `tuiRenderers` | renderers.d.ts:39-40/44；register(type,renderer→{title?,lines[]},identity) | 会话列表渲染 | **无** | ✅ |
| | → `tuiToast` | toast.d.ts:40-41/49；show(text,{color?,timeoutMs?}):boolean、200 cells、20/min 限流 | 任意插件 | **无** | ✅ |
| | → `tuiThemes` | themes.d.ts:34-35/39；register({name,displayName?,base,colors?},identity) | 主题系统 | **无** | ✅ |
| 8 | `dsh-tui-command-trees` → `ctx.tuiCommandTrees` | command-trees.d.ts:23/:25/:31；register(provider{root,descriptions?,children});执行权归 dsh-commands | 斜杠命令补全 | **无** | ✅ |
| 9 | `dsh-tui-settings-sections` → `ctx.tuiSettingsSections` | settings-sections.d.ts:113/:118/:131/:132；register({ns,title,groups?,fields[]})；存储/校验归 dsh settings | 设置屏 | **无** | ✅ |
| 10 | `dsh-tui-scenes` → `ctx.tuiScenes` | scenes.d.ts:52/:62/:83；register({id,title?,component},identity)、open/close/active/subscribe | 全屏场景 | **无** | ✅ |
| 11 | `dsh-tui-workspaces` → `ctx.tuiWorkspaces` | workspaces.d.ts:68-85 `TuiWorkspaceProvider{schemes[],list(),resolve(uri),resolvePath?,describe(cwd),commandShell?(cwd),rename?,commands?[]}`、:43-50 Command、:51-67 Shell、:89-97 Host、:107-119 Runtime、:121 getHostWorkspaceRuntime、:124 createLocalWorkspaceRuntime、WORKSPACE_PROVIDER_TIMEOUT_MS=2000(:105) | /workspace 面板 | **无第三方 provider**（TUI 自供 LOCAL） | ✅ |
| 12-14 | plugin-host 挂载兄弟服务：`tuiMessageObserver` | message-observer.d.ts Runtime:122/host:172；OBSERVE_CALLBACK_QUEUE_LIMIT=32(:106)、OBSERVE_IMAGE_READ_TIMEOUT_MS=1500(:109)；C-042 messages.observe；grant `messages.observe.read` 默认 deny | 插件订阅消息观察 | **无** | ✅ |
| | `tuiPluginStorage` | plugin-storage.d.ts Runtime:87；STORAGE_MAX_KEYS=256(:39)、STORAGE_KEY_MAX_LENGTH=128(:43)、PLUGIN_STORAGE_DIR='plugin-storage'(:75)；C-040 storage.local；`~/.dsh-tui/plugin-storage/<ns>.json` | 插件持久化 | **无** | ✅ |
| | `tuiEffectLedger` | effect-ledger.d.ts Runtime:68；EFFECT_LEDGER_FILE(:39)；LEDGER_RESOURCE_KINDS(:41) 9 类；C-060 JSONL | 审计 | 自动记录所有注册 | ✅ |

### 1.2 决策事件面（6 个 tui/* 事件，无 row，channel 直接 dispatch）

extension-events.d.ts:184-190 Events 合并确认 6 事件；DECISION_HANDLER_TIMEOUT_MS=1000(:25)、DECISION_TOTAL_TIMEOUT_MS=5000(:26)；dispatchTuiDecision(:47)/dispatchTuiNotification(:50)/normalizeCancelDecision(:55)。channel.js 实测 **6 处** dispatch：

| 行号 | 事件 | 语义 | 权限（DECISION_EVENT_PERMISSIONS，decision-guard.d.ts:35） |
|---|---|---|---|
| :1207 | `tui/input` | 提交前改写/吞掉/取消（submit+steer；! 行不发） | session.input.intercept |
| :2634 | `tui/rewind-prompt` | fork 前取消或加模式 | session.rewind.intercept |
| :2765 | `tui/rewind-done` | 完成后返回 string→toast（观察类，无权限） | — |
| :1258 | `tui/session-switch` | /new、/resume 前 veto | session.switch.intercept |
| :1281 | `tui/session-switched` | 切换后通知（观察类，无权限） | — |
| :5144 | `tui/compact` | 手动 /compact 前 veto | session.compact.intercept |

- 拦截类订阅需 `~/.dsh-tui/extension-grants.json` 显式 grant，默认 deny；installDecisionGuard(:83) 装 cordis bail 钩子，channel 与 extensions row 幂等双装。
- **现有扩展：无**（无已装插件依赖决策事件；working-activity 只用观察类 session 事件流）。
- **t2 缺口**：§1.1 仅列 4 事件，漏 rewind-done/session-switched 及对应类型；且「消费方 channel.ts/Chat.tsx 用 ctx.get 软读」表述不精确——决策事件是 host 中介 dispatch，ctx.get 软读针对的是 6 个服务。

### 1.3 契约/权限面

| 扩展点 | 机制 | 复核 |
|---|---|---|
| registry-0.15.json（149 行） | imports: commands/storage.local/messages.observe/presentation.{open-external,user-interaction,external-redirect}；definitions: tui.decision-events(4 项 intercept 权限)/tui.channel；extensions: workspace.provider/tui.settings-section/tui.scene；facetApiVersions ['v1alpha1'] | ✅ 全符 t1 |
| grants + decision-guard | grants.d.ts `EXTENSION_GRANTS_FILE='extension-grants.json'`（~/.dsh-tui/）；GrantStore{allows,defaultOf,knownPermissions,onChange,corrupt}；拦截默认 deny | ✅ |
| component-identity / host-descriptor | VerifiedComponentIdentity{componentId,version,facet:'host',activationId,manifest,projection}；buildHostDescriptor({hostId,hostVersion,generationId,...}) | ✅ |

### 1.4 配置面

| 扩展点 | 机制 | 复核 |
|---|---|---|
| 主插件 Config（index.d.ts:20-116） | sessionId/provider/model/cwd/workspace/effort/activity/activityFrames/contextBar/fullscreen(默认 true)/lang/preset/diffLayout/thinkingFold/toolBackground/scrollGutter/foldTerminalCommand/promptSessionLabel/expandEditor/smoothStreaming/statusBar/shortcuts(按 ShortcutActionId)/modes | ✅ 与 t1 一致 |
| profile cordis.patch.yml（432 行） | id 定向覆盖 + insert 17 行（storage 栈 4 行/workspace/code-runtime/subagent-model-selection-settings/workspaces/command-trees/settings-sections/scenes/plugin-host/extensions/agent-presets(default standard)/cordis-host-runner/auth(oauth inject [llm,commands] :349-351)/主行 dsh-tui(:359-415 inject [workspaceRegistry,agents,tuiWorkspaces,tuiScenes,tuiDialogs,tuiStatus,tuiShortcuts,tuiRenderers,tuiThemes])/working-activity(:428-432 publish:false)）；官方同名行在场自禁用（!!js 表达式） | ✅ 全符 t1 |
| 主行 inject | **无 tuiPrompt/tuiCommandTrees/tuiSettingsSections**（entry-level 排序，issue #183 软读降级设计） | ✅ |

### 1.5 预设面

| 扩展点 | 机制 | 复核 |
|---|---|---|
| presets/*/agent.cordis.yml | dsh-tui 打包 presets/liangshen 8 文件（preset.yml name 梁神模式 order 5 + agent.cordis.yml 391 行 anchored-standard + 5 个 .mjs + .dsh-tui-managed.json） | ✅ 存在确认 |
| dsh-tui-agent-presets 名册 | patch:310-333：@deepseek-ai/dsh-agent-presets，default standard，用户根 `~/.dsh/.agent-presets`（rc 兜底 trust system），官方行在场 self-disable | ✅ |

---

## 2. 现有扩展总表（t2 内容 + t3 复核修正）

| 扩展 | 类型 | 接入面 | 复核结论 |
|---|---|---|---|
| `dsh-working-activity@0.4.0` | 第三方 npm 包（已被 dsh-tui 收为 dep ^0.4.0） | 经 `@deepseek-harness-tui/dsh-tui/working-activity` 子路径挂载（cordis.yml:212-216 / patch:428-432，config publish:false publishIntervalMs:500）；apply 内 `ctx.get('tuiPrompt',false)` 软探测 → `prompt?.register('activity',undefined)` → `promptHandle?.set(line)` | **⚠️ t2 关键错误**：宿主**无 tuiPrompt/leftPrompt**（lib 零匹配），探测恒返回 undefined，**TUI 侧 activity 实际不渲染**。生效的是：①systemPrompt section `working-activity:narrate`(order 60)；②Web 半边 `ctx.slots.inject('conversation.input.dock')` id activity order 15（dsh.client.platform=web，inject 3 服务已验证）；③session 事件流消费。 |
| `presets/liangshen`（dsh-tui 包内） | 打包预设 | agent-presets 名册，order 5，revision `liangshen-toolcall-full-catalog-subagents-durable-hint-v5` | ✅ 与 t1/t2 描述一致 |
| `~/.dsh/.agent-presets/` liangshen（order 4）+ router-spec + router-standard | 用户级预设 | dsh-tui-agent-presets 用户根消费 | ✅ 三目录 15 文件实测存在 |
| `@linxin666/dsh-liangshen@0.1.20` | 独立 bundle 插件 | dsh.bundle.patch insert `{id:liangshen,name:'@linxin666/dsh-liangshen'}`；presets/liangshen 同步到用户根 | ✅ tui profile 实测安装（package.json version 0.1.20；bundles 顺序 dsh-base→workflow→dsh-liangshen→…→dsh-tui→skin→task-board→dsh-neovim 与 t2 一致） |
| profile patch 接线（tui/web） | 配置覆盖 | dsh-tui 主行 `{preset:code, provider:wpscodingplan, model:deepseek/deepseek-v4-flash-0731, effort:high}`；dsh-tui-code-runtime→`@deepseek-ai/dsh-code-runtime-worker-thread`；7 个 MCP client + skill-filesystem-opencode + agent-teams/distill/compaction-acp/billion-context-dsh/dsh-hindsight | ✅ tui patch 131 行实测全符 t2 |
| 未接入 profile | — | dsh-tui（cordis.yml=[] **且 cordis.patch.yml=[] 存在**——t2 误称「无」）；cc-tui（patch=[]）；~/.dsh/cordis.patch.yml（8 个 skin 行 disabled，与 TUI 无关） | ✅ 均已实测 |

**UI 接缝服务使用现状**：tuiStatus/tuiDialogs/tuiShortcuts/tuiRenderers/tuiToast/tuiThemes/tuiScenes/tuiWorkspaces/tuiCommandTrees/tuiSettingsSections/tuiMessageObserver/tuiPluginStorage —— **全部零真实接入**；decision 事件零订阅（无 extension-grants.json grant 配置证据）。t2 §3.1「目前实际使用 ctx.tuiPrompt 的只有 working-activity 一家」需降级为「**没有任何扩展实际连上 UI 接缝**——working-activity 的 TUI 槽位因宿主无实现而不生效」。

---

## 3. 生态成熟度评价

**扩展点体系：成熟且治理完备（评分 8/10）**
- 五族分层清晰（Service/Events/契约权限/配置/预设），v0.15 registry + grants 默认 deny + effect ledger 审计 + verified identity + 40+ verify 脚本（verify:plugin-{spec,grants,storage,messages,ledger,commands,negotiation,lifecycle} 等），防呆设计周全（软读降级、超时隔离、幂等 bail 钩子、issue #183 注入纪律）。
- dispatch 语义刻意绕开 cordis `ctx.serial` 的 bail 陷阱，做逐 handler 隔离+归一化+预算，成熟度高于多数插件宿主。

**接入生态：几乎空白（评分 1/10）**
- 唯一「第三方」扩展 working-activity 的 TUI 接缝因宿主未实现 tuiPrompt 而空转（插件为 Web 优先设计，dsh.client.platform=web）。
- 活跃平面集中在**非渲染面**：预设名册（liangshen 三源并存同源演进）与 profile 配置覆盖。
- 无一个插件使用 status/shortcuts/dialogs/renderers/toast/themes/scenes/workspaces/command-trees/settings-sections/storage/message-observer；decision 事件零消费。

**结论**：TUI 扩展基础设施是「高配低用」状态——宿主提供了完整、安全、可审计的接缝，但生态尚未长出真实 UI 插件。这与 v0.10.0-beta 早期阶段相符，也与 t2 的「正确姿势=insert 一行+收录 bundle」结论一致，但 t2 对 working-activity 的实际生效状态判断过乐观。

---

## 4. 扩展者最短上手路径（基于本次实测）

1. **建包**：npm 包 + `dsh.bundle.patch` 指向 cordis.patch.yml，内含单条 `- insert: {id: <唯一id>, name: '<包名>'}`（dsh-working-activity 先例；dsh-tui 自身也走同构 insert）。
2. **收录**：profile package.json `dsh.profile.bundles` 数组加入包名；注意 pnpm isolated 布局下裸包名解析会 `ERR_MODULE_NOT_FOUND`（issue #60）——**优先经宿主 subpath 重导出挂载**（如 `@deepseek-harness-tui/dsh-tui/working-activity`）。
3. **接线**：`apply(ctx)` 内全部用 `ctx.get('tuiStatus', false)` 式**软读**（无 row 时静默降级，勿 inject——issue #183 防 boot 死锁）；UI 效果用 status.set / toast.show / shortcuts.register / themes.register / scenes.register / workspaces.register / command-trees.register / settings-sections.register。
4. **决策事件**（如需拦截 input/switch/rewind/compact）：订阅 `tui/*` 需先在 `~/.dsh-tui/extension-grants.json` 显式 grant（默认 deny）；经 `ctx.tuiPluginHost.subscribeDecision` 或 host 中介注册；返回归一化决策（cancel/text/handled/modes）。
5. **持久化/观察**：`ctx.tuiPluginStorage.open(ns)`（storage.local，256 keys）；`ctx.tuiMessageObserver.subscribe(ctx,listener,{scope})`（messages.observe.read 默认 deny）；全部自动进 effect ledger。
6. **⚠️ 文档坑**：**以 .d.ts 声明为准，勿信 README**——dsh-working-activity README 声称的 `ctx.tuiPrompt`/theme.leftPrompt 在 v0.10.0-beta.4 宿主中不存在，插件自身靠软探测容忍该缺口，但扩展者若照 README 开发会拿到空槽位。

---

## 5. 交叉审查裁定

| 维度 | t1（扩展点清单） | t2（现有扩展清单） |
|---|---|---|
| 一致性 | ✅ 6 服务无 tuiPrompt 正确；与 t2 冲突处以 t1 为准 | ⚠️ 7 服务含 tuiPrompt 系误列（README 来源） |
| 完整性 | ⚠️ 小遗漏：channel dispatch 5→6（漏 :1281 session-switched 通知），事件清单本身 6 个完整 | ❌ 决策事件漏 2 个；dsh-tui profile patch 存在性判断错 |
| 准确性 | ✅ 抽查 ~30 处行号/常量/挂载点全部吻合 | ⚠️ tuiPrompt/leftPrompt 主张无宿主实现支撑；其余（版本/路径/bundle 顺序/配置覆盖）准确 |
| 综合 | **通过（附 1 条补注）** | **需修订 3 处后再作为最终依据** |

**给下一环节的修订建议（t2 若继续使用）**：
- §1.1 删除或降级 `ctx.tuiPrompt` 行（标注「插件侧软探测契约，宿主 v0.10.0-beta.4 未实现，TUI 不渲染」）；
- §1.1 决策事件补全为 6 个；
- §2.2⑤ dsh-tui profile 改为「cordis.yml=[] 且 cordis.patch.yml=[]（空骨架）」；
- §3.1 改为「UI 接缝服务目前零真实接入」。
