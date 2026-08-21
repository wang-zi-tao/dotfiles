---
name: kcontext-skill
description: 使用 kso-context MCP 工具查询公司内部代码库文档与 API 上下文。触发词（满足其一）：use kctx、kskillhub-kcontext、kcontext。用户消息包含任一则必须使用本 skill。
---

# kso-context MCP Skill

## 触发方式

用户消息中包含以下**任一**关键词时激活本 skill，然后按下方工作流调用 MCP 工具完成请求：

| 关键词 | 说明 |
|--------|------|
| **`use kctx`** | 显式指令 |
| **`kskillhub-kcontext`** | MCP 服务标识名（配置、连接或排查时常出现） |
| **`kcontext`** | 产品/能力简称 |

以上与内部域名库触发规则并列；出现任一则**必须**走本 skill 的 MCP 工作流。

`kso-context` 是公司内部代码库的 AI 上下文引擎，索引了内部 GitLab 仓库，提供 4 个 MCP 工具。

## 工具速查

| 工具 | 用途 | 必填参数 |
|------|------|---------|
| `search-libraries` | 按名称/关键词搜索库，获取 `libraryId` | `query` |
| `discover-libraries` | 自然语言描述需求，跨库推荐最相关的库 | `query` |
| `get-library-context` | 查某库的具体 API / 代码片段 / 用法 | `libraryId`, `query` |
| `get-library-summary` | 获取某库的整体架构概览 | `libraryId` |

## 标准工作流

### 场景 A：已知库名，查具体用法
```
1. search-libraries(query="库名")        → 取 libraryId（仓库 URL）
2. get-library-context(libraryId, query) → 获取 API 与代码示例
```

### 场景 B：不知用哪个库，按需求找
```
1. discover-libraries(query="自然语言需求", language="go")  → 推荐候选库列表
2. get-library-summary(libraryId)                          → 快速了解架构
3. get-library-context(libraryId, query)                   → 深入查具体用法
```

### 场景 C：先建立整体认知再深入
```
1. search-libraries / discover-libraries  → 定位 libraryId
2. get-library-summary(libraryId)         → 整体架构 + 主要包
3. get-library-context(libraryId, query)  → 针对具体功能查文档
```

## 各工具详解

### `search-libraries`
- `query`：库名、路径片段或关键词，如 `xfx`、`o/xkit`、`router`
- 返回：JSON，含 `results[].url`（即 `libraryId`）、`name`、`description`、`snippet_count`

### `discover-libraries`
- `query`：自然语言目标，如 `distributed lock`、`HTTP retry with circuit breaker`
- `language`（可选）：`go`、`python` 等
- 返回：JSON，最多 5 个候选，含 `score`、`reason`、`matched_packages`

### `get-library-context`
- `libraryId`：仓库 URL 或路径后缀（来自 search/discover 结果）
- `query`：想了解的具体内容
- `branch`（可选）：指定分支
- `maxTokens`（可选）：限制返回长度
- `snippetType`（可选）：`code`（代码片段）或 `info`（说明文档）
- 返回：**纯文本**，可直接作为 prompt 上下文

### `get-library-summary`
- `libraryId`：仓库 URL 或名称
- `branch`（可选）
- 返回：**纯文本**，库的整体描述、架构与主要功能

## 典型示例

**用户问**：「xfx 框架怎么注册路由？」
```
search-libraries(query="xfx")
→ libraryId = "https://ksogit.kingsoft.net/o/xfx"

get-library-context(
  libraryId = "https://ksogit.kingsoft.net/o/xfx",
  query = "register route HTTP handler"
)
```

**用户问**：「有没有内部的分布式锁库？」
```
discover-libraries(
  query = "distributed lock redis",
  language = "go"
)
→ 返回候选库列表，挑选合适的 libraryId

get-library-summary(libraryId)
get-library-context(libraryId, query="how to use distributed lock")
```

## 处理返回结果的规则

### 规则 1：遇到 Deprecated 标识，停止推荐，反问用户

若返回内容中包含 `Deprecated`、`@Deprecated`、`废弃`、`deprecated` 等标识，**禁止直接使用该代码**，必须：

1. 告知用户：「该方法/库已被标记为废弃，不建议使用」
2. 尝试换不同的 `query` 重新查询是否有替代实现
3. 若仍找不到更好的方案，**主动反问用户**：

> 「该实现已被标记为 Deprecated，我暂未找到明确的替代方案。你是否知道应该用哪个库或方法来替代？」

### 规则 2：遇到多个同类实现，必须先让用户选择

若返回多个名称相近、功能相同但版本不同的实现（例如 `RedisLocker` 与 `RedisLockerV2`），**不得自行选择**，必须：

1. 列出所有候选实现及其差异
2. **明确反问用户**选择哪一个：

> 「我找到以下几个实现，功能相似但版本不同，请选择你希望使用的：
> 1. RedisLocker — 旧版实现
> 2. RedisLockerV2 — 新版实现（推荐）
> 3. 以上都不合适，我来描述需求」

3. 用户选择「其他」时，请用户补充描述，再重新查询

### 规则 3：其他不确定情况，反问而非猜测

若 API 签名存在歧义、参数含义不明确、或文档中存在多条可能匹配的路径，**不要猜**，向用户确认：

> 「我在文档中找到了 X 和 Y 两种用法，不确定哪个适合你的场景，能描述一下具体需求吗？」

**原则：有疑问就问，不要用猜出来的代码糊弄用户。**

---

## 注意事项

- `libraryId` 优先使用 `search-libraries` / `discover-libraries` 返回的 `url` 字段
- `get-library-context` 返回纯文本，直接注入对话即可，无需再解析
- 若 `get-library-context` 返回 *"No matching documentation found"*，尝试换更简洁的 `query` 或改用 `get-library-summary`
- API Key 由 MCP 连接时自动携带，无需手动处理鉴权
