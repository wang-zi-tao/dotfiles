# AGENTS.md
- **MUST** 如果代码文件与记忆中的代码不一致, 有可能是因为用户手动修改了文件. 
    - 请重新读取文件内容. 
    - 不要用记忆中的文件内容覆盖实际文件内容.
    - 必要时询问用户

## 快速了解架构（按优先级从高到低）
### 1. 最高优先: AGENTS.md和README.md
### 2. 高优先: 笔记库
- 先读 `~/Documents/Obsidian-work/wiki/index.md`，了解笔记库结构
- 通过笔记库快速了解代码结构和关键模块
- 通过其他方式了解到的代码架构也和关键信息记录进笔记库

### 3. 中优先: explore agent
- 调用explore agent来探索代码结构

## 代码搜索方法（按优先级从高到低）
- 如果不确定类型名或不确定文件路径, 请用子agent去按照以下方法搜索代码

### 1. 最高优先：LSP（dsh-lsp 插件）
- **`explore`（推荐，聚合查询）**: 一次调用同时返回 definition + typeDefinition + hover + implementation + references，效果等同 codegraph 的单符号多信息查询；需 `line` + `character`
- **`workspaceSymbol`**: 全工作区搜符号，支持只给符号名一部分的模糊/子串匹配；参数 `filePath` + `query`（`line`/`character` 可省略）
  - **重要**: 查 C++ 符号时 `filePath` 优先用 `.cpp`（编译单元在 compile_commands.json 中，命中更快更全）；`.h` 也能工作但依赖后台索引预热，命中略慢

### 2. 高优先：hindsight 搜索记忆
- 搜索过往会话和记忆中存储的信息
- 使用`hindsight_recall({query:"..."})`搜索记忆
- 使用`hindsight_reflect({query:"..."})`进行复杂查询

### 3. 中优先：VectorCode
- 适合语义搜索，找到功能相似的代码
- 使用`tools["mcp__vectorcode-mcp-server__query"]({n_query:4,query_messages:["关键词1","关键词2"]})`通过多个关键词按 API 含义模糊搜索
- 不需要使用vectorcode-mcp-server_ls来获取vectorcode工作状态. 这个一工具调用结果通常是错误的

### 4. 中优先：explore agent
- 使用 explore agent 进行代码库探索
- 适合需要深入搜索的场景

### 5. 中优先：codegraph
- 搜索具体函数和调用关系
- 使用`mcp__codegraph__codegraph_node({symbol:"符号名"})`查询单个符号

### 6. 低优先：笔记库
- 快速了解模块，搜索笔记库中记录的关键模块 UML 图

### 7. 最后手段：grep / astgrep
- **尽量不用** grep 或者其他等价命令 搜索整个仓库，效率低
- **尽量** 使用grep时指定目录范围
- **绝对不用** 等价于grep工具的其他命令, 例如findstr和powershell的select-string命令, 性能比grep低

## 笔记库
- **SHOULD** 使用obsidian mcp访问笔记库, 笔记库在`~/Documents/Obsidian-work/`下
- 笔记库中有大量的代码关系, 关键调用栈, uml图, 以及代码分析结果等信息, 可以帮助快速理解和导航代码。

## 并行工具调用
- **SHOULD** 根据任务使用并行式工具调用. 如果确定性不够, 并发度控制在6以下. 
- **MUST** 如果工具列表中有`run_code`, 且需要并行工具调用, 使用`run_code`内的`Promise.all()`并发执行工具函数

## SUB AGENT
- 把任务拆分成子任务, 把子任务交给子agetn处理
- 用AgentTeam 来处理复杂任务.

### 子agent类型
- explorer: 适合探索代码库
- planner: 适合规划任务
- implementer: 适合编写代码
- reviewer: 适合审查代码
- debugger: 适合调试代码
- questioner: 适合提问
- librarian: 适合查找文档和笔记
