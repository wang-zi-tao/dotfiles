# AGENTS.md
- **MUST** 如果代码文件与记忆中的代码不一致, 有可能是因为用户手动修改了文件. 
    - 请重新读取文件内容. 
    - 不要用记忆中的文件内容覆盖实际文件内容.
    - 必要时询问用户

- **MUST** thingking in chinese

# 快速了解架构（按优先级从高到低）
### 1. 最高优先: AGENTS.md和README.md
### 2. 高优先: 笔记库
- 先读 `~/Documents/Obsidian-work/wiki/index.md`，了解笔记库结构
- 通过笔记库快速了解代码结构和关键模块
- 通过其他方式了解到的代码架构也和关键信息记录进笔记库

### 3. 中优先: explore agent
- 调用explore agent来探索代码结构

# 代码搜索方法（按优先级从高到低）
- 已知精确名字, 但名字过于通用, 同名符号过多: LSP
- 已知部分名字: LSP 和 codegraph
- 已知部分名字和代码目录: LSP 和 codegraph 和 grep
- 已知位置的代码符号的使用点: LSP
- 已知位置的虚函数或者接口的实现点: LSP
- 未知精确名字: hindsight 和 VectorCode
- 曾经考察过代码模块架构: hindsight 和 Obsidian
- 未考察过的代码模块架构: 子代理考察模块结构, 最终写入笔记库和记忆
- 兜底: grep

### 1. 最高优先：LSP（dsh-lsp 插件，工具名 `lsp_*`）
- **`lsp_symbols`**: `scope:"workspace"` 全工作区搜符号，支持只给符号名一部分的模糊/子串匹配；参数 `filePath` + `query`（`line`/`character` 可省略）；`scope:"document"` 取当前文件大纲. 速度远快于全局grep查找, 不会搜索出无关结果
- **`lsp_explore`（推荐，聚合查询）**: 一次调用同时返回 definition + typeDefinition + hover + implementation + references，效果等同 codegraph 的单符号多信息查询；需 `filePath` + `line` + `character`
- 单点查询用 `lsp_definition` / `lsp_type_definition` / `lsp_implementation` / `lsp_references` / `lsp_hover`（都需 `filePath` + `line` + `character`）；`lsp_status` 只看 server 状态（只读，不会启动新进程）
  - **重要**: 查 C++ 符号时 `filePath` 优先用 `.cpp`（编译单元在 compile_commands.json 中，命中更快更全）；`.h` 也能工作但依赖后台索引预热，命中略慢

### 2. 最高优先：codegraph 代码静态索引
- 搜索类型和函数的速度最快
- 搜索具体函数和调用关系
- 使用`mcp__codegraph__codegraph_node({symbol:"符号名"})`查询单个符号

### 3. 高优先：hindsight 搜索记忆
- 搜索过往会话和记忆中存储的信息
- 使用`hindsight_recall({query:"..."})`搜索记忆
- 使用`hindsight_reflect({query:"..."})`进行复杂查询

### 4. 中优先：obsidian 笔记库
- 快速了解模块，搜索笔记库中记录的关键模块 UML 图

### 5. 中优先：VectorCode 代码rag
- 适合语义搜索，找到功能相似的代码
- 使用`tools["mcp__vectorcode-mcp-server__query"]({n_query:4,query_messages:["关键词1","关键词2"]})`通过多个关键词按 API 含义模糊搜索
- 不需要使用vectorcode-mcp-server_ls来获取vectorcode工作状态. 这个一工具调用结果通常是错误的

### 6. 中优先：explore agent
- 使用 explore agent 进行代码库探索
- 适合需要深入搜索的场景

### 7. 最后手段：grep / astgrep
- **尽量不用** grep 搜索整个仓库. wps仓库尺寸超过10GB, grep搜索效率低, 且可能会搜索出大量无关结果.
- **尽量** 使用grep时指定目录范围
- **绝对不用** 等价于grep工具的其他命令, 例如findstr和powershell的select-string命令, 性能比grep低

# 代码检查
- 优先使用 `lsp_diagnostics` 诊断代码问题（`severity` 可按最低级别过滤）
- 最终需要编译

# 笔记库
- **SHOULD** 使用obsidian mcp访问笔记库, 笔记库在`~/Documents/Obsidian-work/`下
- 笔记库中有大量的代码关系, 关键调用栈, uml图, 以及代码分析结果等信息, 可以帮助快速理解和导航代码。

# 并行工具调用
- **SHOULD** 根据任务使用并行式工具调用. 如果确定性不够, 并发度控制在6以下. 
- **MUST** 如果工具列表中有`run_code`, 且需要并行工具调用, 使用`run_code`内的`Promise.all()`并发执行工具函数

# 临时文件管理
- 项目的临时文件根目录: .temp
- 项目的临时文件要根据任务来分类, 存入不同文件夹中. 
- 文件夹中添加README.md, 记录临时文件时间和任务信息. 复用临时文件时可以先grep搜索.temp下的所有README.md
- 文本类型的临时文件中, 最好写入作用,时间, 是否过时等信息

# shell
- 脚本文件: 推荐使用python/nodejs脚本文件, 代替shell脚本
- 数据分析命令: 推荐使用nushell. 避免使用powershell
- 日常终端命令: nushell > bash >> cmd > powershell
- 脚本文件中, 尽量不要重定向输出

# 经验总结
**MUST** 每次执行任务后, 总结任务中的失败原因和成功经验, 并存储经验

## 失败经验类型
- 工具或命令多次(>=3)调用失败
- 任务多次(>=2)被要求返工
- 事实与常识不符合
- 事实与记忆不一致

## 成功经验类型
- 跑通了一个复杂的工作流
- 完成了复杂的研究任务
- 重构代码, 让笔记或记忆中的代码结构发生变化

## 经验存储
- 方法类经验适合使用`harness_refine`, 总结成skill
- 事实类和经历经验适合 `hindsight_retain` 存入记忆
- 如果经验总结内容过于复杂, 存入笔记库
- 如果经验内容复杂, 且其信息分散在记忆库中, 则建立一个hindsight 知识页, 来方便检索
- 如果本机skill或者AGENTS.md的信息与事实不符, 可以进行修改
- 如果是本机dsh插件(dsh-hindsight/dsh-lsp/dsh-neovim)或者脚本程序有问题, 可以在用户同意后进行修复
