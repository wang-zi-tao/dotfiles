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
- 已知精确名字, 但名字过于通用, 同名符号过多: lsp_symbols, lsp_references
- 已知精确位置，但是是代码重的隐藏信息（例如推导的类型）：lsp_type_definition
- 已知部分名字, 且名字不够通用 => lsp_symbols 和 grep(特定目录内搜索)
- 已知部分名字和代码目录 => lsp_symbols 和 grep(特定目录内搜索)
- 宏和其他特殊语法干扰grep搜索 => lsp工具
- 已知位置的代码符号的使用点 => lsp_references
- 已知位置的虚函数或者接口的实现点 => lsp_implementation
- 未知精确名字 => hindsight 和 VectorCode
- 曾经考察过代码模块架构 => hindsight 和 Obsidian
- 未考察过的代码模块架构 => 子代理考察模块结构, 最终写入笔记库和记忆
- 兜底: grep(全工作空间搜索较慢)

# 代码检查
- 优先使用 `lsp_diagnostics` 诊断代码问题（`severity` 可按最低级别过滤）
- 最终需要编译

# 笔记库
- **SHOULD** 使用obsidian mcp访问笔记库, 笔记库在`~/Documents/Obsidian-work/`下
- 笔记库中有大量的代码关系, 关键调用栈, uml图, 以及代码分析结果等信息, 可以帮助快速理解和导航代码。

# 并行工具调用
- **SHOULD** 根据任务使用并行式工具调用. 
- **MUST** 避免非常多步骤数的串行式调用工具
- **MUST** 如果使用`run_code`, 且需要并行工具调用, 使用`run_code`内的`Promise.all()`并发执行工具函数

# 临时文件管理
- 项目的临时文件根目录: .temp
- 项目的临时文件要根据任务来分类, 存入不同文件夹中. 
- 文件夹中添加README.md, 记录临时文件时间和任务信息. 复用临时文件时可以先grep搜索.temp下的所有README.md
- 文本类型的临时文件中, 最好写入作用,时间, 是否过时等信息

# shell
- 脚本文件: 推荐使用python/nodejs脚本文件, 代替shell脚本
- 数据分析命令: 推荐使用nushell. 避免使用powershell

> [!IMPORTANT]
> 请用nushell语法调用bash工具和pwsh工具

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
- 重构代码, 导致代码模块结构发生较大变化

## 常用知识类经验
- 高频率出现的代码，包括常用类型和常用函数
    - 用一个知识页存储. 或者建立一个skill, 或者在笔记库中新增一个markdown文件
    - 知识页描述或者skill描述中要包含功能和使用场景

## 经验存储
- 方法类和工作流程经验 => `hindsight_capture_initiative`生成知识页
- 关于代码中的常用知识 =>  `hindsight_capture_initiative`生成知识页
- 事实类和经历经验 =>  `hindsight_retain` 存入记忆
- 如果经验总结内容过于复杂 => 存入笔记库
- 如果经验内容复杂, 且其信息分散在记忆库中 => `hindsight_capture_initiative`生成知识页
- 如果本机skill或者AGENTS.md的信息与事实不符 =>  可以进行修改skill或者AGENTS.md
- 如果是本机dsh插件(dsh-hindsight/dsh-lsp/dsh-neovim)或者脚本程序有问题,  => 用户同意后进行修复

# 用户
## 简介
- 资深C++开发工程师
- 精通Rust语言
- 熟悉haskell/java/typescript/python/lua
- 熟悉多种编程语言
## 编程偏好
- 静态强类型
- 写动态脚本语言也时也倾向于用静态类型, 比如TypeScript
- 喜欢函数式编程风格
- 优先复用现有代码和库
- 异步优先
- 不抛弃报错信息

### Rust编程偏好
- 喜欢零成本抽象
- 不过度使用智能指针
- 不用RefCell
- 使用过程宏和声明宏
- 避免出现无效状态

### C++编程偏好
- 控制修改的影响面
- 优先用智能指针
- 优先用现代化C++写法
- 移动语言优先
