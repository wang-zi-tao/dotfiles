---
name: wps-cmake-module-analyzer
description: >
  分析 wpsmain 仓库中代码，提取 C++ 类型依赖、生命周期与模块能力， 并创建或更新对应模块笔记。 分析代码后, 使用本skill来更新笔记库, 以便长期沉淀模块知识.
---

# Skill 概述

本 skill 面向本项目代码仓库 `D:\branch-master\wpsmain` 与 Obsidian 笔记库
`~/Documents/Obsidian-work`。核心目标：

- 如果要求分析模块, 定位并解析 CMake 模块（bundle/子模块），基于 `CMakeLists.txt` 推断模块边界与依赖。
- 如果要求分析类型, 定位模块, 并在模块范围内分析 C++ 类型的继承、接口实现与依赖关系。
- 扫描模块目录下的头/源文件，分析 C++ 类型的继承关系、接口实现以及依赖/所有权关系。
- 通过关键源文件追踪类型生命周期与典型调用流程。
- 按 Obsidian `AGENTS.md` 中的“代码模块相关笔记”模板，将分析结果写入或更新模块笔记。

触发时机示例：

- “分析 Coding/api_bundle/api/jsapi/v8proxy 这个模块，并把结果写到笔记库里”。
- “分析 KxJdeMainWindow 这个类型，并把结果写到笔记库里”。
- “总结某个 CMake 模块的类型结构和生命周期，按模块模板生成 Obsidian 文档”。
- “我想了解某个模块的核心类型和调用流程，帮我分析代码并更新笔记库”。
- "补全笔记 cefipc.md 中对相关类型的分析 "
- "补全笔记 cefipc.md 中对初始化流程的分析 "

> 约束：
> - 遵守仓库 `AGENTS.md` 约定：**优先使用 LSP / AST-grep** 进行代码搜索与分析，避免无目标的大范围 grep。
> - 笔记库根目录固定为 `~/Documents/Obsidian-work`，WPS 相关笔记在 `wps/` 目录下。

---

## 目录与命名约定

### 代码仓库

- 仓库根：`D:\branch-master\wpsmain`
- CMake 根入口：`Coding/CMakeLists.txt`
- bundle 层级结构与说明：参考仓库根目录 `AGENTS.md` 中的 “wpsmain项目目录结构” 与 “Bundle层级架构”。

模块（module / 子模块）定义：

- 至少包含一个 `CMakeLists.txt`，内部使用 `wps_package(...)` 或 `add_library(...)` 等声明目标；
- 配套头/源文件通常在同级或子目录（如 `include/`, `src/`, `office/...`）。

### 笔记库

- 笔记库根：`~/Documents/Obsidian-work`
- WPS 相关笔记：`~/Documents/Obsidian-work/wps/`

模块级笔记推荐路径：

```text
wps/模块/<bundle-name>/<module-name>.md   # 推荐
# 若 bundle 不易确定，可退化为：
wps/模块/<module-name>.md
```

其中：

- `bundle-name`：如 `core_bundle`, `api_bundle` 等；
- `module-name`：默认取 CMake 模块名（如 `jde`, `v8proxy`），可首字母大写。

---

## 笔记模板与 AI 区块

模块文档整体结构遵循 Obsidian `AGENTS.md` 中的“代码模块相关笔记”模板：

```md
# 模块名
- cmake所在目录路径
- 模块的功能简介
## 能力
模块的能力, 作用, 适用场景等
## 依赖关系
模块的依赖关系, 包括依赖的模块, 被哪些模块依赖等, 包括动态链接和静态链接
# 模块细节
## 类型
用plantuml画出模块的类图，主要包含类型间的继承, 依赖, 所有权等关系. 

> [> [!IMPORTANT]
> 由于类型太多, 不生成每个类型的字段和方法. 
> 要区分class和interface和abstract class
> 需要画出接口实现关系, 以及所有权关系(聚合/组合/弱引用)
> 需要画Qt信号槽绑定关系

#### TypeClass (具体的类型)
类型的作用, 字段, 依赖关系, 继承关系等
##### TypeClass.Method (具体的方法)
方法的作用, 参数, 返回值, 依赖关系等

## 流程

### 流程名
用plantuml时序图说明模块的关键执行流程, 以及各类型关系, 各线程关系
#### 调用栈
用代码块说明关键的调用栈, 以及各函数的作用, 参数, 返回值等
```

为便于多次运行本 skill 时安全更新，约定在文件中为 AI 生成内容包裹以下标记区块：

```md
<!-- AI:MODULE-OVERVIEW START -->
... 这里写入/更新模块概览、能力和依赖关系 ...
<!-- AI:MODULE-OVERVIEW END -->

<!-- AI:MODULE-TYPES START -->
... 这里写入/更新类型关系与 PlantUML 类图 ...
<!-- AI:MODULE-TYPES END -->

<!-- AI:MODULE-FLOWS START -->
... 这里写入/更新关键流程、时序图与调用栈 ...
<!-- AI:MODULE-FLOWS END -->
```

后续再次运行该 skill 时，只替换标记内的内容，保留标记外的用户手写内容与结构。

---

## 工作流程

整体分为四个阶段：

1. 定位模块 CMake 目录
2. 扫描头文件并分析类型关系
3. 结合源文件分析生命周期与关键流程
4. 创建或更新 Obsidian 模块笔记
5. git提交笔记库修改

### 阶段一：定位模块 CMake 目录

1. 根据用户输入确定起点：
   - 若给出 CMakeLists 路径：以文件所在目录作为模块根；
   - 若给出模块名/目标名：在 `Coding/**/CMakeLists.txt` 中匹配：
     - `wps_package($NAME $$$)`
     - 或 `add_library($TARGET $$$)` / `add_executable($TARGET $$$)` 作为兜底。

2. 使用 AST-grep / LSP（而非裸 grep）在 CMake 中解析：
   - 提取模块名 `$NAME` / `$TARGET`；
   - 记录 CMake 文件路径；
   - 根据路径前缀推断所属 bundle（如 `Coding/core_bundle/...` → `core_bundle`）。

3. 在该 `CMakeLists.txt` 中分析依赖：
   - 查找 `target_link_libraries(<module> ...)`；
   - 分析 WPS 宏（如 `wps_bundle_export_packages` 等）；
   - 得到“依赖的模块”列表；若能找到反向引用则记录“被哪些模块依赖”。

### 阶段二：扫描头文件并分析类型关系

1. 确定扫描范围：
   - 从模块根目录出发，优先遍历 `include/`, `inc/`, 与模块名相近的子目录；
   - 仅考虑 `*.h`, `*.hpp`, `*.hh` 等 C++ 头文件。

2. 使用 LSP 与 AST-grep 提取类型(注意有的类型无基类, 有的类型继承多个基类)：
   - `class $CLASS`
   - `struct $STRUCT`

3. 记录继承与接口实现：
   - 对每个类型记录：类型名、所在文件、基类列表；
   - 对没有字段且只有虚函数的抽象类型标记为“接口”；
   - 对继承自接口的具体类型标记为“接口实现”。

4. 分析依赖与所有权：
   - 在头文件成员字段中识别：
     - 原始指针：`Type*` / `const Type*`（弱引用）；
     - 智能指针：`std::unique_ptr`, `std::shared_ptr`, `QSharedPointer`；
     - Qt 父子关系：`QObject` 派生类中的 `parent` / `children` 模式；
     - 容器持有：`std::vector<Type>`, `QVector<Type>` 等。
     - com引用计数: `ks_stdptr<Type>` 等.
     - 通过Variant存储: `QVariant`, `VARIANT`, `KComVariant`, `KJSVariant`
   - 归纳为：聚合/组合、所有权、弱引用三类关系。

5. 分析qt信号槽绑定关系:
   - 在类定义中查找 `signals:` 和 `slots:` 块，记录信号与槽的名称和参数；
   - 在源文件中使用 LSP 查找 `connect(...)` 调用，分析信号与槽的绑定关系。

### 阶段三：结合源文件分析生命周期与关键流程

1. 选择重点类型：
   - 暴露在公共接口中的类型（头文件在 `include/` 等公共路径）；
   - 继承层级深或实现接口多的核心类型；
   - 名称包含 `Application`, `Manager`, `Engine` 等的管理类。

2. 分析生命周期：
   - 在对应 `*.cpp` 中使用 AST-grep / LSP：
     - 查找构造函数、析构函数；
     - 查找 `init`, `uninit`, `startup`, `shutdown` 等方法实现；
   - 记录：
     - 对象何时创建，在哪个线程/上下文创建；
     - 使用哪些 RAII / 智能指针 / Qt 父子关系管理资源；
     - 何时释放或由谁负责销毁。

3. 提炼关键流程：
   - 使用 LSP `findReferences` / `prepareCallHierarchy` / `incomingCalls`：
     - 从对外入口函数（接口方法、导出 API）出发，追踪调用链；
   - 选择 1–3 条“典型流程”（如模块初始化、文档打开、请求处理）；
   - 为每条流程准备：
     - 参与类型列表；
     - 粗略的线程/任务信息（如主线程 vs 工作线程）；
     - 便于转写为时序图的步骤说明。

### 阶段四：创建或更新 Obsidian 模块笔记

1. 决定笔记路径：
   - 已知 bundle 时：`wps/模块/<bundle-name>/<module-name>.md`；
   - 否则：`wps/模块/<module-name>.md`。

2. 读取或创建文件：
   - 通过 Obsidian MCP 在 `~/Documents/Obsidian-work` 下：
     - 若文件存在：读取全文；
     - 若文件不存在：新建并按模板写入基础骨架（标题、cmake路径、空的各节）。

3. 填充/更新内容（注意保留手写部分）：

   - 顶部信息（非 AI 区块，可手动编辑）：
     - `# 模块名`：使用 `<module-name>`；
     - `- cmake所在目录路径`：如 ``Coding/api_bundle/api/jsapi/v8proxy/``；
     - `- 模块的功能简介`：用 2–3 句中文概括模块职责和所在 bundle。

   - `<!-- AI:MODULE-OVERVIEW -->` 区块：
     - `## 能力`：根据 CMake 与代码，总结模块的能力、典型使用场景、与其他 bundle 的交互；
     - `## 依赖关系`：列出依赖的模块与被依赖模块，可附简单 plantuml 依赖图。

   - `<!-- AI:MODULE-TYPES -->` 区块：
     - `# 模块细节`
     - `## 类型`：生成 PlantUML 类图代码块（可简化），展示主要类型的继承/实现与所有权关系；
     - 为每个核心类型写：
       - `#### TypeClass`：描述用途、关键字段和依赖；
       - `##### TypeClass.Method`：对关键方法说明作用、参数和返回值。

   - `<!-- AI:MODULE-FLOWS -->` 区块：
     - `## 流程`：为每个典型流程生成：
       - `### 流程名`：如“模块初始化流程”；
       - PlantUML 时序图伪代码块，强调线程/任务边界（如 main thread / worker）；
       - `#### 调用栈`：用代码块列出关键调用栈，并对每个函数做一句话说明。

4. 更新策略：
   - 若文档中已存在相应 `<!-- AI:... START/END -->` 标记：
     - 仅在块内部添加内容，不修改其他部分. 注意标记内部可能被手工添加过内容, 需要尽量保留手工添加的内容；
   - 若不存在标记：
     - 在模板合适位置插入三段 AI 区块，后续运行时保持区块稳定。

### 阶段五：git提交笔记
笔记库更新后, git提交笔记库修改. 

---

## 工具与代理使用建议

实现本 skill 时，推荐优先使用：

- **代码结构/模式搜索**：
  - `ast_grep_search`：匹配 CMake 模块声明、类定义/继承与特定生命周期函数模式；
- **代码导航（LSP）**：
  - `lsp_symbols`（document/workspace）：列出文件/项目中的类型与符号；
  - `lsp_goto_definition` 与 `lsp_find_references`：追踪类型定义和调用关系；
  - `prepareCallHierarchy` / `incomingCalls` / `outgoingCalls`：分析调用链与流程；
- **笔记读写**：
  - 通过 Obsidian MCP 操作 `~/Documents/Obsidian-work` 下的 `.md` 文件，遵守现有目录结构。

对于复杂模块：

- 使用 `explore` 子代理在仓库内搜索同类模块与现有笔记，复用已有模式；
- 使用 `librarian` 子代理在外部仓库或官方文档中寻找 CMake/Qt/C++ 模块化实践以辅助判断。

> 重要：一旦某次分析已委托给 `explore`/`librarian` 子代理，不要在主会话中对**同一搜索目标**重复 grep；
> 应等待子代理结果并综合使用，以避免重复工作和上下文浪费。

---

## 总结

本 skill 将“CMake 模块/C++类型 → C++ 类型与生命周期分析 → 标准化模块笔记”串成一条固定流程：

- 通过 CMake 与目录结构精确定位模块；
- 基于 LSP 与 AST-grep 提取类型、继承、接口实现与所有权关系；
- 从关键源文件中分析生命周期与典型调用流程；
- 按 Obsidian `AGENTS.md` 模板生成/更新模块笔记，并使用 AI 区块保证多次运行的可维护性。

在遇到需要系统性理解某个模块时，优先使用本 skill，将一次分析沉淀为长期可复用的知识。
