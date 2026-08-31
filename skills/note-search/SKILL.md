---
name: note-search
description: "在 Obsidian 笔记库中查找笔记。根据关键词、标签、文件路径或概念查询笔记内容。"
version: 1.0.0
author: wps
license: MIT
platforms: [windows]
metadata:
  hermes:
    tags: [note-taking, search, obsidian, knowledge-base]
    related_skills: [obsidian]
---

# 笔记查找 (Note Search)

## 概述

在 `C:\Users\wps\Documents\Obsidian-work` 笔记库中查找笔记。提供按关键词搜索、按标签筛选、按路径搜索、以及基于向量相似度的语义搜索。
对应的wps源码位置:
- D:\branch-master\wpsmain\ (master分支)
- D:\branch1\wpsmain\ (功能分支)

笔记库结构：
```
Obsidian-work/
├── wps/               # WPS相关笔记
├── 编程语言技术/       # 编程语言与技术
├── 跨平台编程/         # 跨平台开发
├── c艹/               # C++相关
├── bugs/              # Bug分析
├── Office/            # Office相关
├── 命令行/            # 命令行工具
├── 想法/              # 想法记录
├── AGENTS.md          # 编写笔记前必读
└── wiki/              # Code-LLM-Wiki (索引 + 概念页)
```

## 搜索策略

按优先级使用以下工具：

### 1. 基于 MCP 的 Obsidian 搜索（推荐）

当 Obsidian MCP 服务可用时，使用 `mcp_obsidian_obsidian_search_notes` 进行搜索：

**文本搜索（关键词匹配）：**
```
mcp_obsidian_obsidian_search_notes(
  mode="text",
  query="<关键词>",
  pathPrefix="<可选: wps/>",
  contextLength=100,
  maxMatchesPerHit=5
)
```

**Dataview 查询（元数据筛选）：**
```
mcp_obsidian_obsidian_search_notes(
  mode="dataview",
  query="TABLE where tags contains 'v8'"
)
```

### 2. 基于文件路径的模式匹配

当需要按文件名或路径搜索时：
```
search_files(pattern="*<关键词>*", target="files", path="C:/Users/wps/Documents/Obsidian-work")
```

### 3. 内容 grep 搜索

当需要搜索文件内容中的特定文本时：
```
search_files(
  pattern="<正则表达式>",
  target="content",
  path="C:/Users/wps/Documents/Obsidian-work",
  file_glob="*.md",
  output_mode="content"
)
```

### 4. 基于索引的手动查找

当 MCP 不可用时，直接读取索引文件：
```
read_file(path="C:/Users/wps/Documents/Obsidian-work/wiki/index.md")
```

### 5. 概念页查找

查找特定概念的综合说明：
```
read_file(path="C:/Users/wps/Documents/Obsidian-work/wiki/concepts.md")
```
然后按需读取:
- `wiki/concepts/v8-integration.md`
- `wiki/concepts/cef-ipc.md`
- `wiki/concepts/bundle-architecture.md`
- `wiki/concepts/mojo-ipc.md`
- `wiki/concepts/data-layer.md`
- `wiki/concepts/wps-dev.md`

## 常用搜索场景

### 场景 1: 查找 WPS 某个模块的笔记

```
# 先看索引
read_file(path="C:/Users/wps/Documents/Obsidian-work/wiki/index.md")

# 或者按路径搜索
search_files(pattern="*表格*", target="files", path="C:/Users/wps/Documents/Obsidian-work/wps")
```

### 场景 2: 查找某个 Bug 的分析

```
# Bugs 集中在 bugs/ 目录
search_files(pattern="*V8*", target="files", path="C:/Users/wps/Documents/Obsidian-work/bugs")
```

### 场景 3: 查找某个 V8 概念

```
# 概念页在 wiki/concepts/
read_file(path="C:/Users/wps/Documents/Obsidian-work/wiki/concepts/v8-integration.md")

# 或搜索 V8 相关笔记
search_files(pattern="V8", target="files", path="C:/Users/wps/Documents/Obsidian-work/编程语言技术/虚拟机/V8")
```

### 场景 4: 按标签搜索

```
# 查看所有标签
mcp_obsidian_obsidian_list_tags()

# 或用 dataview 查询
mcp_obsidian_obsidian_search_notes(
  mode="dataview",
  query='TABLE where tags contains "wps-api"'
)
```

### 场景 5: 跨笔记查找关联信息

```
# 1. 找到相关笔记
search_files(pattern="CreateFileMapping", target="content", path="C:/Users/wps/Documents/Obsidian-work")

# 2. 读取找到的笔记内容
read_file(path="C:/Users/wps/Documents/Obsidian-work/bugs/cefipc崩溃问题.md")

# 3. 顺着 wiki 链接打开关联笔记
read_file(path="C:/Users/wps/Documents/Obsidian-work/wps/office/io/fork.md")
```

## 搜索技巧

- **关键词太宽泛**: 使用 `wps/` 路径前缀缩小范围（如 `pathPrefix="wps/office/"`）
- **标签不如预期**: 先用 `obsidian_list_tags` 查看可用的标签列表
- **需要概念综述**: 首选 `wiki/concepts/` 下的概念页，它们已聚合了多篇笔记的信息
- **结果过多**: 增加更具体的关键词或使用正则表达式缩小范围
- **文件未找到**: 先用 `search_files(target="files")` 确认文件是否存在

## 笔记结构参考

### 目录索引 (wiki/index.md)

根索引覆盖所有笔记的分类，是查找的起点。

### 标签列表 (wiki/tags.md)

已注册标签分类：
- `wps-*`: WPS 各模块
- `v8`, `javascript`: V8/JS 引擎
- `c++`, `rust`: 编程语言
- `ohos`, `macos`, `windows`: 跨平台
- `debugging`, `windbg`, `lldb`: 调试技术
- `knowledge-base`, `concept`: Wiki 概念页

### 概念页 (wiki/concepts/)

跨笔记聚合的概念说明：
| 概念页 | 覆盖笔记数 |
|--------|-----------|
| v8-integration | 5 |
| cef-ipc | 3 |
| bundle-architecture | 3 |
| mojo-ipc | 3 |
| data-layer | 6 |
| wps-dev | 15 |

## 故障恢复

当搜索工具返回空结果时：

1. **确认笔记库存在**: `terminal(command="ls /c/Users/wps/Documents/Obsidian-work/")`
2. **检查索引文件**: `read_file(path="C:/Users/wps/Documents/Obsidian-work/wiki/index.md", limit=30)`
3. **直接文件枚举**: `search_files(pattern="*.md", target="files", path="C:/Users/wps/Documents/Obsidian-work", limit=10)`
4. **如果 MCP 服务异常**: 回退到 `search_files` 和 `terminal` + `grep` 组合
