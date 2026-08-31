# 笔记库目录分类规则

## 分类决策流程图

```plantuml
@startuml
skinparam backgroundColor transparent
skinparam defaultFontName Microsoft YaHei
skinparam defaultFontSize 12

start

:研究/分析结果;

if (是否与 WPS 源码相关？) then (是)
  if (具体关联？) then (API/JSAPI/加载项)
    :wps/api/<目标>.md;
  elseif (Shell/应用框架)
    :wps/shell/<目标>.md;
  elseif (核心库/基础组件)
    :wps/核心库/<目标>.md;
  elseif (Office 组件: 文字/表格/演示/PDF)
    if (I/O / 文件格式？) then (是)
      :wps/office/io/<目标>.md;
    elseif (渲染/绘制？) then (是)
      :wps/office/渲染/<目标>.md;
    else (其他)
      :wps/office/<目标>.md;
    endif
  elseif (Qt / UI 框架)
    :wps/qt/<目标>.md;
  elseif (编译/仓库/工具)
    :wps/<专用笔记>.md;
    note right
      如: 编译命令.md
      仓库.md, 工具脚本.md
    end note
  elseif (Bug 分析)
    :bugs/<bug标题>.md;
  elseif (坑点/踩坑)
    :wps/坑.md (追加);
  else (其他 WPS 相关)
    :wps/<目标>.md;
  endif

elseif (是否编程语言/运行时相关？) then (是)
  if (V8 / JavaScript 引擎？) then (是)
    :编程语言技术/虚拟机/V8/<目标>.md;
  elseif (Rust 相关？)
    :编程语言技术/Rust/<目标>.md;
  elseif (FFI / NAPI？)
    :编程语言技术/ffi/<目标>.md;
  elseif (node / deno 运行时？)
    :编程语言技术/运行时组件/<目标>.md;
  else (其他语言技术)
    :编程语言技术/<目标>.md;
  endif

elseif (是否跨平台/OS 相关？) then (是)
  if (鸿蒙/OHOS？) then (是)
    :跨平台编程/ohos/<目标>.md;
  elseif (macOS？)
    :跨平台编程/macos.md (追加);
  elseif (aarch64/ARM？)
    :跨平台编程/aarch64.md (追加);
  else (其他平台)
    :跨平台编程/<目标>.md;
  endif

elseif (是否 Office 通用概念？) then (是)
  :Office/<目标>.md;

elseif (是否命令行工具？) then (是)
  :命令行/<目标>.md;

elseif (是否想法/实验？) then (是)
  if (WPS 相关实验？) then (是)
    :wps/小实验/<目标>.md;
  else (通用想法)
    :想法/<目标>.md;
  endif

else (以上都不是)
  :根目录 <主题>.md;
  note right
    或有子目录时在子目录下
  endif

stop

@enduml
```

## 笔记库顶层结构图

```plantuml
@startuml
skinparam backgroundColor transparent
skinparam packageBorderColor #333333
skinparam packageFontName Microsoft YaHei

package "Obsidian-work/" {
  [AGENTS.md] as agents #lightgray

  package "wps/" as wps #E3F2FD {
    [wps.md] as wps_overview
    [WPS-core.md] as wps_core
    [模块.md] as wps_mod
    [坑.md] as wps_pit
    [编译命令.md] as wps_build
    [仓库.md] as wps_repo
    [工具脚本.md] as wps_scripts
    [三方库.md] as wps_3rd
    [官网.md] as wps_official
    [事项.md] as wps_todo

    package "api/" as wps_api {
      [jsapi.md]
      [ts-idl.md]
      [SAP.md]
      [宏录制.md]
      package "jside/" { }
      package "WPS加载项/" { }
    }
    package "shell/" as wps_shell {
      [kshell.md]
      [Application.md]
      [Command.md]
      [触控.md]
    }
    package "核心库/" as wps_corelib {
      [核心库模块.md]
      [核心库类型.md]
      [IO.md]
    }
    package "office/" as wps_office {
      [表格结构.md]
      [数据层.md]
      [属性包.md]
      [事务机制.md]
      [复制粘贴.md]
      [MVC分层.md]
      package "io/" { }
      package "渲染/" { }
    }
    package "qt/" as wps_qt { }
    package "框架/" as wps_framework { }
    package "模块/" as wps_modules { }
    package "生命周期关系/" as wps_lifecycle { }
    package "动态插桩/" as wps_instrument { }
    package "小实验/" as wps_experiment { }
  }

  package "编程语言技术/" as prog #E8F5E9 {
    [编程语言技术.md] as prog_overview
    [类型系统.md]
    [JavaScript.md]
    [lua.md]
    package "虚拟机/V8/" as v8 { }
    package "Rust/" as rust { }
    package "ffi/" as ffi { }
    package "运行时组件/" as runtime { }
  }

  package "跨平台编程/" as xplat #FFF3E0 {
    package "ohos/" as ohos { }
    [macos.md]
    [aarch64.md]
  }

  package "bugs/" as bugs #FFEBEE { }
  package "Office/" as office_dir #F3E5F5 { }
  package "命令行/" as cli #ECEFF1 { }
  package "想法/" as ideas #FCE4EC { }
  package "wiki/" as wiki #FFF9C4 {
    [index.md]
    [tags.md]
    [concepts.md]
    package "concepts/" { }
  }
}

@enduml
```

## 分类规则详解

### wps/ 子目录

| 子目录 | 内容 | 判别关键词 |
|--------|------|------------|
| `api/` | JSAPI、VBA 加载项、宏、CEF、IDL | jsapi、加载项、com、vba、宏、cef、idl |
| `shell/` | Shell 框架、命令系统、触控 | kshell、command、触控、ribbon、窗口管理 |
| `核心库/` | 基础库、IO 层、类型系统 | 核心库、IO、类型、vector、string、thread |
| `office/` | 文档引擎：文字/表格/演示/PDF | et、wps、wpp、pdf、表格、文字、演示 |
| `office/io/` | 文件格式读写 | fork、ole、复合文档、doc、xls |
| `office/渲染/` | 绘制引擎 | 绘制、render、paint、canvas |
| `qt/` | Qt 框架集成 | QWidget、QTextLayout、QPainter、signal/slot |
| `框架/` | KIPC、通用框架 | kipc、framework、module |
| `模块/` | 具体 bundle 模块深度分析 | bundle、module、cmake |
| `生命周期关系/` | 对象生命周期 | 生命周期、init、destroy、create |
| `动态插桩/` | 插桩技术 | discript、hook、instrument |
| `小实验/` | 代码实验、原型验证 | 实验、test、验证、原型 |
| (根) | 全局性文档 | wps.md、编译命令、仓库、三方库 |

### 编程语言技术/ 子目录

| 子目录 | 内容 | 判别关键词 |
|--------|------|-----------|
| `虚拟机/V8/` | V8 引擎深度分析 | v8、isolate、context、handle、gc、turbofan、ignition、maglev |
| `Rust/` | Rust 相关 | rust、cargo、borrow、trait |
| `ffi/` | 跨语言调用 | napi、ffi、node-addon、cgo |
| `运行时组件/` | Node/Deno 等运行时 | node、deno、bun、event loop |

### 跨平台编程/ 子目录

| 子目录 | 内容 | 判别关键词 |
|--------|------|-----------|
| `ohos/` | 鸿蒙 OS 开发 | ohos、harmonyos、arkui、hdc、ability |
| (根) `macos.md` | macOS 相关 | macos、cocoa、appkit、xcode |
| (根) `aarch64.md` | ARM64 相关 | aarch64、arm64、neon、apple silicon |

### 其他目录

| 目录 | 内容 | 判别关键词 |
|------|------|------------|
| `bugs/` | Bug 分析与排查 | bug、crash、崩溃、dump |
| `Office/` | Office 通用概念 | office、文档格式、通用规范 |
| `命令行/` | CLI 工具 | git、shell、bash、cmd |
| `想法/` | 思路、设计讨论 | 想法、设计、思路、可否 |
| `wiki/` | Code-LLM-Wiki 索引 | 索引、concept、index、tags |

## 追加 vs 新建

| 场景 | 操作 |
|------|------|
| 内容与已有笔记强相关 | 追加到已有笔记的对应 heading |
| 新主题/新模块 | 新建 `.md` 文件 |
| 坑点记录 | 追加到 `wps/坑.md` |
| macOS 相关 | 追加到 `跨平台编程/macos.md` |
| aarch64 相关 | 追加到 `跨平台编程/aarch64.md` |
| 编译命令 | 追加到 `wps/编译命令.md` |
| 工具脚本 | 追加到 `wps/工具脚本.md` |

## 多级子目录选择

当内容可以归入多级子目录时，选择最具体的：
- 优先级：深度目录 > 浅层目录 > 根目录
- 例：V8 垃圾回收 → `编程语言技术/虚拟机/V8/`，而非 `编程语言技术/`
- 例：ET 表格渲染 → `wps/office/渲染/`，而非 `wps/office/`
