# 业务代码里的真实用法与反例（实测统计）

> 路径相对 `D:\branch2\wpsmain`。统计方式：grep 限定到具体子目录，未改任何文件。

## 1. 包含方式：三条路径并存

| 模块 | 引入姿势 | 证据 |
|---|---|---|
| core/et/pqfunc | `#include <kbase/kglobal_def.h>` → 再由它拉 `kern/*` | `Coding\core_bundle\office\et\pqfunc\stdafx.h:47` |
| core/et/uilogic | 直接 `#include "kern/typedef.h"`（放在 `#ifndef X_OS_WINDOWS` 内） | `Coding\core_bundle\office\et\uilogic\stdafx.h:69` |
| shell/ksolite | `<kfc.h>` + 直接 `#include "kern/typedef.h"` | `Coding\shell_bundle\ksolite\stdafx.h:53` / `:55` |
| api/jsapi/jsapiservice | `<kfc.h>` + `<kern/typedesc.h>` | `Coding\api_bundle\api\jsapi\jsapiservice\stdafx.h:17` / `:27` |

三类姿势的评价：

1. **主流（推荐）**：`#include <kfc.h>` 或 `<kbase/kglobal_def.h>`。`kglobal_def.h:41-85` 会在非 Windows 上拉齐 `kern/typedef.h`、`winnt.h`、`winuser.h`、`wingdi.h`、`objbase.h`。
2. **次主流（可接受）**：模块只缺少数 Win 类型时，在 stdafx.h 的 `#ifndef X_OS_WINDOWS` 里直接引 `kern/typedesc.h` / `kern/typedef.h`，并就地 `typedef void* HWND; typedef INT_PTR WPARAM;` …（`Coding\core_bundle\office\et\uilogic\stdafx.h:66-69`、`Coding\api_bundle\api\jsapi\jsapiservice\stdafx.h:27-33`）。
3. **单点补头（可接受）**：`Coding\core_bundle\office\et\applogic\stdafx.h:74`（`kern/winnls.h`）、`Coding\api_bundle\api\jsapi\jsapirender\stdafx.h:40`（`kern/typedesc.h`）。

⚠️ 直接 include `kern/typedef.h` 有前提：**必须先于任何 Windows SDK 头**，否则 `DWORD` 会重定义（见 ref-gotchas A1）。所以旧代码那种写法只在 `#ifndef X_OS_WINDOWS` 分支里安全。

## 2. 平台条件编译：两套宏体系并存

grep 计数（`*` = 触 250 上限）：

| 目录 | `X_OS_WINDOWS` | `X_OS_LINUX` | `X_OS_DARWIN` | `X_OS_UNIX` | `Q_OS_WIN` |
|---|---|---|---|---|---|
| core/office/et/uilogic | 142 | 8 | 157 | 2 | 18 |
| core/office/et/applogic | 482* | 50 | 115 | 14 | 44 |
| core/office/et/render2 | 8 | 4 | 142 | 1 | 7 |
| api/jsapi/ksojscore | 5 | 8 | 0 | 0 | 65 |
| shell/ksolite | 48 | 19 | 48 | 9 | 624* |

- 写法几乎都是 `#ifdef X_OS_xxx`（不是 `defined()` 组合）。代表点：`Coding\core_bundle\office\et\render2\src\pdf\pdfexport.cpp:321`（WINDOWS）、`...\render2\src\common\tools\draw_line.cpp:42`（DARWIN）、`Coding\api_bundle\api\jsapi\ksojscore\ksojscore.cpp:175`（LINUX）、`Coding\shell_bundle\ksolite\stdafx.cpp:3`（UNIX）。
- Qt 壳层混用两套宏，同一个文件三套并存：`Coding\shell_bundle\ksolite\util\common.cpp:12`（`#ifndef Q_OS_WIN`）、`:32`（`#ifndef X_OS_DARWIN`）、`:1928`（`#ifdef X_OS_UNIX`）。纯 Qt 例子：`Coding\shell_bundle\ksolite\accountsdk\kaccountsdkproxy.cpp:58`。

**规律**：
- **ET 内核（core_bundle）以 `X_OS_*` 为准**，且 `X_OS_DARWIN` 计数常高于 `X_OS_WINDOWS` —— macOS 适配比 Linux 更深。
- **shell / api 的 Qt 侧以 `Q_OS_WIN` 为准** —— 历史遗留，两套体系并行。
- 新代码在非 Qt 模块里用 `X_OS_*`；只有在 Qt 控件/窗口相关代码里才用 `Q_OS_*`。**同一文件里不要混用**。

## 3. CMake：POSIX 上必须显式链 kskern

| 事实 | 证据 |
|---|---|
| kskern 只在 POSIX 构建 | `Coding\foundation_bundle\kfc_base\CMakeLists.txt:3` `wps_add_subdirectory(kskern LINUX BSD DARWIN)` |
| kskern 是 STATIC | `Coding\foundation_bundle\kfc_base\kskern\CMakeLists.txt:25` `wps_package(kskern STATIC)` |
| kbase 是 SHARED，且只对 UNIX 链 kskern | `Coding\foundation_bundle\kbase\CMakeLists.txt:1`、`:12-13` |
| `wps_link_packages` 最终是 `PRIVATE` | `Coding\toolchain\cmake\wps_internal.cmake:1313` → `target_link_libraries(... PRIVATE ...)` |

**PRIVATE 不向消费者传递** → kbase 不能替代，消费方必须自己写 `UNIX(kskern)`。

真实调用点（都是 kbase + kskern 同时链）：

| 文件:行号 | 内容 |
|---|---|
| `Coding\api_bundle\api\jsapi\jsapibrowser\CMakeLists.txt:59-60` | `UNIX(kskern) UNIX(kbase)` |
| `Coding\api_bundle\api\jsapi\jscefservice\ipc\CMakeLists.txt:39-40` | `UNIX(kbase) UNIX(kskern)` |
| `Coding\api_bundle\api\jsapi\asyncjsapi\et\CMakeLists.txt:25-27` | `UNIX(kskern)` + kbase |
| `Coding\api_bundle\api\jsapi\ksojscore\CMakeLists.txt:132-133` | `UNIX(kskern)` kbase |
| `Coding\core_bundle\office\et\applogic\CMakeLists.txt:1094-1096` | `UNIX( kskern )` |

**规则**：目标里只要直接调 `SysAllocString`/`SafeArrayCreate`/`VariantXxx` 等实现符号，就自己写 `UNIX(kskern)`。

## 4. 真实反例（照着这些 grep 能找出同类问题）

| 反例类型 | 证据 | 说明 |
|---|---|---|
| `wcscmp` 直接吃 `WCHAR` | `Coding\core_bundle\office\et\appcore\revision\revisions.cpp:635`；`...\revision\revisionopt.cpp:307`；`Coding\api_bundle\api\jsapi\jside\vba2js\kvba2js\main.cpp:136` | 仅 etch 目录就 **145 处**。POSIX 上 `WCHAR`=`char16_t`（`include\kern\typedef.h:224`），kern/kbase 只给 `char16_t` 提供 `_Xu2_strcmp`/`xstrcmp`（`kbase\string\_\cstring_posix.h:900`、`cstring_posix_u16.h:49`），**没有** `wcscmp` 的 `char16_t` 重载 |
| 用 `long` 表 32 位 | `Coding\core_bundle\office\et\include\io\et_ioacceptor.h:11`（`typedef unsigned long _IOTHEID`，`:12-13` 派生成 `IOTAGID`/`IOPROPID`）；`Coding\api_bundle\apiv8\common\idl\dgapi_enum.h:714`（`typedef long KsoRGBType`）；`wppapiv8\idl\wppapi_i.h:785` | Win64 上 long=32、POSIX x64 上 long=64 → ID/颜色类型跨平台宽度不一致。对照 kern 的正确做法：POSIX `SIZE32`=`int32`（`kern\typedef.h:182-185`），Windows `size32`=`long`（`kglobal_def.h:68-70`），**同为 32 位** |
| `L"..."` 宽字面量 | `Coding\api_bundle` 共 **1683 处**，如 `jsapibrowser\stdafx.h:62-63`（`LOG_INFO` 宏里 `L"jsapibrowser"`）、`jscefservice\kjscefclient.cpp:139` | 跨平台共享代码应写 `__X("...")`（`kglobal_def.h:334-345`：VC→`L##x`，否则 `u##x`） |
| `wchar_t` 当 `WCHAR` | `Coding\core_bundle\office\et\ettext\stdafx.h:77`/`:79`；`...\ettext\RangeCollector.h:25`/`:32`；`Coding\api_bundle\api\jsapi\ksojscore\stdafx.cpp:9`（`const wchar_t* g_logModuleName = L"ksojscore"`）；`jscefservice\kjscefvisibilitytracemanager.cpp:520` | POSIX 上 `wchar_t` 是 32 位、`WCHAR` 是 `char16_t`，两者不能互传 |
| 格式串宽度不符 | `Coding\core_bundle\office\et\compiler\compiler.h:280` 用 `"%ld"` 打 `:275` 声明的 `int _iToken` | POSIX x64 上 `long` 是 64 位，变参宽度不符 |

反例的反例（**不是**违规）：`Coding\core_bundle\office\et\etsolver\tools\sobolseq.c:57-64` 用 `SIZEOF_UNSIGNED_LONG == 4` 守卫后才 `typedef unsigned long uint32_t` —— 这是正确的判宽写法。

## 5. 推荐 API 的用量（说明这些才是主流写法）

| API | 定义位置 | 真实用例 |
|---|---|---|
| `__X(x)` | `kglobal_def.h:334-345` | `core\office\et\webhook\src\db_webhook_changed_range.cpp:396` `xstrcmp(oldValue, __X(""))`；`demo\test_pivot_core\case_pivot_token.cpp:52` |
| `xstrcmp` | `cstring_win.h:121`→`wcscmp`；`cstring_posix.h:900`→`_Xu2_strcmp` | ET 内 **788 处**：`appcore\consolidate\cons_source.cpp:267`（比较 `V_BSTR`）、`consolidate.cpp:357` |
| `ks_wcslen` | `cstring_win.h:17`→`wcslen`；`cstring_posix.h:373`/`:747`（`wchar_t*` 与 `wchar16_t*` 两个重载） | ET 内 **313 处**：`persist\global.cpp:438`、`compiler\utilities.h:211`；api_bundle 15 处：`apiv8\etapiv8\etapiv8.cpp:111` |
| `SIZE32` | `kern\typedef.h:182-185`（POSIX `int32`）/ `kglobal_def.h:68-70`（Windows `long`） | SafeArray 下标：`applogic\xll\kxloper.inl:2329`/`:2363`、`appcore\customlist\customlistsop.cpp:163-164` |

**一句话规范**：字面量 `__X("")`，比较 `xstrcmp`，长度 `ks_wcslen`，32 位整数 `SIZE32`/`INT32`/`UINT32`，不用裸 `long` 和裸 `L""`。
