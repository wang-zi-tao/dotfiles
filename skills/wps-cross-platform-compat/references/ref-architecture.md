# 架构参考：四层逐层清单

> 路径相对仓库根 `D:\branch2\wpsmain`。
> 缩写：`K` = `Coding/foundation_bundle/include/kern`，`I` = `Coding/foundation_bundle/include`，
> `S` = `Coding/foundation_bundle/kfc_base/kskern`，`B` = `Coding/foundation_bundle/kbase`。

---

## L1 平台探测：`I/kbase/platform_detect.h`（331 行）

| 组 | 宏 | 行 |
|---|---|---|
| OS | `X_OS_WIN32`/`X_OS_WINDOWS`（`_WIN64` 另加 `X_OS_WIN64`） | :9-16 |
| | `X_OS_ANDROID` = + `X_OS_LINUX` + `X_OS_UNIX` | :25-28 |
| | `X_OS_OHOS` = + `X_OS_LINUX` + `X_OS_UNIX` | :29-32 |
| | `X_OS_LINUX` = + `X_OS_UNIX` | :33-38 |
| | `X_OS_DARWIN` = + `X_OS_UNIX`，再分 `X_OS_IOS`/`X_OS_MACOSX`+`X_OS_MACOS` | :83-92 |
| | 未知平台 `#error "Unknown OS Planform!!!"` | :96 |
| 位宽 | `X_BIT_64`（`__LP64__` 或 `_WIN64`），否则 `X_BIT_32` | :99-103 |
| 编译器 | `X_CC_VC`；`X_CC_GCC`（其内 `__clang__` → `X_CC_CLANG`）；兜底 `X_CC_CLANG` | :118-124, :168-170 |
| CPU | `X_CPU_X86/X86_64/ARM/ARM_64/MIPS/LOONGARCH64/SW_64` | :182-210 |
| **编码** | `X_ENCODE_UCS2`（Windows）/ `X_ENCODE_UCS4`（Linux/FreeBSD/Darwin） | **:226-242** |
| 组合 | `X_SUPPORT_QT_IMAGE`、`X_LINUX_GENERIC`+`X_SUPPORT_CUPS`、`X_LINUX_DESKTOP`、`X_SUPPORT_ODBC/IRM/RMS/OHOS_DLP`、`WEB_OFFICE_ENABLE`→`X_ENABLE_WO` | :264-329 |

⚠️ `X_OS_LINUX` 在 Android/OHOS 上恒真，`X_OS_DARWIN` 在 iOS 上恒真。

---

## L2 总开关：`I/kbase/kglobal_def.h`（784 行）

`:6` include `platform_detect.h`；`:41-85` 二选一：

```c
#if defined(X_OS_WINDOWS) && !defined(USES_KSKERN)
    /* 真 Windows */
    #if !defined(_WINDEF_)
        #define WIN32_LEAN_AND_MEAN
        #include <windows.h>
    #endif
    #include <objbase.h>
    #include <wchar.h>
    #include <cstdint>
    typedef int8_t int8;  ... typedef intptr_t intp; typedef uintptr_t uintp;
    typedef uintp UINTP;  typedef intp INTP;
    typedef uintp KSO_WParam; typedef intp KSO_LParam;
    typedef long          size32;   /* :68 */
    typedef unsigned long usize32;
    typedef size32   SIZE32;        /* :70 —— Windows 上底层是 long */
    typedef usize32  USIZE32;
    #undef TRUE
    #undef FALSE
    #define TRUE  1
    #define FALSE 0
    #define MAKEINT32 MAKELONG
#else
    /* POSIX（或 USES_KSKERN 强制仿真） */
    #include <kern/typedef.h>
    #include <kern/wingdi.h>
    #include <kern/errno.h>
    #include <kern/winnt.h>
    #include <kern/winuser.h>
#endif
```

`USES_KSKERN` 由 `S/stdafx.h:64`（及 `S/noqt/stdafx.h:64`）定义 —— 意思是「kskern 自己编译时，即使平台是 Windows 也走仿真分支」。

其余垫片：

| 能力 | 定义 | 行 |
|---|---|---|
| `__stdcall`/`__fastcall`/`__cdecl`/`__export` | `__attribute__((stdcall)) / ((fastcall)) / 空 / ((visibility("default")))` | :153-161 |
| `__int64`/`__uint64`/`PROPID`/`SIZE_T` | typedef | :158-161 |
| `__novtable` | VC→`__declspec(novtable)`，否则空 | :166-172 |
| `__export` 兜底 | 空 | :174-176 |
| `__forceinline` | `inline` | :178-180 |
| FORCEINLINE/FORCENOINLINE | | :182-198 |
| `_THROW0/_THROW1/_THROW` | | :200-208 |
| `__noop` | VC 用 inline `_Xnullfunc` 吃掉可变参 | :210-218 |
| `__w64` | 空 | :220-222 |
| `__stdcall` 兜底 | 或 `#error "Unsupport __stdcall?"` | :224-230 |
| `InterlockedIncrement/Decrement`（GCC） | `__sync_add_and_fetch/__sync_sub_and_fetch` | :250-258 |
| `__X(x)`/`__Xc`/`_Xs`/`_Xc` | VC→`L##x`；否则 `u##x` | **:334-345** |
| `_Xu(x)` | `(u ## x)` | :346 |
| `_KBASE/_KBASE_BEGIN/_KBASE_END/_USING_KBASE` | 命名空间宏 | :355-365 |
| `DECLARE_IID(Interface)` | `EXTERN_C const GUID IID_##Interface;` | :769 |

非 Windows 还会 :349-353 include `<kern/objbase.h>` + `<wchar.h>` + `"string/cstring.h"`。

---

## L3 头文件替身：`K/*.h`（38 个）

### 3.1 总表（按能力面）

| 文件 | 模拟什么 | Windows 上是否生效 |
|---|---|---|
| `typedef.h` | 基础类型全集 + 结构体 + 调用约定宏 | 门闩 `:9` `!defined(_WINDEF_)` → 已含 windows.h 则整文件跳过 |
| `errno.h` | **12936 行** HRESULT/SystemError 错误码镜像（≈ winerror.h） | `:10 #ifndef X_OS_WINDOWS` → **整文件不生效** |
| `winnt.h` | 537 行纯语言/区域 ID 常量（**无同步/原子/HANDLE**） | 无平台 gate，只有 include guard（:8） |
| `winuser.h` | 纯常量表：`SB_*`(:19-45)、`MB_*`(:48-93)、`ID*`(:95-108)、`COLOR_*`(:110-162) | 幂等 gate `:19 #ifndef SB_HORZ`、`:110 #ifndef NOCOLOR` |
| `wingdi.h` | 非 Win 平台自造 GDI 结构/常量（`CHARSET`:24-44、`LOGFONTA`:54、`FW_*`:380-391、`MM_*`:394-401、`R2_*`:407-423） | `:11 #if defined(_WIN32) && !defined(USES_KSKERN)` → :13-15 直接 `#include <wingdi.h>` |
| `winnls.h` | `NORM_*`(:14-23)、`LOCALE_*`(:47-256)、`CSTR_*`(:258-260) + 唯一函数 `CompareStringW`(:262-267) | 无常驻 gate |
| `string/winbase.h` | 仅 `lstrcmpA/W` 等 6 组 `_l` 宏 + UNICODE 分派 | :20/:31/:42/:53/:64/:75 |
| `sema.h` | **废壳**（21 行，提示改用 `kern/thread/sync.h`） | — |
| `thread.h` | 29 行聚合头；`sync` 被 `#if 0` 屏蔽(:13)，只带出 `tls.h`+`process.h`(:19-25) | — |
| `threads.h` | **整文件被 `//` 注释**(:8-24)，零声明 | — |
| `thread/sync.h` | 13 行，`:11` 转 `#include <kbase/kern/sync.h>`（真身在 I 层） | — |
| `thread/tls.h` | `TLSINDEX`(:20)、`TLS_OUT_OF_INDEXES`(:22-24)、4 个 `_XTls*`(:28-31)、**无条件**宏重定向(:35-38) | ⚠️ 见 ref-gotchas B6 |
| `thread/process.h` | 线程启动例程类型(:20-21)、`_XCreateThread`(:23-30)、**无条件** `CreateThread` 宏(:32-33) | ⚠️ Windows 实现是返回 NULL 的桩 |
| `thread/thread.h` | 不是 Win32 仿真，是 C++ RAII：`ScopedThreadPriority`(`enum`:3-16, `class`:18)，8 处 `#ifdef _WIN32`(:31/46/55/76/105/134/150/165) | 逐函数分叉 |
| `thread/kinstance.h` | 互斥单实例（`CreateMutexW`/`OpenMutexW`/`WaitForSingleObject`，:41-59），非 Win 全 `false` | — |
| `thread/kthreadsafefunctioncaller.h` | 「只初始化一次」模板(:74-95)；**kern 内唯一原子原语**：`InterlockedCompareExchangePointer`→`__sync_val_compare_and_swap`(:67-70，仅 linux/apple)、`YieldProcessor()`(:91) | — |
| `osl/filesys.h` | **空壳**：目录遍历 API 全在注释块(:12-49) | — |
| `stg/ksostgapi.h` | 复合文档存储：`STGM_*`(:21-42)、`STGFMT_*`(:54-58)、`_XStg*`(:211-232)、`Stg*` 宏重定向(:237-256) | `STDAPI` 兜底 :14-17 |
| `stg/ksostgapiex.h` | `WriteClassStg`/`ReadClassStg`：C++ 走 inline(:24/:38/:50)，C 走声明(:74/:79/:84)，宏 :98-108 | — |
| `win98/winver.h` | `:13` 只要 Windows 编译就 `#define X_OS_WIN98` | 恒真（别当 Win98 判断） |
| `base.h` | 17 行，只 include `winver.h`；`X_OS_WIN98` 时 `#pragma comment(lib,"unicows.lib")` | — |
| `typedesc.h` / `typedesc_data.h` | 类型描述表 | — |
| `string/tchar.h` / `string/mbstring.h` | TCHAR/MBCS 家族 | — |
| `objbase.h` / `objidl.h` / `oaidl.h` / `ocidl.h` / `docobj.h` / `ole2.h` / `oleauto.h` / `olectl.h` / `oledb.h` / `oleidl.h` / `propidl.h` | COM/OLE 全套 | 各文件 `#ifndef X_OS_WINDOWS` |
| `clx/typedef.h` / `clx/objidl.h` | **半废弃** legacy 变体 | 见 ref-gotchas F |

### 3.2 三种门闩写法（新加头时照抄）

1. **平台门闩**：`#ifndef X_OS_WINDOWS`（`oleauto.h:10`、`oaidl.h:10`、`objidl.h:10`、`errno.h:10`）
2. **SDK 幂等门闩**：`#if !defined(__KERN_TYPEDEF_H__) && !defined(_WINDEF_)`（`typedef.h:9`）
3. **行为幂等门闩**：`#ifndef SB_HORZ`（`winuser.h:19`）、`#ifndef NOCOLOR`（`winuser.h:110`）、`#ifndef DECLARE_HANDLE`（`typedef.h:422`）

---

## L4 实现层

### 4.1 `S = kfc_base/kskern`（STATIC，仅 Unix 构建）

```
bstr/bstr.cpp               _XSysAllocString 等（CoTaskMemAlloc）
safearray/safearray.cpp     _MSafeArray*（943 行，隐藏槽位 + FADF_* + GetVTSize）
variant/variant.cpp         _MVariant* 
variant/propvariant.cpp     PROPVARIANT 侧
time/time.cpp               VariantTimeToSystemTime / SystemTimeToVariantTime
com/cocreateguid.cpp        CoCreateGuid
com/errorinfo.cpp           _XCreateErrorInfo 等
fmtguid/fmtguid.cpp         GUID ↔ 字符串
memory/taskmalloc.cpp       _XCoGetMalloc/_XCoTaskMemAlloc/Realloc/Free（源码写 CoGetMalloc，靠 objbase.h:138-141 宏改名）
memory/malloc.cpp           _XFastAllocate/_XFastReallocate/_XFastDeallocate(:119/125/131)、_XTrackFileLine(:41/:61)
kskern.cpp / kskern.def     聚合 + 导出表
```

### 4.2 `B = kbase`（SHARED）

| 实现 | 落点 | 备注 |
|---|---|---|
| 同步原语 `_XInitializeCriticalSection/_XEnter/_XLeave/_XDelete` | `B/src/kern/sync.cpp:205/216/227/238` | 整文件 `:3 #if defined(X_OS_LINUX)||defined(X_OS_IOS)`，**Windows 不编译** |
| 信号量 `_XCreateSemaphore/_XReleaseSemaphore/_XWaitForSemaphore/_XDeleteSemaphore` | `B/src/kern/sync.cpp:115/156/31/184` | `SEMAPHORE` 结构 :23-29；`_XOpenSemaphore` 只声明(:61)，实现未找到 |
| 字符串 `_Xu2_*` | `B/src/string/cstring_posix_u16.cpp` | 声明在 `I/kbase/string/_/cstring_posix_u16.h` |
| 文件/目录 | `B/src/file/{file,dir,filefind,file_posix}.cpp` | 平台选源见 `B/CMakeLists.txt:39-44` |
| 快速分配 | `B/src/malloc/fastalloc.cpp:10` | 与 `S/memory/malloc.cpp` 重复实现 |

### 4.3 未进构建 / 未找到

- `S/thread/{synch,thread,tls}.cpp`：**不在** `S/CMakeLists.txt:6-22` 源清单内；`synch.cpp` 还是 10 行空文件
- `kskern.def:61-75`：`_XTls*`/`_XCreateThread` 导出被注释
- `CompareStringW`（声明 `K/winnls.h:262`）：foundation_bundle 内**未找到实现**
- `YieldProcessor`：仅 `K/thread/kthreadsafefunctioncaller.h:91` 使用，未找到定义（仅 linux/apple 分支有 `InterlockedCompareExchangePointer` 定义）

---

## 5. 一句话层级心智模型

```
platform_detect.h   告诉你是谁（哪个 OS/编译器/位宽/编码）
kglobal_def.h       决定用真 SDK 还是替身头
kern/*.h            提供「名字」（类型、宏、声明）—— 平台无关的 API 面
kskern + kbase/src 提供「实现」（Unix 上真正干活）
业务代码            只认名字，不认平台
```

出问题时先判断错在哪一层：

| 错误类型 | 层 | 排查入口 |
|---|---|---|
| 找不到类型/宏/名字 | L2 或 L3 | 是否 include 了 `kglobal_def.h`；`UNICODE` 之类宏是否定义 |
| 类型宽度/布局不对 | L3 | 是否用了 `long`/`ULONG` 而不是固定宽度类型 |
| 链接找不到符号 | L4 + CMake | `wps_link_packages(UNIX(kskern))`、`libsafec` |
| 运行时行为不同 | L4 | 看对应 `S/`/`B/` 实现，别假设和 Windows 一样 |
