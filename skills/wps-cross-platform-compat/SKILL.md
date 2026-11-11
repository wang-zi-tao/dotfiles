---
name: wps-cross-platform-compat
description: "WPS Office 的 Windows 兼容层（kern/* 头文件 + kskern 实现 + kbase 抽象）使用与扩展指南。用于在 Windows/Linux/macOS/OHOS 共享代码中正确使用 WCHAR/DWORD/HRESULT/VARIANT/SAFEARRAY/BSTR/COM 接口，规避 wchar_t 宽度、long 宽度、调用约定、__uuidof、__declspec 等平台差异。触发词：跨平台、平台兼容、kern 头文件、X_OS_WINDOWS、X_ENCODE_UCS2、WCHAR、char16_t、SIZE32、__X 宏、xstrcmp、ks_wcslen、VARIANT、SAFEARRAY、BSTR、SysAllocString、_MSafeArray、_Xu2_、kskern、wps_link_packages(kskern)、wcscmp 编译错误、wchar_t 类型不匹配、DWORD 重定义、CreateThread、TlsAlloc、thread_local、ohos 链接失败、linux 编译失败。"
---

# WPS 跨平台 Windows 兼容层

## 一句话本质

WPS 不是在每个平台写 `#ifdef` 补丁，而是**在 POSIX 上把 `<windows.h>` 那整套头文件按文件名逐个重造一遍**，让 40M 行业务代码在 Linux/macOS/OHOS 上 include 到的 `windows.h`、`objbase.h`、`oleauto.h` 是 WPS 自己的版本。类型名、API 名、宏名、调用约定全部保持一致，因此**业务代码不需要平台分叉**——需要分叉的只有极少数内核实现文件。

这带来一个直接推论，也是本技能最重要的一条：

> 在 WPS 的共享代码里，你写的就是 Windows 代码。**看到 `wcscmp`/`wcslen`/`L"..."`/`long` 当 32 位用/`wchar_t` 当 WCHAR 用，就是在制造跨平台 bug**；正确做法不是加 `#ifdef`，而是换成兼容层提供的等价物（`xstrcmp`、`ks_wcslen`、`__X(...)`、`SIZE32`）。

## 四层架构

```
业务代码（api_bundle / core_bundle / shell_bundle ...，共享、几乎不分叉）
        │  #include "kglobal_def.h" 或 stdafx.h
        ▼
┌─────────────────────────────────────────────────────────┐
│ L1 平台探测   kbase/platform_detect.h                    │
│     X_OS_* / X_BIT_64 / X_CC_* / X_CPU_* / X_ENCODE_*    │
├─────────────────────────────────────────────────────────┤
│ L2 总开关     kbase/kglobal_def.h:41                     │
│     Windows → 真 <windows.h> + <objbase.h>               │
│     POSIX   → <kern/typedef.h> <kern/winnt.h> ...        │
├─────────────────────────────────────────────────────────┤
│ L3 头文件替身 include/kern/*.h  （38 个，同名对应系统头）│
│     typedef.h winnt.h wingdi.h winnls.h errno.h ...      │
│     objidl.h oaidl.h oleauto.h ... （COM/OLE 全套）      │
│     ★ 名字/类型/宏/声明为主，个别 inline（见 3 个例外）  │
├─────────────────────────────────────────────────────────┤
│ L4 实现层（两个包！不是只有一个）                        │
│     a) kfc_base/kskern/  STATIC，仅 POSIX 构建           │
│        bstr safearray variant time com fmtguid memory    │
│     b) kbase/src/        SHARED                          │
│        kern/sync.cpp（同步原语）、string/、file/、malloc/│
└─────────────────────────────────────────────────────────┘
        │
        ▼
  POSIX 系统调用 / pthread / CoTaskMem 模拟 / libsafec
```

关键点：**L3 基本只有声明，符号在 L4，而 L4 横跨两个包**：

- 字符串/变体/数组/COM 类符号（`_XSysAllocString`、`_MSafeArray*`、`_MVariant*`、`_XCoTaskMem*`）→ **kskern**（STATIC，只有 POSIX 下有）
- 同步原语（`_XInitializeCriticalSection` 等）→ **kbase/src/kern/sync.cpp**（不是 kskern！`kern/winnt.h` 里根本没有同步原语）
- 字符串工具（`_Xu2_strcmp` 等）→ **kbase/src/string/**

所以「头文件能编过」不等于「链接能过」——这是 WPS 跨平台最高频的问题，见 `references/ref-cmake-linking.md`。

**三个「L3 里有实现」的例外**（别以为 L3 一律只有声明）：
- `kern/oleauto.h:206-232` — `VariantInit` 等 C++ inline
- `kern/stg/ksostgapiex.h:24-50` — `_XWriteClassStgInline` 等 C++ inline
- `kern/thread/thread.h` — `ScopedThreadPriority` 整套 RAII 实现

## 五个核心机制

### 1. 文件名即契约：用同名头文件替换系统头

```
include/kern/typedef.h   ← 替换 <windows.h> 里的基础类型段
include/kern/winnt.h     ← 替换 <winnt.h>（⚠️ 只有语言/区域常量，没有同步原语）
include/kern/errno.h     ← 替换 <winerror.h>（12936 行错误码）
include/kern/objidl.h    ← 替换 <objidl.h>（IUnknown/IClassFactory...）
include/kern/oaidl.h     ← 替换 <oaidl.h>（VARENUM/SAFEARRAY/tagVARIANT）
include/kern/oleauto.h   ← 替换 <oleauto.h>（SysAllocString/SafeArray*/Variant*）
```

**注意 `kern/winbase.h` 并不存在**（只有 `kern/string/winbase.h`，仅补 `lstrcmpA/W` 等 6 组 `_l` 宏）。`CreateFile`/`CreateProcess` 这类 Win32 文件/进程 API **没有**兼容层，要用 kbase 的 `KRtFile`/`KFile` 抽象。

每个头都有自己的「已被真实系统头定义过就跳过」门闩，共三种写法：
1. **平台门闩**：`#ifndef X_OS_WINDOWS`（`oleauto.h:10`、`oaidl.h:10`、`objidl.h:10`、`errno.h:10`）
2. **SDK 幂等门闩**：`#if !defined(__KERN_TYPEDEF_H__) && !defined(_WINDEF_)`（`typedef.h:9`）
3. **行为幂等门闩**：`#ifndef SB_HORZ`（`winuser.h:19`）、`#ifndef NOCOLOR`（`winuser.h:110`）、`#ifndef DECLARE_HANDLE`（`typedef.h:422`）

**新加一个平台替身头时，必须照抄其中一类**，否则 Windows 构建会和真 windows.h 撞定义（真实事故见 `references/ref-gotchas.md` A1/A2）。

### 2. 类型同义：名字一样、宽度保证一样

`kern/typedef.h` 把 Win32 基础类型逐个重新 typedef，并用 `static_assert`（:190-200）把宽度钉死：

| Win32 | 兼容层定义 | 位置 |
|---|---|---|
| `DWORD` | `typedef UINT32 DWORD` | kern/typedef.h:140 |
| `BOOL` | `typedef int BOOL` | kern/typedef.h:141 |
| `HRESULT` | `typedef INT32 HRESULT` | kern/typedef.h:204 |
| `WCHAR` | `typedef char16_t WCHAR` | kern/typedef.h:224 |
| `VARTYPE` | `typedef unsigned short` | kern/typedef.h:105 |
| `VARIANT_BOOL` | `typedef short` | kern/typedef.h:106 |
| `SIZE32` | `typedef int32 SIZE32`（POSIX）/ `long`（Windows） | kern/typedef.h:184 / kglobal_def.h:70 |
| `HANDLE` | `void*` | kern/typedef.h:428 |

⚠️ 注意 `SIZE32`：两个分支的底层类型不同（`int32` vs `long`），但**都是 32 位**，所以作为索引/计数是可以互换的。这正是「索引统一用 `SIZE32`、不要用 `LONG`/`long`/`int`」的原因——`long` 在 Linux x64 上是 64 位。

同样的思路出现在 `kern/oaidl.h:145-153` 的 `SAFEARRAY` 结构：字段刻意写成 `UINT32 cElements` / `UINT32 cbElements` 而不是 `ULONG`，以保证 32/64 位、Win/POSIX 下**内存布局逐字节一致**（SAFEARRAY 会被跨模块、跨 DLL 边界传递，布局错一字节就是崩溃）。

### 3. 调用约定与导出：宏垫片

`kbase/kglobal_def.h:153-161`（GCC 分支）：

```c
#define __stdcall  __attribute__((stdcall))
#define __fastcall __attribute__((fastcall))
#define __cdecl                                  /* 空 */
#define __export   __attribute__((visibility("default")))
typedef int64_t  __int64;
typedef uint64_t __uint64;
typedef size_t   SIZE_T;
```

MSVC 分支在 `:113-143`，含 `X_VER_VC_*` 版本门限（低于 msvc2010 直接 `#error`）。

配套还有：`__declspec(novtable)` → `__novtable`（:166-172）、`__forceinline` → `inline`（:178-180）、`_THROW0/_THROW1/_THROW`（:200-208）、`__noop`（:210-218）、`__w64`（:220-222）、`InterlockedIncrement/Decrement` → `__sync_add_and_fetch/__sync_sub_and_fetch`（:250-258）。

**规范：接口/回调一律写 `STDMETHODCALLTYPE`/`STDMETHODIMP`/`STDAPI`，不要手写 `__stdcall`**；只有实在绕不开时才用 `__stdcall` 宏。

### 4. API 重定向：对外保留微软名字，对内指向 WPS 实现

这是整套设计最精妙的地方。`kern/oleauto.h` 里：

```c
/* :147-154 先声明 WPS 自己的实现（_X 前缀 = X 平台原生实现） */
EXPORTAPI_(BSTR) _XSysAllocString(const OLECHAR* psz);
EXPORTAPI_(BSTR) _XSysAllocStringLen(const OLECHAR* psz, UINT32 ui);
EXPORTAPI_(void) _XSysFreeString(BSTR bstr);
EXPORTAPI_(UINT32) _XSysStringLen(BSTR bstr);
...
/* :156-171 再把微软名字重定向过去 */
#define SysAllocString(psz)      _XSysAllocString(psz)
#define SysFreeString(bstr)      _XSysFreeString(bstr)
#define SysStringLen(bstr)       _XSysStringLen(bstr)
```

SafeArray 同理（`:297-320`）：`#define SafeArrayCreate _MSafeArrayCreate`、`SafeArrayDestroy`、`SafeArrayGetElement`…全部重定向到 `_MSafeArray*`。Variant 同理（`:235-241`）：`VariantInit → _MVariantInit`、`VariantClear → _MVariantClear`、`VarCmp → _MVariantCompare`。

前缀语义（写新代码时按这个规则选）：
- `_X...` —— WPS 自己定义的语义，名字是 WPS 加的（`_XSysAllocString`、`_XCreateErrorInfo`）
- `_M...` —— Microsoft 语义的镜像实现（`_MVariantClear`、`_MSafeArrayCreate`）
- 宏重定向 —— 给业务代码用的微软名字

**所以业务代码里照 Windows 习惯写 `SysAllocString`/`SafeArrayCreate`/`VariantClear` 就是对的，不要改成 `_M*` 名字。**

### 5. 字符编码：UCS2 vs UCS4 是整层的根因

`kbase/platform_detect.h:226-242`：

```c
#ifdef X_OS_WINDOWS
  #define X_ENCODE_UCS2      /* Windows: wchar_t = 16 bit */
#else
  #define X_ENCODE_UCS4      /* Linux / Darwin / FreeBSD: wchar_t = 32 bit */
#endif
```

于是：
- `kern/typedef.h:224` → `typedef char16_t WCHAR;`（POSIX 上 `WCHAR` **不是** `wchar_t`）
- `kern/clx/typedef.h:67-73` 是老版本写法：`X_ENCODE_UCS2 → wchar_t`、`X_ENCODE_UCS4 → char16_t`（该文件已半废弃，别照抄，见 `references/ref-gotchas.md`）
- 字面量：`kglobal_def.h:334-345`
  ```c
  #ifdef X_CC_VC
    #define __X(x)  L ## x        /* → wchar_t*   */
  #else
    #define __X(x)  (u ## x)      /* → char16_t*  */
  #endif
  ```
- 字符串函数：`xstrcmp` 在 Windows 上是 `wcscmp`（`kbase/string/_/cstring_win.h:121`），在 POSIX 上是 `_Xu2_strcmp`（`kbase/string/_/cstring_posix.h:900`），后者由 `cstring_posix_u16.h:31-71` 声明、按 UTF-16 语义逐字符比较。

推论（写代码时直接背）：
1. **宽字面量一律 `__X("...") `，禁止裸 `L"..."`**（除非该文件确实只在 Windows 编译）。
2. **宽字符串处理一律 `x` 前缀族**（`xstrlen/xstrcmp/xstrcpy_s/xstrcat_s/xstrdup/xstrchr/xstrstr/xstrtol/xsprintf/xsplitpath`…），禁止 `wcs*`。
3. **跨语言的字符串视图用 `std::u16string_view`，不要用 `std::wstring_view`**——后者在 POSIX 上是 32 位字符，和 `WCHAR*` 不兼容。
4. `sizeof(WCHAR)` 两边都是 2，但 `sizeof(wchar_t)` 不是——**不要拿 `wchar_t` 当 `WCHAR` 用**。

## 同步、线程、TLS：**不要**照 Win32 名字写

这是整套兼容层里唯一「名字存在但实现不存在/未进构建」的雷区，必须先看清：

| 你可能会写 | 兼容层里实际是什么 | 结论 |
|---|---|---|
| `CRITICAL_SECTION` / `EnterCriticalSection` | 真身在 `include/kbase/kern/sync.h:38-44`（`pthread_mutex_t` 模拟），**不在 `kern/winnt.h`** | POSIX 可用；Windows 走真 SDK |
| `CreateSemaphore` / `WaitForSingleObject` | `kbase/src/kern/sync.cpp:115/156/31/184` | 同上（`_XOpenSemaphore` 无实现） |
| `TlsAlloc` / `TlsGetValue` | `kern/thread/tls.h:35-38` **无条件**重定向到 `_XTls*`，实现文件 `kskern/thread/tls.cpp` **不在 kskern 构建里**（`kskern/CMakeLists.txt:6-22` 源清单无 `thread/`，`kskern.def:61-75` 导出也被注释） | **别用** → `thread_local` |
| `CreateThread` | `kern/thread/process.h:32-33` **无条件**重定向到 `_XCreateThread`；Windows 分支是 `REPORT("CreateThread - Not Impl"); return NULL;`（`kskern/thread/thread.cpp:84-95`），且同样未进构建 | **别用** → `std::thread` |

正确姿势：

```cpp
#include <thread>
static thread_local Ctx* t_ctx = nullptr;          // 替代 TlsAlloc/TlsGetValue
std::thread worker([this]{ ... });                 // 替代 CreateThread
```

真要在 WPS 里降线程优先级，用 `kern/thread/thread.h` 的 `ScopedThreadPriority`（内部按平台分叉，含 macOS QoS / Linux nice / Windows 优先级），别自己调 `SetThreadPriority`。

**原子操作**：`kern/thread/kthreadsafefunctioncaller.h:67-70` 是 kern 里唯一一处 `InterlockedCompareExchangePointer` 的跨平台实现（→ `__sync_val_compare_and_swap`，仅 linux/apple）。新代码请用 `std::atomic`，不要指望 kern 提供 `Interlocked*`（`kern/` 目录内 grep `Interlocked` 仍会命中这个文件，但没有其他通用原子原语）。

## 快速对照表（写代码时查这张）

| 想干的事 | ❌ 不要写 | ✅ 要写 |
|---|---|---|
| 宽字符串字面量 | `L"abc"` | `__X("abc")` |
| 宽字符串长度 | `wcslen(s)` | `xstrlen(s)` / `ks_wcslen(s)` |
| 宽字符串比较 | `wcscmp(a,b)` | `xstrcmp(a,b)` |
| 宽字符串复制/拼接（安全版） | `wcscpy_s` / `wcscat_s` | `xstrcpy_s` / `xstrcat_s` |
| 索引/元素个数 | `long` / `int` | `SIZE32` / `size32` |
| 位宽固定整数 | `long` 当 32 位 | `INT32` / `UINT32` |
| 调用约定 | 手写 `__stdcall` | `STDMETHODCALLTYPE` / `STDAPI` |
| 接口声明 | `struct IX { virtual ... }` | `interface IX` + `STDMETHOD`/`STDMETHOD_` |
| 接口 IID | 手写 GUID 常量 | `DECLARE_IID(IX)` + `DEFINE_IID` |
| 取接口 IID | `__uuidof(IX)`（GCC 不支持） | `__iid(IX)` |
| 实现 COM 类 | 手写 QueryInterface | `DECLARE_COMCLASS(CMy, IMY)` |
| 字符串类型 | `wchar_t*` | `WCHAR*` / `LPWSTR` / `LPCWSTR` |
| 字符串视图 | `std::wstring_view` | `std::u16string_view` |
| 布尔 | `bool` 传给 COM | `BOOL` / `VARIANT_BOOL` |
| 平台判断 | `#ifdef _WIN32` | `#ifdef X_OS_WINDOWS`（见下表） |
| 线程 | `CreateThread` / `_beginthreadex` | `std::thread` |
| 线程局部存储 | `TlsAlloc` / `TlsSetValue` | `thread_local` |
| 互斥 | `CreateMutex` | `kbase/kern/sync.h` 的 `CRITICAL_SECTION` 是模拟的；跨平台首选 `std::mutex` |

平台宏（`kbase/platform_detect.h`）：

| 宏 | 含义 | 何时为真 |
|---|---|---|
| `X_OS_WINDOWS` | Windows 族 | Win32/Win64/Cygwin |
| `X_OS_WIN32` / `X_OS_WIN64` | 具体 | |
| `X_OS_LINUX` | Linux 族 | **含 Android 与 OHOS** |
| `X_OS_ANDROID` / `X_OS_OHOS` | 移动端细分 | |
| `X_OS_DARWIN` | Darwin 族 | **含 iOS 与 macOS** |
| `X_OS_MACOSX` / `X_OS_MACOS` / `X_OS_IOS` | 桌面/移动细分 | |
| `X_OS_UNIX` | 所有类 Unix | Linux/Android/OHOS/Darwin |
| `X_BIT_64` | 64 位 | `__LP64__` 或 `_WIN64` |
| `X_CC_VC` / `X_CC_GCC` / `X_CC_CLANG` | 编译器 | |
| `X_ENCODE_UCS2` / `X_ENCODE_UCS4` | 字符编码 | Windows / 其他 |

⚠️ `X_OS_LINUX` 恒真于 Android/OHOS，`X_OS_DARWIN` 恒真于 iOS。**要区分「桌面」请用 `X_OS_MACOS`、`X_LINUX_DESKTOP`**，不要用 `X_OS_LINUX` 判断「是不是桌面 Linux」。

## COM 层：三组宏就够了

WPS 不用 MIDL 生成代码，而是用宏在 C++ 里手写 COM 对象（`kbase/kcomobj.h`）：

```cpp
class CMyObject : public IMyInterface
{
public:
    DECLARE_COMCLASS(CMyObject, IMyInterface)   // kcomobj.h:243 —— QueryInterface+AddRef+Release+计数
    // 业务方法...
    STDMETHODIMP DoWork(INT32 n) override;
};
```

展开逻辑（`kcomobj.h:118-191`）：`BEGIN_QUERYINTERFACE(Interface)` 生成 `QueryInterface`，内部用 `ISEQUAL_IID(riid, __iid(Interface)) || ISEQUAL_IID(riid, IID_IUnknown)` 判定，命中则 `static_cast` 返回；`ADD_INTERFACE(X)` 追加支持的接口；没命中走 `END_QUERYINTERFACE()` 的 `E_NOINTERFACE` 分支。

GUID 的跨编译器模拟（`kbase/com/kguiddef.h`）：
- MSVC：`DEFINE_IID` → `interface __declspec(uuid("...")) IX;`，`__uuid` → `__uuidof`
- GCC/Clang：用模板特化 `non_native_uuidof<T>()`（:8-14）模拟同一个语义，`__uuid(cls)` → `NON_NATIVE_UUIDOF(cls)`
- 统一取用：`__iid(IX)`（:57-59）/ `__clsid(CLS)`（:61-63）

这套头文件里有历史包袱（`kguiddef.h:52-56` 的注释自承 `__iid`/`__clsid` 与 `guid.h` 定义冲突），细节与规避见 `references/ref-gotchas.md`。

## 硬性规范（MUST / MUST NOT）

**MUST**
1. 共享代码里所有 Win32 类型/宏/API 都直接用裸名字（`DWORD`、`HRESULT`、`SysAllocString`、`SafeArrayCreate`），不要加平台命名空间或别名。
2. 平台差异优先用 `X_OS_*` 宏；确实需要系统头（`<unistd.h>`、`<sys/mman.h>`）时，放在 `#if !defined(X_OS_WINDOWS)` 里，且限定在实现文件（`.cpp`）而非头文件。
3. 新写跨平台 API 时，按 L3/L4 分工落地：声明进 `include/kern/<对应系统头名>.h`，实现进 `kfc_base/kskern/<模块>/`，并用宏/或同名函数把名字对齐。
4. 引用 `WCHAR` 的字符串一律 `__X` + `x` 族函数。
5. 改了 kern 头或 kskern 实现后，**必须至少编一次 OHOS 或 Linux 目标**——Windows 构建永远发现不了这些错误（AGENTS.md 里也有这条：ohos 编译能快速暴露跨平台问题）。
6. 用到 `SysAllocString`/`SafeArrayCreate`/`VariantXxx` 的目标，**自己**在 CMakeLists.txt 写 `wps_link_packages(UNIX(kskern))`；kbase 是 PRIVATE 链 kskern，不会传递（`Coding\toolchain\cmake\wps_internal.cmake:1313` 是 PRIVATE）。

**MUST NOT**
1. 不要在共享代码里 `#include <windows.h>`（应 include `kglobal_def.h` 或模块 stdafx.h）。
2. 不要用 `wchar_t` / `std::wstring` / `wcs*` 处理 `WCHAR` 数据。
3. 不要用 `long` 表示 32 位整数或索引。
4. 不要在头文件顶层写 `#pragma pack` 而不 `pop`（`kern/typedef.h` 用 push/pop 是对的，`kern/clx/typedef.h` 是反例）。
5. 不要复制粘贴一份新的类型 typedef 到自己模块（仓库里 `kbase/string/_/cstring_def.h:54-70` 就重复了 `kglobal_def.h` 的垫片，属维护债）。
6. 不要用 `TlsAlloc`/`CreateThread`——那两个是「名字在、实现不在构建里」的坑（见「同步、线程、TLS」一节）。

## 业务代码里真实长什么样（先看这段再动手）

**包含方式**（三条路都有人在走，选第 1 条）：

1. `#include <kfc.h>` 或 `<kbase/kglobal_def.h>` —— 主流，例如 `Coding\core_bundle\office\et\pqfunc\stdafx.h:47`
2. `#ifndef X_OS_WINDOWS` 里直接 `#include "kern/typedef.h"` + 就地补 `typedef void* HWND;` —— 次主流，例如 `Coding\core_bundle\office\et\uilogic\stdafx.h:66-69`
3. 只补单个头，例如 `Coding\core_bundle\office\et\applogic\stdafx.h:74` 引 `kern/winnls.h`

**平台宏用两套，别混**：

- `core_bundle`（ET 内核）以 `X_OS_*` 为准，且 `X_OS_DARWIN` 计数常高于 `X_OS_WINDOWS`（macOS 适配更深）
- `shell_bundle`/`api_bundle` 的 Qt 侧以 `Q_OS_WIN` 为准（历史遗留），甚至同文件三套并存：`Coding\shell_bundle\ksolite\util\common.cpp:12`(`Q_OS_WIN`)、`:32`(`X_OS_DARWIN`)、`:1928`(`X_OS_UNIX`)
- 新代码：非 Qt 模块用 `X_OS_*`；Qt 控件/窗口相关用 `Q_OS_*`；**同一文件别混**

**存量反例规模**（说明这问题很普遍，不是个例）：`wcscmp` 吃 `WCHAR` 在 etch 目录 **145 处**；`L"..."` 在 `Coding\api_bundle` **1683 处**；典型 `typedef unsigned long _IOTHEID`（`Coding\core_bundle\office\et\include\io\et_ioacceptor.h:11`）。

**正确写法的用量**（这些才是主流）：`xstrcmp` ET 内 788 处、`ks_wcslen` 313 处、`SIZE32` 264 处。看到老代码写 `wcscmp` 不代表它对——大概率是没编过 POSIX 的历史遗留。

完整统计、逐条 grep 证据见 `references/ref-usage-patterns.md`。

## 排错 Checklist（编译/链接报错对着这张查）

| 症状 | 根因 | 处理 |
|---|---|---|
| `no matching function for call to 'wcscmp'` | POSIX 上实参是 `char16_t*`，`wcscmp` 要 `wchar_t*` | 改 `xstrcmp`（曾一次出现 198 处） |
| `typedef redefinition ... ('unsigned long' vs 'UINT32')` on `DWORD` | `kern/typedef.h:140` 与 SDK `minwindef.h:156` 撞了：`kern/typedef.h` 先于 `<windows.h>` 被包含，`_WINDEF_` 门闩还没立起来 | 改走 `kglobal_def.h`（按平台二选一），别直接 include `kern/typedef.h` |
| `typedef redefinition with different types ('char16_t' vs 'wchar_t')` | 同时包含了真 windows.h 和 kern/typedef.h；或误引了 `cstring_posix_u16.h` | 检查 include 顺序，别在 POSIX 引入 `<windows.h>`；POSIX-only 头不进 Windows TU |
| `undefined reference to _XTlsAlloc` / 运行期 `CreateThread` 返回 NULL | `kern/thread/tls.h:35-38`、`kern/thread/process.h:32-33` 无条件重定向，但实现文件不在 kskern 构建里 | 改用 `thread_local` / `std::thread`，见「同步、线程、TLS」一节 |
| `use of undeclared identifier 'SafeArrayCreate'` | 没 include `kern/oleauto.h`（或没经 kglobal_def.h） | 补 include |
| `undefined reference to _MSafeArrayCreate` / `_XSysAllocString` | 头文件在 L3、实现在 L4，目标没链接 kskern | `wps_link_packages(UNIX(kskern))`，可能还要 `wps_use_packages(LINUX(libsafec))` |
| `undefined reference to vmemcpy_s` / `u_strlen` | kskern 依赖 libsafec | 同上补 libsafec |
| `#endif without #if` | 头文件 endif 配对写坏（`kern/clx/typedef.h:102` 有先例） | 修配对 |
| 结构体大小在平台间不一致 / 跨模块传 SAFEARRAY 崩溃 | 用了 `ULONG`/`long` 成员 | 成员改 `UINT32`/`INT32` |
| `__uuidof` 在 GCC 下报错 | GCC 无此关键字 | 改 `__iid(IX)` |
| `interface` 报语法错误 | `interface` 宏未定义 | 经 `kglobal_def.h`/`kern/typedef.h` 引入 |

## 如何新增一个「Windows API」的跨平台支持（标准工作流）

1. **确认归属系统头**：这个 API 在 Windows 属于哪个头（`winbase.h`/`oleauto.h`/…）→ 就加到 `include/kern/` 里同名文件的对应段落。
2. **写声明**：用 `EXPORTAPI_`/`EXTERN_C`/`STDAPICALLTYPE` 修饰；加 `#ifndef X_OS_WINDOWS` 门闩（若无）。
3. **写实现**：放 `kfc_base/kskern/<功能目录>/*.cpp`。实现用 POSIX/pthread/CoTaskMem 模拟，命名按 `_X*`（自造语义）或 `_M*`（微软语义镜像）。
4. **接名字**：如果业务要写微软名，在声明后加 `#define WinName _MImpl`（参照 `kern/oleauto.h:297-320` 的 SafeArray 段落）。
5. **加链接依赖**：调用方目标补 `wps_link_packages(UNIX(kskern))`（以及 libsafec 等传递依赖）。kbase 只是 PRIVATE 链接 kskern，**不会**把符号导出给下游。
6. **验证**：编一次 OHOS 或 Linux；Windows 编过不代表可用。

## 参考文件

| 文件 | 内容 |
|---|---|
| `references/ref-architecture.md` | 四层架构逐层文件清单、门闩条件、头与 cpp 的符号对应表 |
| `references/ref-api-mapping.md` | Win32 / COM / VARIANT / SAFEARRAY / BSTR / 字符串 API 的完整映射表与行号 |
| `references/ref-gotchas.md` | 历史包袱、废弃文件、重复实现、真实踩坑案例 |
| `references/ref-cmake-linking.md` | 链接约定、kskern 与 libsafec 依赖、常见链接错误处理 |
| `references/ref-usage-patterns.md` | 业务代码真实包含方式、X_OS_*/Q_OS_* 用量统计、真实反例与推荐 API 用例 |
