# 踩坑与历史包袱

> 路径相对仓库根 `D:\branch2\wpsmain`。每条都带证据行号；标「实测」的是本次分析中实际触发过的诊断。

---

## A. 包含顺序 / 宏冲突

### A1. `DWORD` 重定义：`unsigned long` vs `unsigned int`（实测）

```
error: typedef redefinition with different types
       ('unsigned long' vs 'UINT32' (aka 'unsigned int'))
  minwindef.h:156:29  note: previous definition is here   ← Windows SDK
  kern/typedef.h:140:19 note: previous definition is here  ← 兼容层
```

根因：`kern/typedef.h:9` 的门闩是 `#if !defined(__KERN_TYPEDEF_H__) && !defined(_WINDEF_)`。
`_WINDEF_` 由 `<windows.h>`/`<minwindef.h>` 定义。如果某个 TU **先** 包含了 `kern/typedef.h`、**后** 才间接引入 Windows SDK 头，门闩当时还没立起来，两份 `DWORD` 就会撞。

**处理**：在 Windows 上永远经 `kglobal_def.h` 引入（它按 `X_OS_WINDOWS` 二选一，不会两个都进）；不要直接 `#include <kern/typedef.h>`。

### A2. `WCHAR` 重定义：`char16_t` vs `wchar_t`（实测）

```
error: typedef redefinition with different types ('char16_t' vs 'wchar_t')
  cstring_posix_u16.h:9:18
  winnt.h:463:17 (Windows SDK) note: previous definition is here
```

`cstring_posix_u16.h` 是**纯 POSIX** 头（`typedef char16_t WCHAR`），不能出现在 Windows 编译单元里。它应该只被 `cstring_posix.h` 间接拉进来，而 `cstring.h` 会按平台分派 `cstring_win.h` / `cstring_posix.h`。

**处理**：不要直接 include `*_posix*.h`；走 `kbase/string/cstring.h`。

### A3. 同一份垫片被复制了多份

`kbase/string/_/cstring_def.h:54-70` 里又写了一遍 `__stdcall`/`__fastcall`/`__cdecl`/`__export`/`__int64` 的 GCC 分支（和 `kglobal_def.h:153-161` 重复），`:340` 又重复了一次 `DECLARE_IID`。

**风险**：改一处漏一处，两个头一起包含时出现 macro redefined。
**处理**：新代码只依赖 `kglobal_def.h`；发现重复定义优先删而不是加 `#undef`。

---

## B. 「以为有其实没有」的 API

### B1. `kern/winnt.h` 里**没有**同步原语

`kern/winnt.h` 共 537 行，全是语言/区域 ID 常量（`LCID`:14、`LANG_NEUTRAL`:35、`SUBLANG_*`:185-430、`MAKELANGID`:480、`MAKELCID`:507）。
`CRITICAL_SECTION` / `SRWLOCK` / `Interlocked*` 在整个 `include/kern/` 目录 **0 命中**。

真正的同步模拟在：

```
Coding/foundation_bundle/include/kbase/kern/sync.h:38-44   CRITICAL_SECTION 用 pthread_mutex_t；SEMAPHORE 用 mutex+cond
                                               :46-77     _XInitializeCriticalSection/_XEnterCriticalSection/_XLeaveCriticalSection/_XDeleteCriticalSection
                                                          _XCreateSemaphore/_XReleaseSemaphore/_XWaitForSemaphore/_XDeleteSemaphore
                                               :91-116    Win32 名 → _X* 的宏重定向
                                               :31        #if defined(X_OS_UNIX)  ← 总闸
实现：Coding/foundation_bundle/kbase/src/kern/sync.cpp:205/216/227/238（Initialize/Enter/Leave/Delete）
                                                     :115/156/31/184（CreateSemaphore/ReleaseSemaphore/Wait/Delete）
```

注意 `kern/thread/sync.h` 只有 13 行，内容是把本体转出去（`:11 #include <kbase/kern/sync.h>`）。
而 `kbase/src/kern/sync.cpp` 自身被 `:3 #if defined(X_OS_LINUX)||defined(X_OS_IOS)` 包裹，**Windows 上不编译**（Windows 用真 SDK）。

### B2. `kern/winbase.h` 不存在

只有 `kern/string/winbase.h`，且仅补 `lstrcmpA/W` 等 6 组 `_l` 宏（:20/:31/:42/:53/:64/:75，UNICODE 分派）。
`CreateFile`/`CreateProcess` 等 Win32 文件/进程 API 在这里**都没有**——它们走 kbase 自己的 `KRtFile`/`KFile` 抽象（`kbase/src/file/`）。

### B3. `kern/threads.h` 是空文件

`:8-24` 整段被 `//` 注释，**零有效声明**。别用它。

### B4. `kern/sema.h` 是废壳

21 行，只有一句 `#pragma prompt(...请改为包含<kern/thread/sync.h>)`（:11），再转 include（:16）。

### B5. `kern/osl/filesys.h` 目录遍历全在注释里

51 行，`:12-49` 的 `_XFindFirstW`/`_XFindNextW`/`_XFindClose` 被 `/* */` 整段注释掉。目录遍历请用 `kbase/src/file/filefind.cpp` 的抽象。

### B6. `kern/thread/tls.h` 与 `kern/thread/process.h` 是陷阱（重要）

```
kern/thread/tls.h:28-31    声明 _XTlsAlloc/_XTlsFree/_XTlsSetValue/_XTlsGetValue
                 :35-38    **无条件** #define TlsAlloc() _XTlsAlloc() 等
kern/thread/process.h:23-30 声明 _XCreateThread
                    :32-33  **无条件** #define CreateThread _XCreateThread
```

但它们的实现体：
- `kskern/thread/synch.cpp` 全文 10 行，只有 `#include "stdafx.h"`，**零实现**
- `kskern/thread/tls.cpp`（Linux 走 `pthread_key_t`，Windows 分支转调真 Win32）
- `kskern/thread/thread.cpp:84-95` Windows 分支 = `REPORT("CreateThread - Not Impl"); return NULL;`
- **这三个 cpp 都不在 kskern 构建里**（`kskern/CMakeLists.txt:6-22` 源清单无 `thread/`）
- `kskern.def:61-75` 把对应 `_X` 符号导出全注释掉

后果：跨平台代码里调 `TlsAlloc()` / `CreateThread()` → 要么链接期 `undefined reference`，要么（Windows 上）拿到 `NULL`。

**处理**：线程/TLS 用 C++ 抽象——`kern/thread/thread.h` 的 RAII 类（`ScopedThreadPriority` 等），或直接 `std::thread` + `thread_local`。

### B7. `kern/win98/winver.h` 不是「判断 Win98」

`:13` `#if defined(_WIN32)||defined(WIN32)||defined(__WINDOWS__)` → `:15` `#define X_OS_WIN98`
即**只要在 Windows 上编译就定义**。别拿 `X_OS_WIN98` 做任何平台/版本判断。

---

## C. 字符编码类（最高频）

### C1. `wcscmp` 类型不匹配（历史级事故）

实测提示形如：

```
error: no matching function for call to 'wcscmp'
note: candidate function not viable: no known conversion from 'const char16_t *' to 'const wchar_t *'
```

根因：POSIX 上 `WCHAR = char16_t`（`kern/typedef.h:224`），而 `wcscmp` 吃 `wchar_t*`（32 位）。
曾经一次代码生成直接产出 `wcscmp`，一次性产生 **198 处** 该错误。

**处理**：一律 `xstrcmp`（Windows→`wcscmp`，POSIX→`_Xu2_strcmp`，见 `cstring_posix.h:900`）。
同类：`wcslen`→`xstrlen`/`ks_wcslen`、`wcscpy_s`→`xstrcpy_s`、`wcschr`→`xstrchr`、`swprintf`→`xsprintf`。

### C2. 裸 `L"..."`

`L"abc"` 在 POSIX 上是 `wchar_t[4]`（32 位字符），赋给 `LPCWSTR`(=`const char16_t*`) 直接编译失败。
**处理**：`__X("abc")`（`kglobal_def.h:334-345`）。

### C3. `wchar_t` 当 `WCHAR`

两者在 Windows 上恰好都是 16 位（能编过、能跑），在 POSIX 上一个 32 位一个 16 位 —— **编译期不一定报错，运行期字符串截断/乱码/越界**。这是最隐蔽的一类。
**处理**：不要写 `wchar_t`/`std::wstring`/`std::wstring_view` 处理 `WCHAR` 数据；用 `WCHAR`/`std::u16string_view`。

### C4. `LPTSTR` 与 `TEXT()` 的 UNICODE 分叉

`kern/typedef.h:245-274` 的 `TCHAR`/`LPTSTR`/`TEXT` 依赖 `UNICODE` 宏；WPS 的工程基本全 UNICODE。新代码别用 `TCHAR` 家族，直接用 `WCHAR`。

---

## D. 整数宽度类

### D1. `long` 当 32 位

- Windows: `sizeof(long) == 4`
- Linux x64 / macOS: `sizeof(long) == 8`

直接后果：`LONG` 参与的 `struct` 布局、序列化格式、`printf("%d")`、以及所有「用 long 存索引」的地方在 POSIX 上全错。

**处理**：
- 32 位整数 → `INT32`/`UINT32`
- 索引/计数 → `SIZE32`/`USIZE32`（两边都是 32 位，`kern/typedef.h:182-185` POSIX、`kglobal_def.h:68-71` Windows 上是 `long`，但同为 32 位）
- 指针宽度 → `intp`/`uintp`（`kglobal_def.h`），或 `intptr_t`

### D2. SAFEARRAY 成员为什么必须 `UINT32`

`kern/oaidl.h:137-153`：

```c
typedef struct tagSAFEARRAYBOUND { UINT32 cElements; INT32 lLbound; } SAFEARRAYBOUND;
typedef struct tagSAFEARRAY {
    USHORT cDims; USHORT fFeatures;
    UINT32 cbElements;   /* 不是 ULONG */
    UINT32 cLocks;       /* 不是 ULONG */
    PVOID  pvData; SAFEARRAYBOUND rgsabound[1];
} SAFEARRAY;
```

如果照 MSDN 抄成 `ULONG`，POSIX 上结构体会胖 8 字节，**跨模块传 SAFEARRAY 立刻错位崩溃**。
写自己的跨平台结构体时同理：**任何可能跨 ABI 边界的结构体，成员一律用固定宽度类型**。

### D3. `V_INT_PTR` 的 32/64 分叉

`kern/oleauto.h:107-117`：64 位下 `V_INT_PTR` 映射到 `V_LLVAL`，32 位下映射到 `V_LVAL`。手写 `v.lVal` 存指针在 64 位上会截断。

---

## E. VARIANT / SAFEARRAY / BSTR 语义

### E1. BSTR 不保证是 2 字节字符

`kern/oleauto.h:142` 的原注释：「不要假设BSTR为双字节，可能是4字节Unicode」。
所以 `SysStringLen()` 返回的是**字符数**（不是字节数），`SysStringByteLen()` 才是字节数。二者别混用。

### E2. BSTR 的分配器必须是 `CoTaskMem*`

`kskern/bstr/bstr.cpp:26-41` 用 `CoTaskMemAlloc`，:43-61 用 `CoTaskMemRealloc`。
**不要** `new`/`malloc` 出一块内存塞给 `BSTR`，也不要用 `free` 去放 `SysAllocString` 的产物。

### E3. SAFEARRAY 描述符前有隐藏槽位

`kskern/safearray/safearray.cpp`：
- `:53-64` 隐藏 DWORD 存在 `((LPDWORD)psa)[-1]`
- `:66-86` `SafeArrayGetIID/SetIID` 把 GUID 存在 `((GUID*)psa)[-1]`
- `:12-14` 自定义标志 `FADF_DATADELETED` 0x1000、`FADF_CREATEVECTOR` 0x2000、`SAFEARRAY_HIDDEN_SIZE` = `sizeof(GUID)`

**不要自己 `malloc(sizeof(SAFEARRAY))`**，必须走 `SafeArrayCreate`/`SafeArrayAllocDescriptor`。

### E4. `SafeArrayGetVTSize` 的 `VT_RECORD` 固定 32

`safearray.cpp:19-50`：`VT_RECORD` 元素大小硬编码 32。存放自定义 UDT 时，请确认与 Windows 行为一致，别假设是 `sizeof(你的结构)`。

---

## F. 历史包袱 / 陈旧代码

| 位置 | 问题 | 建议 |
|---|---|---|
| `kern/clx/typedef.h:102` | 裸 `#endif` 后面 `:106` 又有一个 `#endif`，clang 报 `#endif without #if`；`:67-73` 用 `X_ENCODE_*` 选 `wchar_t`/`char16_t`；`#pragma pack(push,1)` 未 pop | 半废弃文件。**不要照着写新代码**，用 `kern/typedef.h` |
| `kbase/com/kguiddef.h:52-56` | 源码注释自承：`__iid`/`__clsid` 在 `guid.h` 里另有一份定义且定义体不同，「这种自相矛盾的现象在 KFC 里面很多地方都存在」 | 统一用 `__iid(IX)`，不要自己再定义一份 |
| `kbase/com/kcomdef.h:9` | `// #include <kern/oleauto.h>` 被注释掉 | 历史残留，别恢复 |
| `kern/wingdi.h:432` | 注释写 `defined(X_OS_WINDOWS)`，实际 gate 是 `:11 #if defined(_WIN32) && !defined(USES_KSKERN)` | **以代码为准，别信注释** |
| `kskern/thread/synch.cpp` | 10 行空文件，「同步实现在 synch.cpp」是误传 | 真同步实现见 B1 |
| `kskern/memory/malloc.cpp` 与 `kbase/src/malloc/fastalloc.cpp` | `_XFastAllocate` 等有两处重复实现（`malloc.cpp:119/125/131` 与 `fastalloc.cpp:10`） | 改动前先确认哪份被链进目标 |
| `kbase/string/_/cstring_def.h:54-70` | 与 `kglobal_def.h` 重复的垫片 | 见 A3 |

---

## G. 观察到的编译噪音（可以无视）

调试时 clangd 会对整个 kern 目录刷出大量：

```
warning: Macro name is a reserved identifier        ← __KERN_TYPEDEF_H__ 之类的双下划线宏
warning: Identifier '_Xu2_strcmp' is reserved ...    ← _X 前缀全局符号
warning: Macro is not used
```

这些是**命名规范噪音**，不是错误。整套兼容层的历史命名就是 `_X`/`__` 开头，改不动也不用改。

---

## H. 一条经验

跨平台 bug 的分布极不均匀：绝大多数集中在这四处 ——
1. 宽字符串（`L` / `wcs*` / `wchar_t`）
2. 整数宽度（`long` / 结构体成员类型）
3. 结构体 ABI（缺固定宽度类型、`#pragma pack` 不配对）
4. 链接（`kskern` / `libsafec` 没链）

改完代码**先编 OHOS 或 Linux**，Windows 编过不代表正确。
