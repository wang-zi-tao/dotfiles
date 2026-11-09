# WPS 兼容层 API 映射表（含行号）

> 所有路径相对仓库根 `D:\branch2\wpsmain`。头文件根 `Coding/foundation_bundle/include/`，实现根 `Coding/foundation_bundle/kfc_base/kskern/`。

## 0. 命名前缀规则

| 前缀 | 含义 | 例子 |
|---|---|---|
| `_X...` | WPS 自造的语义实现（X = WPS 旧代号） | `_XSysAllocString`、`_XCreateErrorInfo`、`_XTlsAlloc` |
| `_M...` | Microsoft 语义的镜像实现 | `_MVariantClear`、`_MSafeArrayCreate` |
| `_Xu2_...` | UTF-16（char16_t）字符串/内存函数 | `_Xu2_strcmp`、`_Xu2_memcpy` |
| 宏重定向 | 把微软名字映射到上面两类 | `#define SysAllocString _XSysAllocString` |

结论：**业务代码只认微软名字**。`_M`/`_X` 前缀名只出现在 kern 头和 kskern 实现里。

---

## 1. 基础类型（kern/typedef.h）

文件门闩：`kern/typedef.h:9` → `#if !defined(__KERN_TYPEDEF_H__) && !defined(_WINDEF_)`

| 类型 | 定义 | 行 |
|---|---|---|
| `CDECL` / `PASCAL` | `__cdecl` / `__stdcall` | :37-52 |
| `FAR` / `NEAR` / `CONST` | 空 / 空 / `const` | :37-52 |
| `MAX_PATH` | Linux=`PATH_MAX`、Darwin=1024、其他=260 | :54-70 |
| `interface` | `struct` | :77-81 |
| `ULONG` | `unsigned long` | :83-100 |
| `LONG` | `long` | :83-100 |
| `VARTYPE` | `unsigned short` | :105 |
| `VARIANT_BOOL` | `short` | :106 |
| `SCODE` | `int` | :107 |
| `DWORD` | `UINT32` | :140 |
| `BOOL` | `int` | :141 |
| `BYTE` / `WORD` | `unsigned char` / `unsigned short` | :142-143 |
| `size32` / `usize32` | `int32` / `uint32` | :182-183 |
| `SIZE32` / `USIZE32` | `size32` / `usize32` | :184-185 |
| `HRESULT` | `INT32`（`_HRESULT_DEFINED` 保护） | :204 |
| `WCHAR` | `char16_t` | :224 |
| `PWCHAR`/`LPWSTR`/`LPCWSTR`/`LPCOLESTR`/`OLECHAR` | `WCHAR*` 家族 | :226-242 |
| `TCHAR`/`LPTSTR`/`TEXT`/`__TEXT` | UNICODE 分支 | :245-274 |
| `RECT`/`POINT`/`SIZE`/`FIXED`/`LARGE_INTEGER`/`FILETIME`/`CY`/`DECIMAL` | 结构体 | :278-416 |
| `HANDLE` | `void*` | :428 |
| `HINSTANCE`/`HMODULE` | 句柄 | :430-432 |
| `STDAPI`/`STDAPI_`/`STDAPICALLTYPE`/`EXPORTAPI` | 调用约定宏 | :444-455 |
| `VARIANT_TRUE`/`VARIANT_FALSE` | `(VARIANT_BOOL)0xffff` / `0` | :466-469 |

宽度断言：`kern/typedef.h:190-200`（`sizeof(DWORD)==4` 等）、`:418` `sizeof(DECIMAL)==16`。

---

## 2. COM / OLE 接口（kern/objidl.h, oaidl.h）

### 2.1 接口声明宏

`kern/objidl.h:13`：

```c
#define MIDL_INTERFACE_(x, Intrf)  DECLARE_IID(Intrf) struct Intrf
```

即：声明接口的同时声明它的 IID 常量（`DECLARE_IID` 定义在 `kbase/kglobal_def.h:769`）。

`_In_`/`_Out_`/`_In_opt_`/`_Reserved_` 等 SAL 注解在 :23-41 全部展开为空。

### 2.2 IUnknown

`kern/objidl.h:43-53`：

```c
MIDL_INTERFACE_("00000000-0000-0000-C000-000000000046", IUnknown)
{
    virtual HRESULT __stdcall QueryInterface(REFIID riid, void** ppvObject) = 0;
    virtual ULONG   __stdcall AddRef() = 0;
    virtual ULONG   __stdcall Release() = 0;
};
typedef IUnknown* LPUNKNOWN;      // :55
```

`IClassFactory` 在 :59-70。

### 2.3 VARENUM / SAFEARRAY（kern/oaidl.h）

门闩：`:10` `#ifndef X_OS_WINDOWS`

- `VARENUM` 枚举：`:77-131`（`VT_EMPTY`…`VT_RECORD` 全套）
- `SAFEARRAYBOUND`：`:137-141`

```c
typedef struct tagSAFEARRAYBOUND {
    UINT32 cElements;   /* 注意：不是 ULONG */
    INT32  lLbound;
} SAFEARRAYBOUND;
```

- `SAFEARRAY`：`:145-153`

```c
typedef struct tagSAFEARRAY {
    USHORT         cDims;
    USHORT         fFeatures;
    UINT32         cbElements;   /* 不是 ULONG */
    UINT32         cLocks;       /* 不是 ULONG */
    PVOID          pvData;
    SAFEARRAYBOUND rgsabound[1];
} SAFEARRAY;
```

**这是 ABI 设计的关键**：成员全用 `UINT32` 而非 `ULONG`，保证 32/64 位、Win/POSIX 布局完全一致（SAFEARRAY 会跨模块传递）。

- `FADF_*` 标志：`:157-168`
- `tagVARIANT` 的 union：`:238+`，`llVal`(INT64)/`lVal`(INT32)/`bstrVal`/`pdispVal`/`parray`/`byref` 等

### 2.4 VARIANT 访问宏（kern/oleauto.h）

双形态（`:61-73`）：

```c
#if defined(__STDC__) && !defined(_FORCENAMELESSUNION)
  #define V_VT(X)          ((X)->n1.n2.n3.vt)
#else
  #define V_VT(X)          ((X)->vt)
#endif
```

全部访问宏：`:77-138`（`V_ISBYREF/V_ISARRAY/V_ISVECTOR/V_UI1/V_I2/V_I4/V_I8/V_R4/V_R8/V_I1/V_UI2/V_UI4/V_UI8/V_INT/V_UINT/V_CY/V_DATE/V_BSTR/V_DISPATCH/V_ERROR/V_BOOL/V_UNKNOWN/V_VARIANTREF/V_ARRAY/V_ARRAYREF/V_BYREF/V_DECIMALREF`）。

**32/64 位分叉点**（`:107-117`）：

```c
#if defined(__x86_64__) || defined(__LP64__) || defined(__arm64__)
  #define V_INT_PTR(X)   V_LLVAL(X)
  #define V_UINT_PTR(X)  V_ULLVAL(X)
#else
  #define V_INT_PTR(X)   V_LVAL(X)
  #define V_UINT_PTR(X)  V_ULVAL(X)
#endif
```

---

## 3. BSTR（kern/oleauto.h :140-204）

源注释（`:142`）：「不要假设BSTR为双字节，可能是4字节Unicode」。

### 3.1 声明（`:147-154`）

```c
EXPORTAPI_(BSTR) _XSysAllocString(const OLECHAR* psz);
EXPORTAPI_(BSTR) _XSysAllocStringLen(const OLECHAR* psz, UINT32 ui);
EXPORTAPI_(BOOL) _XSysReAllocString(BSTR* pbstr, const OLECHAR* psz);
EXPORTAPI_(BOOL) _XSysReAllocStringLen(BSTR* pbstr, const OLECHAR* psz, UINT32 ui);
EXPORTAPI_(void) _XSysFreeString(BSTR bstr);
EXPORTAPI_(UINT32) _XSysStringLen(BSTR bstr);
EXPORTAPI_(UINT32) _XSysStringByteLen(BSTR bstr);
EXPORTAPI_(BSTR) _XSysAllocStringByteLen(const char* psz, UINT32 len);
```

### 3.2 名字重定向（`:156-171`）

| 微软 API | 兼容层实现 |
|---|---|
| `SysAllocString(psz)` | `_XSysAllocString(psz)` |
| `SysAllocStringLen` | `_XSysAllocStringLen` |
| `SysReAllocString` / `SysReAllocStringLen` | `_XSysReAllocString` / `_XSysReAllocStringLen` |
| `SysFreeString` | `_XSysFreeString` |
| `SysStringLen` / `SysStringByteLen` | `_XSysStringLen` / `_XSysStringByteLen` |
| `SysAllocStringByteLen` | `_XSysAllocStringByteLen` |

`:175-204` 是 `_MSys*` 形式的备选分支（含 C++ inline 包装）。

### 3.3 实现（kfc_base/kskern/bstr/bstr.cpp）

- `_XSysAllocString`（:26-41）：`ks_wcslen` 测长 → `CoTaskMemAlloc` → `ks_memcpy_s`
- `_XSysReAllocString`（:43-61）：`CoTaskMemRealloc`
- `_XSysAllocStringLen`（:63-70）：注释警告「cch如果为-1 转换为UINT会很大，访问内存就会崩溃」
- `:123` `_force_link_kskern_bstr`：防静态库被链接器丢弃（**如果你新增 kskern 源文件，记得加同类 force-link 符号，否则静态链接可能整段被丢弃**）

分配器语义：BSTR 用 `CoTaskMem*` 分配器，和 Windows 一致，可与 `CoTaskMemFree` 互操作。

---

## 4. VARIANT（kern/oleauto.h :206-256）

| 微软 API | 兼容层实现 | 行 |
|---|---|---|
| `VariantInit` | `_MVariantInit`（C++ inline 直接 `V_VT(pvarg)=VT_EMPTY`） | :216-241 |
| `VariantClear` | `_MVariantClear` | :235-241 |
| `VariantCopy` | `_MVariantCopy` | |
| `VariantCopyInd` | `_MVariantCopyInd` | |
| `VariantChangeType` | `_MVariantChangeType` | |
| `VariantChangeTypeEx` | `_MVariantChangeTypeEx` | |
| `VarCmp` | `_MVariantCompare` | |

`VARIANT_*` 标志位：`:243-256`。

时间转换（`:258-266`）：`_MVariantTimeToSystemTime` / `_MSystemTimeToVariantTime` + 宏。
实现：`kfc_base/kskern/variant/variant.cpp`、`variant/propvariant.cpp`、`time/time.cpp`。

---

## 5. SAFEARRAY（kern/oleauto.h :268-320）

### 5.1 声明（`:273-295`）

`_MSafeArray` + 动词：`AccessData`、`AllocData`、`AllocDescriptor`、`Copy`、`CopyData`、`Create`、`CreateVector`、`Destroy`、`DestroyDescriptor`、`DestroyData`、`GetDim`、`GetElemsize`、`GetElement`、`GetLBound`、`GetUBound`、`GetVartype`、`Lock`、`Unlock`、`PutElement`、`PtrOfIndex`、`Redim`、`UnaccessData`

`typedef SAFEARRAY* _SAFEARRAY_PTR;`（`:271`）

### 5.2 重定向（`:297-320`）

```c
#define SafeArrayCreate       _MSafeArrayCreate
#define SafeArrayDestroy      _MSafeArrayDestroy
#define SafeArrayGetElement   _MSafeArrayGetElement
#define SafeArrayPutElement   _MSafeArrayPutElement
/* ... 全部 22 个 ... */
/* SafeArrayCreate/CreateVector 带 (SAFEARRAY*) 强转 */
```

### 5.3 实现（kfc_base/kskern/safearray/safearray.cpp，943 行）

| 机制 | 位置 | 说明 |
|---|---|---|
| `FADF_DATADELETED` 0x1000 | :12 | 自定义标志 |
| `FADF_CREATEVECTOR` 0x2000 | :13 | 区分 Create / CreateVector |
| `SAFEARRAY_HIDDEN_SIZE` `sizeof(GUID)` | :14 | 隐藏区大小 |
| `SAFEARRAY_GetVTSize(VARTYPE)` | :19-50 | 每个 VT 的元素字节数（`VT_RECORD` 固定 32） |
| 隐藏 DWORD | :53-64 | `LPDWORD lpDw = (LPDWORD)psa; lpDw[-1]` |
| `SafeArrayGetIID/SetIID` | :66-86 | GUID 存在 `((GUID*)psa)[-1]` |
| `SAFEARRAY_GetCellCount` | :89 | 单元格总数 |

关键：**SAFEARRAY 描述符前有隐藏槽位**，所以任何「自己 malloc 一个 SAFEARRAY 结构」的代码都会破坏实现。必须走 `SafeArrayCreate` / `SafeArrayAllocDescriptor`。

---

## 6. 字符串 / 内存（kbase/string/）

### 6.1 双实现分流

| 平台 | 头 | `xstrcmp` 展开 |
|---|---|---|
| Windows | `kbase/string/_/cstring_win.h:121` | `wcscmp` |
| POSIX | `kbase/string/_/cstring_posix.h:900` | `_Xu2_strcmp` |

### 6.2 POSIX 重定向清单（cstring_posix.h :900-939）

| 兼容名 | 展开 | 行 |
|---|---|---|
| `xstrcmp` | `_Xu2_strcmp` | :900 |
| `xstricmp` / `xstrncmp` / `xstrnicmp` | `_Xu2_*` | :901-903 |
| `xstrlen` | `ks_wcslen` | :904 |
| `xstrcat_s` / `xstrncat_s` / `xstrcpy_s` / `xstrncpy_s` / `xstrnlen_s` | `_Xu2_*` 安全版 | :905-909 |
| `itox_s` / `ltox_s` / `ultox_s` | 数字转串 | :910-912 |
| `xsplitpath_s` / `xstrtok_s` | | :913-914 |
| `xstrupr_s` / `xstrlwr_s` | 仅 `X_BIT_64` | :915-918 |
| `toxlower` / `toxupper` | `_Xu2_tolower/toupper` | :921-922 |
| `xstrchr`/`xstrcspn`/`xstrdup`/`xstrpbrk`/`xstrrchr`/`xstrspn`/`xstrstr` | `_Xu2_*` | :923-933 |
| `xstrtod`/`xstrtol`/`xstrtoul` | `_Xu2_*` | :934-936 |
| `itox`/`ltox`/`ultox` | `_Xu2_itoa/ltoa/ultoa` | :937-939 |
| `xsprintf` | | :939 |

### 6.3 `_Xu2_*` 声明头（cstring_posix_u16.h，77 行）

- `:9` `typedef char16_t WCHAR;`
- `:11-16` `STDAPI_` / `EXPORTAPI_`（`EXPORTAPI_` = `extern "C" __attribute__((visibility("default"))) type __attribute__((stdcall))`）
- `:18` `#define interface struct`
- `:31-71` 全部原型：`_Xu2_memcmp/memcpy/memmove/memset/memchr/strcat/_Kso_strncat/strcpy/strncpy/strecpy/strchr/strrchr/strlwr/strupr/strdup/strlen/strcmp/stricmp/strncmp/strnicmp/strcspn/strspn/strpbrk/itoa/ltoa/ultoa/splitpath/strstr/strtod/strtol/strtoul/tolower/toupper/strtok`

---

## 7. 宽字符串字面量（kbase/kglobal_def.h）

| 宏 | MSVC | GCC/Clang |
|---|---|---|
| `__X(x)` | `L ## x` → `wchar_t*` | `(u ## x)` → `char16_t*` |
| `__Xc(x)` | 字符常量 | 字符常量 |
| `_Xs(x)` / `_Xc(x)` | 同上 | 同上 |
| `_Xu(x)` | — | `(u ## x)` |
| `OLESTR(x)` | — | `u ## x` |

位置：`:334-346`。

---

## 8. COM 对象实现宏（kbase/kcomobj.h）

| 宏 | 行 | 作用 |
|---|---|---|
| `BEGIN_QUERYINTERFACE(Interface)` | :118 | 转发到 `_IID` 版本 |
| `BEGIN_QUERYINTERFACE_IID(Interface, piid)` | :120-129 | 生成 `GetUnknown()` + `QueryInterface` |
| `ADD_INTERFACE(X)` / `ADD_INTERFACE_IID` | :147-152 | 追加支持的接口 |
| `ADD_INTERFACEEX` | :154-156 | 带偏移的接口 |
| `ADD_NEST_INTERFACE(_2)` | :158-167 | 嵌套接口 |
| `ADD_AGGREGATION` / `_BLINDLY` | :169-175 | 聚合 |
| `END_QUERYINTERFACE()` | :182-191 | `E_NOINTERFACE` 收尾 + AddRef/S_OK |
| `DECLARE_NO_QUERYINTERFACE()` | :208-213 | 返回 `E_NOTIMPL` |
| `DECLARE_CLASS(ComClass)` | :240-241 | 惰性构造 + 计数 |
| `DECLARE_COMCLASS(ComClass, Interface)` | :243-247 | **标准写法**：上面四个的组合 |
| `PROTECT_NEW(ComClass)` | :251-258 | 保护 new |
| `_ModuleLock/_ModuleUnlock` | :262-270 | DLL 计数 |

## 9. GUID / IID（kbase/com/kguiddef.h, kcomdef.h）

| 需求 | MSVC 展开 | GCC/Clang 展开 | 行 |
|---|---|---|---|
| `DEFINE_IID(IX, "...", ...)` | `interface __declspec(uuid("...")) IX;` | `NON_NATIVE_DEFINE_GUID` 模板特化 | kguiddef.h:21-24 / :34-37 |
| `DEFINE_CLSID` | `__declspec(uuid(...))` | 同上 | :25-29 / :38-41 |
| `__uuid(IX)` / `__uuidof(IX)` | `__uuidof` | `NON_NATIVE_UUIDOF(IX)` | :16-20 / :31-33 |
| `__iid(IX)` | `IID_##IX` | 同 | kguiddef.h:57-59 |
| `__clsid(C)` | `CLSID_##C` | 同 | kguiddef.h:61-63 |
| `DECLARE_GUID(var)` | `EXTERN_C const GUID var;` | 同 | kguiddef.h:46-48 |
| `ISEQUAL_IID(a,b)` | `InlineIsEqualGUID` | `_XInlineIsEqualGUID` | kcomdef.h:99 |
| `STDPROC` / `STDPROC_(T)` | `virtual HRESULT __stdcall` / `virtual T __stdcall` | 同 | kcomdef.h:20-23 |
| `DECLSPEC_UUID(x)` | 空 | 空 | kcomdef.h:44-46 |

---

## 10. 线程 / TLS / 时间（kern/thread*、kskern/thread、kskern/time）

| 头 | 内容 |
|---|---|
| `kern/thread/tls.h` | `_XTlsAlloc/_XTlsFree/_XTlsSetValue/_XTlsGetValue` 声明 + 重定向（:28-31） |
| `kern/thread/sync.h` | 同步原语 |
| `kern/thread/thread.h` | 线程类 |
| `kern/thread/process.h` | 进程 API |
| `kern/thread/kinstance.h` | 实例/模块句柄 |
| `kern/thread/kthreadsafefunctioncaller.h` | 线程安全调用器模板 |
| `kern/sema.h` / `kern/threads.h` | 信号量 / 线程池 |
| 实现 | `kskern/thread/tls.cpp`、`synch.cpp`、`thread.cpp`、`kskern/time/time.cpp` |

> Win32 同步/内存/文件系统部分的完整覆盖表见 `references/ref-architecture.md`。
