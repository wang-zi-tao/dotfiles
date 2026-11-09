# 链接与构建：为什么「头文件编过、链接挂了」

> 路径相对仓库根 `D:\branch2\wpsmain`。

## 1. 三个 package 的分工（已核对 CMakeLists）

| package | 类型 | 定义位置 | 作用 |
|---|---|---|---|
| `kbase` | **SHARED** | `Coding/foundation_bundle/kbase/CMakeLists.txt:1` → `wps_package(kbase SHARED)` | 抽象层 + 一批 `_X*` 实现的真正落地（如 `src/kern/sync.cpp`） |
| `kskern` | **STATIC** | `Coding/foundation_bundle/kfc_base/kskern/CMakeLists.txt:25` → `wps_package(kskern STATIC)` | POSIX 上 Win32/OLE 的仿真实现库 |
| `kskern_noqt` | **STATIC** | `kfc_base/kskern/noqt/CMakeLists.txt:1` | 不依赖 Qt 的变体（复用 `../kskern.cpp` 等） |

kskern 只在类 Unix 上构建：

```cmake
# Coding/foundation_bundle/kfc_base/CMakeLists.txt:3
wps_add_subdirectory(kskern LINUX BSD DARWIN)
```

Windows 上根本不存在这个 target，所以**任何 `wps_link_packages(kskern)` 都必须包在 `UNIX(...)`/`LINUX(...)` 里**。

## 2. kskern 到底编了哪些文件

`kfc_base/kskern/CMakeLists.txt:6-22` 的显式源清单：

```
PCH stdafx.h stdafx.cpp
kskern.cpp            kskern.def
bstr/bstr.cpp
com/cocreateguid.cpp  com/errorinfo.cpp
fmtguid/fmtguid.cpp
safearray/safearray.cpp
memory/malloc.cpp     memory/malloc.h  memory/mallocimp.h  memory/taskmalloc.cpp
variant/variant.cpp   variant/propvariant.cpp
time/time.cpp
```

⚠️ **注意：没有 `thread/` 目录**。磁盘上确实存在 `kskern/thread/{synch.cpp,thread.cpp,tls.cpp}`，但它们不在构建里：

- `thread/synch.cpp` 全文 10 行，只有 `#include "stdafx.h"`，**零实现**
- `thread/tls.cpp`、`thread/thread.cpp` 里的 Windows 分支分别是转调真 Win32 / `REPORT("CreateThread - Not Impl"); return NULL;`
- `kskern.def:61-75` 把对应的 `_XTls*`/`_XCreateThread` 导出全注释掉了

结论：**用到 `TlsAlloc`/`CreateThread` 的跨平台代码拿不到实现**（Windows 上还可能直接拿到返回 NULL 的桩）。要用线程请走 C++ 封装（`kbase/kern/sync.h` + std::thread / `kern/thread/thread.h` 的 RAII 类），见 `ref-gotchas.md`。

依赖声明：`kfc_base/kskern/CMakeLists.txt:37`

```cmake
wps_use_packages(Qt5Core LINUX(libsafec))
```

即 kskern 自己依赖 libsafec（`vmemcpy_s`/`u_strlen` 等安全函数）。

## 3. kbase 与 kskern 的关系

```cmake
# kbase/CMakeLists.txt:1
wps_package(kbase SHARED)
...
# :12-15
wps_link_packages(
    UNIX(kskern)
    algdiag
)
# :29
wps_use_packages(LINUX(libsafec) UNIX(Qt5Core) ANDROID(Qt5AndroidExtras gperftools) WIN(kimalloc) ks-async ks-network)
```

要点：
- kbase 是 **SHARED**，kskern 是 **STATIC**。静态库被吸进 kbase 后，**符号只存在于 kbase 内部**，不会因为有别的 target link 了 kbase 就自动可用。
- 因此 `kbase` 不能当作「kskern 的转发层」。用到 `SafeArray*`/`SysAllocString`/`Variant*` 的 target **必须自己链接 kskern**。

## 4. 真实调用点（业务侧都得自己写）

`Coding/api_bundle` 下至少这些 target 显式链接了 kskern：

| 文件 | 行 | 写法 |
|---|---|---|
| `Coding/api_bundle/api/jsapi/asyncjsapi/et/CMakeLists.txt` | :25 | `UNIX(kskern)` |
| `Coding/api_bundle/api/jsapi/ksowebjscore/CMakeLists.txt` | :133 | `UNIX(kskern)` |
| `Coding/api_bundle/api/jsapi/ksojscore/CMakeLists.txt` | :132 | `UNIX(kskern)` |
| `Coding/api_bundle/api/jsapi/jsapibrowser/CMakeLists.txt` | :59 | `UNIX(kskern)` |
| `Coding/api_bundle/api/jsapi/jsapirender/CMakeLists.txt` | :72 | `kskern`（裸写，靠 target 只在 Unix 存在） |
| `Coding/api_bundle/api/jsapi/jsapiextend/CMakeLists.txt` | :28 | `kskern` |
| `Coding/api_bundle/api/jsapi/jswebservice/CMakeLists.txt` | :69 | `kskern` |
| `Coding/api_bundle/api/jsapi/jswebservice/CMakeLists.txt` | :69 | `kskern` |
| `Coding/api_bundle/api/jsapi/jscefservice/CMakeLists.txt` | :187 | `kskern` |
| `Coding/api_bundle/api/jsapi/jscefservice/ipc/CMakeLists.txt` | :40 | `UNIX(kskern)` |

> 推荐写法：`UNIX(kskern)`——语义明确、Windows 上自动剔除，不会有 target 不存在的风险。

## 5. 标准补丁模板

目标模块出现 `undefined reference to _MSafeArrayCreate` / `_XSysAllocString` / `_MVariantClear` 时：

```cmake
wps_package(my_module SHARED)
    wps_include_directories(Coding/foundation_bundle/include)
    wps_link_packages(
        UNIX(kskern)          # ← 补这一行
    )
    wps_use_packages(LINUX(libsafec))   # ← kskern 的传递依赖，OHOS/Android 常漏
wps_end_package()
```

验证顺序：
1. 编 Linux 或 OHOS（Windows 编过毫无参考价值）
2. 看是否有 `vmemcpy_s`/`u_strlen` 未定义 → 缺 `libsafec`
3. 看是否有 `_XCreateThread`/`_XTlsAlloc` 未定义 → 不是链接问题，是**不该用这些 API**（见 `ref-gotchas.md`）
4. 静态库被整段丢弃（符号明明实现了却 undefined）→ 检查有没有 `_force_link_*` 锚点，参考 `kskern/bstr/bstr.cpp:123`

## 6. 构建命令

```bash
# Windows x86 debug
cd ../debug/ && krepo-ng build <module>

# OHOS x64 debug（最容易暴露跨平台问题）
export OHOS_NDK_HOME=D:\ohos_sdk_windows\default\openharmony
cd ../debug-ohos/ && krepo-ng build <module>
```

构建工具链相关细节走 `wps-build-system` skill；CMake 宏规范走 `wps-cmake-bundle` skill。
