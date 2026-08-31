---
name: wps-addin-reverse-analysis
description: 逆向分析 WPS 加载项（JsPlugin）
---

# WPS 表格加载项逆向分析

逆向分析 WPS 表格组件（ET）的 JsPlugin 加载项。适用于从页面 URL 开始，下载、格式化、理解并沉淀加载项的全部代码与交互机制。

## 适用场景

- 用户给出 WPS 加载项页面 URL（`index.html`），要求分析加载项实现
- 需要分析 `wps.AddCustomFunction` 自定义函数的执行流程
- 需要分析 jsapi 调用（`api.js` 传输层、`window.Application` COM API、apievent）
- 需要分析 hxiFinD 类批量请求机制、RTD 流式刷新机制
- 需要将分析结果沉淀为文档并 git 提交

## 前置约定（强制）

- **分析全程用 git 记录**：每个阶段（下载、格式化、定位注册、流程追踪、报告）完成后立即 `git add` + `git commit`，commit message 用中文描述该阶段产物
- **所有下载的 JS 必须格式化**：统一用 `npx --yes prettier@3.3.3 --parser babel --print-width 200 <input> > <input>.formatted.js`
- 下载文件按原始 URL 的相对路径组织（静态资源在 `js/`、`css/` 子目录；核心文件放仓库根或 `assets/`）
- 分析报告写入 repo 的 `docs/` 或 Obsidian 笔记库
- 本仓库无需编译/测试/lint

## 标准工作流

### Step 1: 下载页面入口

从用户提供的 URL 下载 `index.html`：

```bash
curl -s "<URL>/index.html" -o index.html
```

### Step 2: 解析资源清单

从 `index.html` 提取全部 `<script src>` 与 `<link href>`：

```bash
findstr /n "script src\|href=" index.html
```

典型产物：
- `manifest.xml`（JsPlugin 清单，226B 级）→ 函数注册指向 `/functions.json`
- `ribbon.xml`（Ribbon UI 定义）
- `functions.json`（自定义函数元数据，含 WPS 专有 `stream` / `requiresAddress` 选项）
- `api.js`（**未混淆的 jsapi 传输层**，核心分析对象）
- `functions-<hash>.js`（自定义函数实现，`wps.AddCustomFunction` 注册）
- `index-<hash>.js`（主 bundle：Vue app + 批量机制 + UI 交互）
- 其余：`chameleon*.js`（主题）、`ta.min.js`（埋点）、CSS、`xlsEDBItem-*.js`（经济数据库模块）

### Step 3: 批量下载

逐个 `curl -s <URL> -o <本地路径>`，保持子目录结构。对 minified 单行 JS，先下载原文件，再格式化生成 `.formatted.js`（原始文件保留，方便 diff 与 grep 精确字符定位）。

### Step 4: js 格式化

```bash
npx --yes prettier@3.3.3 --parser babel --print-width 200 "xxx.js" > "xxx.formatted.js"
```

minified 文件是单行，`grep` 无法给出行级上下文，**必须格式化后才能高效分析**。格式化后行数暴涨，用 grep 定位后按行号 Read。

### Step 5: 定位自定义函数注册

在格式化后的函数 bundle 中搜索注册 API：

```
grep -n "AddCustomFunction" xxx.formatted.js
```

典型输出（13 个函数，id 驼峰小写）：

```js
wps.AddCustomFunction("", "hxRTD", (u,t,e,r,i,a,s)=>Mt(u, t, e, r, i, a, s));
wps.AddCustomFunction("", "hxiFinD", (u,t,e,r,i,a,s,n,l,h,c,f,o,m)=>Gt(u, t, e, r, i, a, s, n, l, h, c, f, o, m));
...
```

对照 `functions.json` 中的 options（`stream`、`requiresAddress`）建立注册表。

### Step 6: 追踪函数执行流程

每个函数入口共享模式（在格式化文件中搜索对应 handler）：

```js
async function Gt(u, t, ..., m) {
    try {
        return await O(),                                  // ① 鉴权/登录检查（main bundle）
        T() ? it(u, t, ..., m) : null                      // ② 表格环境检测 → 桥接核心逻辑
    } catch (g) { return g }
}
```

- `O()` = 登录/鉴权 check；`T()` = 是否在 WPS 表格环境内检测（均在 main bundle）
- 核心逻辑可能跨 bundle（桥接 main bundle 的 `it()` 等）

**批量机制**（hxiFinD 类函数，重点）：
- `reqMap`（paramKey 去重）+ 分类队列（基金/债券/普通）
- 100ms `setInterval` 稳定检测（500ms 或 >1000 条或 ≥3000 分片）
- `paramExcel=` + 分号拼接 → 1 个 HTTP POST 批量请求
- 结果回写：`Range.Dirty()` / `Range.Calculate()` + 手动计算模式切换

**RTD 流式**（stream 函数）：
- `invocation.setResult(data)` 推送 + `setInterval`（refreshRate 默认 3s）
- LCD 码转换（GetTHSCodes API）
- `onCanceled` 清理

### Step 7: 分析 jsapi 传输层（api.js）

优先级三模式（未混淆，直接读）：
1. `window.cefQuery` — CEF 环境，异步
2. `window.external[method]()` — COM，同步（按参数个数 switch）
3. `window.app.sendMessage` — 异步消息

API 方法名列表见 `api.js` 末尾 `h` 变量（逗号分隔字符串，~120+ 方法）。

### Step 8: 分析表格访问（COM jsapi）

WPS 版通过 COM 同步 API 访问表格：

```js
window.Application.ActiveWorkbook.Worksheets.Item(name)
sheet.Range.Value2 / NumberFormat / Clear() / Formula
sheet.Range.Dirty() / Calculate()
Application.Calculation = xlCalculationManual
```

### Step 9: 分析网络层

搜索 `fetch(` / `XMLHttpRequest`，记录：POST 体编码、401/403 登出处理、自定义 header（`ifindlang` 等）。

### Step 10: 沉淀报告 + git 提交

- 报告写入 `docs/<topic>.md`，结构：注册概览表 / 逐函数调用链 / jsapi 调用汇总 / 网络请求 / 性能关注点
- 每阶段 git commit（Step 1-10 各一次或按逻辑合并，message 明确）

## 分析关注点清单

1. WPS jsapi 使用情况（api.js 方法调用统计）
2. 低性能 jsapi 调用（同步 COM 高频调用、循环内重新获取 ActiveWorkbook）
3. 自定义函数中嵌套 jsapi 调用
4. apievent 回调中嵌套 jsapi 调用
5. 加载项与 WPS 自动相互调用规模及优化方法

## 参考

- `references/wps-analysis-notes.md` — 仓库既有分析要点（custom-functions-analysis / hxifind-batch-mechanism / jsapi-event-analysis 摘要）
- `references/example-wps-layout.md` — 典型 WPS 加载项文件布局示例
