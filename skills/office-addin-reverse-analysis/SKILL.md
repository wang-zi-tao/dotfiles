---
name: office-addin-reverse-analysis
description: 逆向分析 Office JS加载项（Excel TaskPaneApp + SharedRuntime）。
---

# Office 加载项（Excel）逆向分析

逆向分析 Excel Office 加载项（TaskPaneApp + CustomFunctions + SharedRuntime）。适用于从清单 URL 开始，下载、格式化、理解并沉淀加载项的全部代码与交互机制。

## 适用场景

- 用户给出 Office 加载项 manifest.xml URL，要求分析加载项实现
- 需要分析 `CustomFunctions.associate` 自定义函数的执行流程
- 需要分析 Excel.js（`Excel.run` / `getRangeByIndexes` / `setDirty` / `notes`）异步批处理
- 需要分析 SharedRuntime 架构（自定义函数与任务窗格共享运行时）
- 需要与 WPS 版加载项做对比分析
- 需要将分析结果沉淀为文档并 git 提交

## 前置约定（强制）

- **分析全程用 git 记录**：每个阶段（下载、格式化、定位注册、流程追踪、报告）完成后立即 `git add` + `git commit`
- **所有下载的 JS 必须格式化**：统一用 `npx --yes prettier@3.3.3 --parser babel --print-width 200 <input> > <input>.formatted.js`
- 下载文件放入 `excel/` 目录（保持 URL 相对结构：`js/`、`css/` 子目录）
- 分析报告写入 repo 的 `docs/` 或 Obsidian 笔记库
- 本仓库无需编译/测试/lint

## 标准工作流

### Step 1: 下载 manifest.xml

用户提供的 URL 通常是 manifest.xml：

```bash
curl -s "<URL>/manifest.xml" -o excel/manifest.xml
```

### Step 2: 解析清单提取资源 URL

提取关键节点（Office Add-in 标准）：

```bash
findstr /n "SourceLocation\|Script.Url\|Metadata.Url\|Taskpane.Url\|bt:Url" excel/manifest.xml
```

必提字段：
- `Id` / `Version` / `ProviderName` / `DisplayName`
- `Requirements > Sets`（如 SharedRuntime 1.1）
- `DefaultSettings > SourceLocation`（任务窗格页面入口）
- `VersionOverrides > CustomFunctions`：`Functions.Script.Url`（函数脚本）、`Functions.Metadata.Url`（函数元数据）、`Taskpane.Url`
- `Hosts` / `ExtensionPoint xsi:type="PrimaryCommandSurface"`（Ribbon 按钮 ExecuteFunction 动作名）

### Step 3: 下载函数元数据 + 页面入口

```bash
curl -s "<Functions.Metadata.Url>" -o excel/functions.json
curl -s "<Taskpane.Url>" -o excel/taskpane.html
```

**注意**：manifest 中 Script/Metadata URL 可能 404（部署路径漂移）。从 taskpane.html 的 `<script src>` 推断真实 CDN 路径（通常在 `s.thsi.cn/cd/<container>/xlsslug-excel/` 下），逐 URL 探测。

### Step 4: 解析 taskpane.html 提取 bundle

```bash
findstr /n "script src\|href=" excel/taskpane.html
```

典型产物：`js/chunk-vendors.<hash>.js`（2MB 级）、`js/chunk-common.<hash>.js`（175KB 级）、`js/taskpane.<hash>.js`（160KB 级）+ CSS。

### Step 5: 下载全部 JS 并格式化

```bash
curl -s "<url>" -o excel/<file>.js
npx --yes prettier@3.3.3 --parser babel --print-width 200 "excel/<file>.js" > "excel/<file>.formatted.js"
```

minified 单行文件 grep 无法给出上下文，**必须格式化**。原始文件保留（grep 精确定位字符偏移用）。

### Step 6: 定位懒加载 chunk（动态 import）

主 bundle 中 `n.e(<chunkId>)` 表示懒加载。chunk 文件名映射在 webpack runtime：

```
搜索 "n.u=function" → 如 "js/"+e+".84f0cfe0.js"（共享 hash）
→ 下载 js/<chunkId>.<hash>.js
```

示例：`n.e(170).then(n.bind(n, 39170))` → 下载 `js/170.84f0cfe0.js`。**懒加载 chunk 常含自定义函数实现副本或核心请求队列（SharedRuntime 共享）**。

### Step 7: 定位自定义函数注册

在格式化后的 bundle 中搜索：

```
grep -n "CustomFunctions.associate\|Office.actions.associate" *.formatted.js
```

两种注册并存：
- `CustomFunctions.associate("HXIFIND", handler)` — 工作表自定义函数（ID 全大写）
- `Office.actions.associate("OpenTaskpane", handler)` — Ribbon 命令（manifest ExecuteFunction 动作名）

对照 `functions.json` 建立注册表（注意 `dimensionality: "matrix"`、`stream`、`result.type` 等 Office 标准字段）。

### Step 8: 追踪函数执行流程

每个函数入口共享模式（Office 版）：

```js
function Oe(e, t, ..., m) {                       // handler
    return me(this, void 0, void 0, function* () {
        try {
            if (yield (0, de.RI)(), !(0, r["do"])()) return null;  // ① 鉴权 ② 环境检测
            const y = yield fe.l(e, t, ..., m);                     // ③ 核心逻辑
            return y;
        } catch (y) { return y }
    })
}
```

- `RI()` = 鉴权/登录检查；`do()` = 环境检测（常检查 `jgbsessid`/`userid`/`User-Agent`）
- 核心逻辑可能在独立模块（如 50764）或懒加载 chunk

**批量机制**（hxiFinD 类，重点，与 WPS 版同构）：
- `reqMap`（paramKey 去重）+ 分类队列（基金/债券/普通）
- 100ms `setInterval` 稳定检测（500ms 或 >1000 条或 ≥3000 分片）
- `paramExcel=` + 分号拼接 → 1 个 HTTP POST 批量请求
- 回写：`Excel.run` 内切手动计算 → `setDirty()`/`calculate()`

**RTD 流式**：`invocation.setResult(data)` + interval（refreshRate 默认 3s）+ `onCanceled` 清理。

### Step 9: 分析 Excel.js 异步批处理（表格访问）

```js
Excel.run((ctx) => {
  const sheet = ctx.workbook.worksheets.getItemOrNullObject(name);
  const range = sheet.getRange(addr);
  range.load(["rowIndex", "columnIndex"]);
  return ctx.sync();   // 一次性提交所有命令
});
```

- 基于 `invocation.address`（`'Sheet1'!$A$1`）解析 sheet/range
- 写入：`range.values` / `range.numberFormat[Local]` / `range.clear()` / `range.formulas`
- 附加能力：`workbook.notes`（批注）、`conditionalFormats`（条件格式）、`getUsedRange().calculate()`

### Step 10: 分析网络层

搜索 `fetch(`，记录：并发队列（如 6 路 `class l`）、请求体编码（如 gzip `el()`）、401/403 登出、自定义 header（`User-Agent`/`ifindlang`）。

### Step 11: 分析平台分支

搜索 `Office.context.platform` / `Office.PlatformType`：Online/Mac/Win 分支（如 OfficeOnline 附加 `jgbsessid`、URL `&type=` 变体）。

### Step 12: 沉淀报告 + git 提交

- 报告写入 `docs/<topic>.md`，结构：资产清单 / 架构差异 / 注册表 / 执行流程时序 / jsapi API 映射 / 网络层 / 性能观察
- 每阶段 git commit

## 与 WPS 版对比分析（可选）

若仓库同时含 WPS 版（`wps-addin-reverse-analysis` 技能覆盖），对比维度：
1. 运行时模型：JsPlugin 双运行时 vs SharedRuntime 单运行时
2. 注册 API：`wps.AddCustomFunction`（驼峰 ID）vs `CustomFunctions.associate`（大写 ID）
3. 表格访问：`window.Application` COM 同步 vs Excel.js 异步批处理
4. 传输层：api.js 三模式 vs office.js + fetch
5. 批量机制：两端同构（paramKey 去重 + 100ms 定时器 + 分号拼接）
6. 网络：Office 常带并发队列 + gzip

## 参考

- `references/office-sharedruntime-notes.md` — SharedRuntime 架构与注册/初始化要点
- `references/example-office-layout.md` — 典型 Office 加载项文件布局与 webpack chunk 分析
