# 典型 Office 加载项文件布局与 webpack chunk 分析

以下为同花顺 iFinD Office 版（`xlsslug-excel`）真实布局，可作为分析模板：

```
manifest.xml                      OfficeApp 清单（TaskPaneApp + SharedRuntime）
functions.json                    函数元数据（14 个，id 大写 + LOG）
taskpane.html                     任务窗格入口
js/
├── chunk-vendors.<hash>.js       第三方库（vue 等，2MB 级）
├── chunk-common.<hash>.js        共享业务 chunk（公式兼容层、网络层、N 单例）
├── taskpane.<hash>.js            任务窗格 bundle（Ribbon 命令注册 + 初始化）
├── functions.js                  自定义函数脚本（manifest Script.Url 指向）
└── <chunkId>.<hash>.js           懒加载 chunk（自定义函数副本 / 核心请求队列）
css/
├── chunk-vendors.<hash>.css
├── chunk-common.<hash>.css
└── taskpane.<hash>.css
```

## webpack chunk 定位方法

minified bundle 中：

1. 懒加载调用：`n.e(170).then(n.bind(n, 39170))` → chunk id 170
2. 文件名规则：搜索 `n.u=function` 找到 `"js/"+e+".<hash>.js"`（可能共享 hash）
3. 下载：`js/170.<hash>.js`
4. 若 `n.u` 找不到，在原始（未格式化）文件中搜索 `170:` 的 hash 映射对象

## 关键 grep 模式

| 目标 | 模式 |
|---|---|
| 自定义函数注册 | `CustomFunctions.associate` |
| Ribbon 命令注册 | `Office.actions.associate` |
| 初始化 | `Office.onReady`、`setStartupBehavior`、`showAsTaskpane` |
| 重算事件 | `onCalculated` |
| 批量入队 | `addRequest`、`reqMap`、`setInterval` |
| RTD 推送 | `setResult`、`invocation`、`refreshRate`、`onCanceled` |
| Excel 访问 | `Excel.run`、`getRangeByIndexes`、`getItemOrNullObject`、`setDirty`、`notes` |
| 网络 | `fetch(`、`gzip`、`Headers` |
| 平台分支 | `Office.context.platform`、`Office.PlatformType` |
| 公式兼容 | `thsiFinD(`、`HX_HisQuote(`（旧 COM 公式名 ↔ 新公式名 Map） |

## 网络层要点（模块 4784 实测）

```js
class l { concurrency; queue; activeCount }   // 并发队列
const p = new l(6);                            // 6 路并发限制
const d = (e) => { ... gzip header + CRC32 + 原始长度 ... };  // gzip 请求体编码
f = (e, t) => p.addTask(() => fetch(e, {method: "POST", headers, body: t}))
// headers: User-Agent / Content-Type: application/x-www-form-urlencoded / ifindlang
// 401/403 → 登出处理
```

## 请求队列单例（模块 80832 实测）

```js
class l {   // N 单例
  aryReqItems / aryResultItems / mapParamsKey / aryCallerAddrBatch
  static getInstance()
  addRequest(e)     // callAddr + paramKey 去重；新请求 push + clearRangeContents
  doRequest()       // calFlag; <5 串行，≥5 分 5 路 Promise.allSettled
  doExport(ctx)     // 遍历结果 → setFormulaRangeFormat → exportToExcel(ctx)
  static isCalculating() / resetCalculate()
}
```

## git 记录建议（每阶段一个 commit）

```
feat(analysis): 下载 Office 加载项 manifest 与全部静态资源
feat(analysis): 格式化全部 js 文件（prettier）
feat(analysis): 定位 CustomFunctions 注册与初始化时序
feat(analysis): 追踪 hxiFinD 批量机制与 Excel.run 流程
docs: 输出 Office vs WPS 对比分析报告
```
