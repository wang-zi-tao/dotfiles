# WPS 加载项分析要点（仓库既有分析摘要）

来源：`D:\repo\IFind\docs\` 既有分析 + `AGENTS.md`。

## 典型文件布局（以 iFinD xlsslug-wps 为例）

```
index.html                页面入口（URL 指向此文件）
manifest.xml              226B，JsPlugin 清单，<Functions><Uri>/functions.json</Uri>
ribbon.xml                3.7KB，Ribbon UI 定义
functions.json            19KB，函数元数据（stream/requiresAddress 为 WPS 专有选项）
api.js                    6.8KB，jsapi 传输层（唯一未混淆核心文件）
index-7vsERyhB.js         1.8MB，主 bundle（Vue app + 批量机制）
functions-C9j6YkxB.js     80KB，自定义函数实现（wps.AddCustomFunction 注册 13 个）
xlsEDBItem-CAfUX67g.js    63KB，经济数据库模块
chameleon*.js             136KB，第三方主题库
ta.min.js                 8KB，埋点
```

## 自定义函数注册（13 个）

| 函数 | options | 说明 |
|---|---|---|
| hxRTD | stream: true | 实时行情，轮询 + setResult |
| hxDR | requiresAddress | 专题报表 |
| hxAD / hxAD2 / hxAD3 | requiresAddress | 插入日期 |
| hxTDaysOffset / hxiFinD / hxDS / hxDS2 / hxGP / hxMEDB / hx_MEDB / hxHQ | — | 批量请求类 |

## 函数入口模式（全部 13 个函数一致）

```js
async function XxxFn(..., invocation) {
    await O();        // ① 登录/鉴权检查（main bundle）
    if (!T()) return null;  // ② WPS 表格环境检测
    // ③ 创建请求对象 → S.getInstance().addRequest(req)
    return "同花顺iFinD";  // 占位返回值
}
```

## 批量请求机制（hxiFinD，main bundle `mt` 类）

```
ume() 入队(去重 reqMap) → lve() 100ms 定时器稳定检测
  → sve() 分号拼接 paramExcel → 1 个 POST /AutoReform
  → uve() 结果回写：手动计算模式 + Range.Dirty()/Calculate()
```

触发发送条件：队列空 / ≥3000 分片 / 稳定 500ms（timerCounter==5）/ >1000 条。

## RTD 流式（hxRTD）

- 单例 `w` 管理 RTD 请求队列
- `invocation.setResult(data)` 推送结果
- `localStorage.refreshRate` 控制刷新率（默认 3s）
- 多单元格共享一个 interval；`onCanceled` 清理

## jsapi 传输层（api.js）

1. `window.cefQuery`（CEF 环境）→ `escape()` 编码参数 + `cefQuery({request, persistent:false, onSuccess, onFailure})`
2. `window.external[method](args...)`（COM 同步，switch 0-5 个参数）
3. `window.app.sendMessage(method, params)` + `setMessageCallback`（异步）

API 方法列表在 api.js 第 136 行 `h` 变量（~120+ 方法）。

## 表格访问（COM jsapi，同步）

```js
window.Application.ActiveWorkbook.Worksheets.Item(name)
Range.Value2 / NumberFormat / NumberFormatLocal / Clear() / Formula
Range.Dirty() / Calculate()
Application.Calculation = xlCalculationManual
```

## 已知性能问题（复用为检查清单）

1. hxRTD 多单元格共享 interval，刷新率取最后一个单元格的 localStorage
2. 公式改写（X()/Ft() 附加 rows=/cols=）→ 触发重算 → 递归风险
3. setDataFormat() 循环逐列设置 NumberFormat
4. j()/v() 元数据查询每次 export 无缓存
5. 每次 export 重新获取 ActiveWorkbook
