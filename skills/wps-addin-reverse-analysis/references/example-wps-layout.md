# 典型 WPS 加载项文件布局示例

以下为 `xlsslug-wps`（同花顺 iFinD）的真实布局，可作为分析 WPS 加载项时的对照模板：

```
index.html  (URL 入口)
├── manifest.xml          JsPlugin 清单
│   └── <Functions><Uri>/functions.json</Uri>
├── functions.json        函数元数据
│   ├── id: "hxRTD", options: {stream: true}
│   ├── id: "hxDR",  options: {requiresAddress: true}
│   └── type: "string?"（WPS 可空类型后缀）
├── api.js                jsapi 传输层（cefQuery/external COM/app.sendMessage）
├── ribbon.xml            Ribbon UI
├── js/
│   ├── index-<hash>.js       主 bundle
│   ├── functions-<hash>.js   自定义函数实现
│   └── xlsEDBItem-<hash>.js  经济数据库模块
├── css/
└── 第三方: chameleon*.js / ta.min.js / time.*.js / package-*.js
```

## git 记录建议（每阶段一个 commit）

```
feat(analysis): 下载 WPS 加载项页面与静态资源
feat(analysis): 格式化全部 js 文件（prettier）
feat(analysis): 定位自定义函数注册与入口模式
feat(analysis): 追踪 hxiFinD 批量机制与 jsapi 流程
docs: 输出自定义函数分析报告
```

## 关键 grep 模式

| 目标 | 模式 |
|---|---|
| 函数注册 | `AddCustomFunction` |
| 鉴权/环境检测 | `function O(`、`function T(` |
| 批量入队 | `addRequest`、`reqMap`、`setInterval` |
| RTD 推送 | `setResult`、`invocation`、`refreshRate` |
| COM 表格访问 | `ActiveWorkbook`、`Worksheets.Item` |
| 网络 | `fetch(`、`XMLHttpRequest`、`GetTHSCodes`、`RealTime` |
| 事件 | `apievent`、`AddEventListener`、`onCanceled` |
