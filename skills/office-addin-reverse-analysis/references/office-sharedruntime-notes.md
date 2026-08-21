# Office SharedRuntime 架构要点

来源：同花顺 iFinD Office 加载项（`xlsslug-excel`）逆向分析。

## 清单结构（TaskPaneApp + SharedRuntime 1.1）

```xml
<OfficeApp xsi:type="TaskPaneApp">
  <Id>...</Id><Version>1.0.0.8</Version>
  <Requirements>
    <Sets DefaultMinVersion="1.1">
      <Set Name="SharedRuntime" MinVersion="1.1" />
    </Sets>
  </Requirements>
  <DefaultSettings>
    <SourceLocation DefaultValue="https://.../taskpane.html" />
  </DefaultSettings>
  <VersionOverrides>
    <Hosts><Host xsi:type="Workbook">
      <Runtimes><Runtime resid="Taskpane.Url" lifetime="long" /></Runtimes>
      <AllFormFactors>
        <ExtensionPoint xsi:type="CustomFunctions">
          <Script><SourceLocation resid="Functions.Script.Url" /></Script>
          <Page><SourceLocation resid="Taskpane.Url" /></Page>
          <Metadata><SourceLocation resid="Functions.Metadata.Url" /></Metadata>
        </ExtensionPoint>
      </AllFormFactors>
      <DesktopFormFactor>
        <FunctionFile resid="Taskpane.Url" />
        <ExtensionPoint xsi:type="PrimaryCommandSurface">... Ribbon 按钮 ...</ExtensionPoint>
      </DesktopFormFactor>
    </Host></Hosts>
    <Resources>
      <bt:Urls>
        <bt:Url id="Functions.Script.Url" DefaultValue="https://.../js/functions.js" />
        <bt:Url id="Functions.Metadata.Url" DefaultValue="https://.../functions.json" />
        <bt:Url id="Taskpane.Url" DefaultValue="https://.../taskpane.html" />
      </bt:Urls>
    </Resources>
  </VersionOverrides>
</OfficeApp>
```

## 核心概念

- **SharedRuntime**：自定义函数与任务窗格共享同一个 JS 运行时。functions.js 与 taskpane.html 加载到同一 runtime，可互相访问全局状态。
- **lifetime="long"**：运行时长期驻留（不随窗格关闭销毁）。
- **CustomFunctions.associate**：注册自定义函数（新版标准；旧版为 `CustomFunctions.associate`，Ribbon 命令用 `Office.actions.associate`）。
- **函数 ID 全大写**约定（HXIFIND），与 functions.json `id` 字段一致；WPS 版用驼峰小写。

## 初始化时序（taskpane.js 实测）

```js
_.add(() => Office.onReady(() => {
    // 1) 懒加载自定义函数实现 chunk（n.e(170) → 170.<hash>.js）
    // 2) Office.addin.setStartupBehavior(Office.StartupBehavior.load)  // 启动时加载
    // 3) Office.addin.showAsTaskpane()
    // 4) Excel.run: workbook.worksheets.onCalculated.add(w)  // 重算事件
    // 5) Office.addin.hide()   // 隐藏任务窗格
}))
```

**onCalculated 驱动**：工作表重算完成后自动触发数据请求（doRequest）+ 回写（doExport），这是 Office 版数据流的关键驱动点。

## 双份打包现象

functions.js（manifest Script URL）与懒加载 chunk（如 `js/170.<hash>.js`）可能**包含同一份自定义函数实现**（webpack 打包重复）。两者都含 `CustomFunctions.associate` 注册，分析时需对比确认。

## 常见注册表（14 个函数示例）

```js
CustomFunctions.associate("LOG", ye)            // 控制台调试
CustomFunctions.associate("HXRTD", pe)          // stream 实时行情
CustomFunctions.associate("HXDR", ge)           // 专题报表
CustomFunctions.associate("HXAD", be) / HXAD2 / HXAD3  // 插入日期
CustomFunctions.associate("HXIFIND", Oe)        // 自助报表（批量）
CustomFunctions.associate("HXDS", De) / HXDS2   // 日期序列
CustomFunctions.associate("HXHQ", xe)           // 历史行情
CustomFunctions.associate("HXMEDB", we) / HX_MEDB
CustomFunctions.associate("HXGP", Te)           // 高频序列
CustomFunctions.associate("HXTDAYSOFFSET", Ce)  // 日期偏移
```

## functions.json 元数据差异（vs WPS）

| 字段 | WPS | Office |
|---|---|---|
| id | `hxiFinD`（驼峰） | `HXIFIND`（大写） |
| 类型 | `"string?"` / `"any?"` | `"any"` + `optional: true` |
| 矩阵参数 | 无显式标注 | `"dimensionality": "matrix"` |
| options | `stream` / `requiresAddress`（WPS 专有） | `stream`（Office 也支持） |
| result | 部分带 type | `{}` 或带 type |

## 环境探测

```js
Office.context.platform === Office.PlatformType.OfficeOnline  // 在线 Office
Office.context.requirements.isSetSupported("ExcelApi", "1.18")  // API 版本门控
Excel.CalculationMode.manual / automatic  // 计算模式
```
