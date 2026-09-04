# mcp-wpsapi

MCP（Model Context Protocol）服务器：通过 **Python COM API** 远程启动并操作 WPS Office（WPS 文字 / WPS 表格 / WPS 演示）与 Microsoft Office（Word / Excel / PowerPoint）。

核心能力是 **`eval_python` 批量执行**：一段 Python 代码可以在持久化的 COM 会话里对文档/表格/演示做任意批量操作（开文档、写单元格、循环处理、遍历多文件），状态跨调用保留。

## 依赖

已确认本机满足：Python 3.14 + `mcp` + `fastmcp` + `pywin32`。若缺，用 pip 安装：

```powershell
pip install -r requirements.txt
```

## 安装

```powershell
cd C:\dotfiles-copy\packages\mcp-wpsapi
pip install -e .        # 提供 mcp-wpsapi 命令
```

## 在 DSH 中注册（stdio）

在 `~/.dsh/profiles/web/cordis.patch.yml`（及 `tui` 同构文件）的 `insert:` 块里追加：

```yaml
- id: mcp-wpsapi
  name: "@deepseek-ai/dsh-mcp-client"
  config:
    transport: stdio
    serverName: wpsapi
    command: python
    args:
      - -m
      - mcp_wpsapi
    env: {}
```

## 可用工具

| 工具 | 说明 |
| --- | --- |
| `available_apps()` | 列出本机已注册的 WPS/Office COM ProgID |
| `start_app(app, visible, attach)` | 启动或附加到应用：`wps` / `et` / `wpp` / `word` / `excel` / `ppt` |
| `app_status(app)` | 查询某应用状态（版本/可见性等） |
| `list_apps()` | 列出已连接的实例 |
| `stop_app(app, save)` | 退出应用（`save=False` 放弃修改） |
| `eval_python(code, timeout)` | **批量执行**：在持久命名空间中跑任意 Python |
| `run_script(path, timeout)` | 执行 `.py` 批处理脚本（同样共享命名空间） |
| `session_vars()` | 查看命名空间中已定义的变量与连接的应用 |

## eval_python 用法示例

启动 WPS 表格并批量写单元格：

```python
et = start_app("et")   # 或直接 eval: start_app(app="et")
# 之后批量执行
eval_python(code="""
import itertools
et.Workbooks.Add()
_ = [et.Cells(1, i).Value2 = i * 10 for i in range(1, 6)]
et.Range(et.Cells(1, 1), et.Cells(1, 5)).Font.Bold = True
et.ActiveWorkbook.SaveAs(r"C:\temp\demo.xlsx")
et.ActiveWorkbook.Close(False)
""")
```

状态持久化示例（两次调用共享命名空间）：

```python
eval_python(code="x = 42")          # 第一次
eval_python(code="x * 2")           # 第二次 -> 84
```

## 说明

- 所有 COM 对象都在一个专用 STA 工作线程上创建与访问，避免 `RPC_E_WRONG_THREAD` 封送错误。
- 工具默认 `attach=True`：若 WPS/Office 已在运行则附加，否则新建实例。
- COM 调用可能在模态对话框处挂起，`eval_python` 支持 `timeout` 参数防卡死。
- 安全提示：`eval_python` 执行任意代码，仅应在受信任的本地环境中使用。
