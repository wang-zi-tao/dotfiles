import sys, asyncio
sys.path.insert(0, r"C:\dotfiles-copy\packages\mcp-wpsapi")

from mcp_wpsapi.server import mcp

async def main():
    # 1) start ET (WPS Spreadsheets)
    res = await mcp.call_tool("start_app", {"app": "et"})
    print("START_ET:", res.content[0].text)

    # 2) batch eval_python: add workbook, write cells, sum (valid Python)
    code = """
et.Workbooks.Add()
for i in range(1, 6):
    et.Cells(1, i).Value2 = i * 10
et.Cells(2, 1).Value2 = 100
result = sum(et.Cells(1, i).Value2 for i in range(1, 6))
print("row sum =", result)
"""
    res = await mcp.call_tool("eval_python", {"code": code})
    print("EVAL_1:", res.content[0].text)

    # 3) persistent state across calls
    res = await mcp.call_tool("eval_python", {"code": "result * 2"})
    print("EVAL_2:", res.content[0].text)

    # 4) session_vars shows user vars + connected apps
    res = await mcp.call_tool("session_vars", {})
    print("SESSION_VARS:", res.content[0].text)

    # 5) get a COM object summary (doc count)
    res = await mcp.call_tool("eval_python", {"code": "et.Workbooks.Count"})
    print("EVAL_COUNT:", res.content[0].text)

    # 6) close workbook + quit
    res = await mcp.call_tool("eval_python", {"code": "et.ActiveWorkbook.Close(False)"})
    print("EVAL_CLOSE:", res.content[0].text)
    res = await mcp.call_tool("stop_app", {"app": "et", "save": False})
    print("STOP_ET:", res.content[0].text)

asyncio.run(main())
