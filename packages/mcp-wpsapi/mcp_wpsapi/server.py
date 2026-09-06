"""FastMCP server exposing WPS/Office COM automation tools."""

from __future__ import annotations

from typing import Any, Dict, List, Optional

from fastmcp import FastMCP

from .com_session import ComSession, APP_DISPLAY, resolve_progids
from .eval_engine import EvalEngine
from . import __version__


mcp = FastMCP(
    "mcp-wpsapi",
    version=__version__,
    instructions=(
        "Drive WPS Office and Microsoft Office through the Python COM API. "
        "Use start_app to launch/attach an application, then eval_python to run "
        "arbitrary Python (or batched scripts) against the live objects."
    ),
)

_session: ComSession = ComSession()
_engine: EvalEngine = EvalEngine(_session)


@mcp.tool()
def start_app(
    app: str = "wps",
    visible: bool = True,
    attach: bool = True,
) -> Dict[str, Any]:
    """Launch or attach to a WPS/Office application.

    Args:
        app: one of wps / et / wpp / word / excel / ppt.
        visible: show the application window (default True).
        attach: prefer attaching to an already-running instance (default True).

    Returns the application status summary.
    """
    return _session.start_app(app, visible=visible, attach=attach)


@mcp.tool()
def stop_app(app: str, save: bool = True) -> Dict[str, Any]:
    """Quit a previously started WPS/Office application. Pass save=False to discard changes."""
    return _session.quit_app(app, save=save)


@mcp.tool()
def list_apps() -> List[Dict[str, Any]]:
    """List all connected application instances with their status."""
    return [_session.app_status(a) for a in _session.list_apps()]


@mcp.tool()
def app_status(app: str) -> Dict[str, Any]:
    """Query the status of one application instance."""
    return _session.app_status(app)


@mcp.tool()
def available_apps() -> Dict[str, str]:
    """Return which WPS/Office COM ProgIDs are registered on this machine."""
    found = resolve_progids()
    return {alias: {"progid": pid, "display": APP_DISPLAY.get(alias, alias)} for alias, pid in found.items()}


@mcp.tool()
def eval_python(code: str, timeout: float = 15.0) -> Dict[str, Any]:
    """Execute Python code inside the persistent COM session.

    This is the batch-workhorse tool. The code runs on the COM worker thread
    against a long-lived namespace that keeps the live application objects:
    `app` (last-used), plus wps/et/wpp/word/excel/ppt and `apps` (alias -> obj).
    State persists between calls: an open document, a selection, a loop over
    workbooks all survive across eval_python invocations.

    A single bare expression is evaluated and its value is returned. A block of
    statements is exec'd with stdout captured; assign to `_` or `result` to get
    a value back. Example (ET/WPS Spreadsheets):
        et.Workbooks.Add()
        _ = [et.Cells(1, i).Value2 = i for i in range(1, 6)]

    Args:
        code: the Python source to run.
        timeout: seconds to wait for the call (default 15).
    """
    return _engine.eval_python(code, timeout=timeout)


@mcp.tool()
def run_script(path: str, timeout: float = 30.0) -> Dict[str, Any]:
    """Execute a .py batch script file inside the persistent session.

    The script runs with the same shared namespace as eval_python, so it can
    loop over many COM calls to operate on WPS/Office in bulk.
    """
    return _engine.run_script(path, timeout=timeout)


@mcp.tool()
def session_vars() -> Dict[str, Any]:
    """List non-Callable variable names currently defined in the eval namespace."""
    def _vars() -> Dict[str, Any]:
        names = [
            n
            for n in _engine._ns
            if not n.startswith("__")
            and not callable(_engine._ns.get(n))
        ]
        return {"names": names, "apps": list(_session.list_apps())}

    return _session.submit(_vars, timeout=10)


def main() -> None:
    """Run the MCP server over stdio (default transport)."""
    mcp.run()


if __name__ == "__main__":
    main()

