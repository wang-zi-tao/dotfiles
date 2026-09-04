"""COM session manager for mcp-wpsapi.

All COM objects are created and used on a single dedicated worker thread that
owns an STA (CoInitialize). Every MCP tool call submits a closure to that thread
and waits for the result, which avoids RPC_E_WRONG_THREAD marshaling errors when
FastMCP dispatches tools across its thread pool.
"""

from __future__ import annotations

import concurrent.futures
import queue
import threading
import traceback
from typing import Any, Callable, Dict, List, Optional

import pythoncom
import win32com.client

# app alias -> list of ProgIDs to try in order
APP_PROGIDS: Dict[str, List[str]] = {
    "wps": ["KWPS.Application"],  # WPS Writer 文字
    "et": ["KET.Application", "ET.Application"],  # WPS Spreadsheets 表格
    "wpp": ["kwpp.Application", "WPP.Application"],  # WPS Presentation 演示
    "word": ["Word.Application"],
    "excel": ["Excel.Application"],
    "ppt": ["PowerPoint.Application"],
}

APP_DISPLAY: Dict[str, str] = {
    "wps": "WPS Writer (文字)",
    "et": "WPS Spreadsheets (表格)",
    "wpp": "WPS Presentation (演示)",
    "word": "MS Word",
    "excel": "MS Excel",
    "ppt": "MS PowerPoint",
}


def _dispatch(progid: str, fresh: bool) -> Any:
    """Create a COM app object, preferring early-bound typelib wrappers.

    gencache.EnsureDispatch generates a wrapper from the registered type
    library, which exposes full method sets (e.g. KET Workbook.SaveAs) that
    the late-bound dynamic Dispatch cannot resolve. Falls back to dynamic
    dispatch when the type library is unavailable.
    """
    try:
        return win32com.client.gencache.EnsureDispatch(progid)
    except Exception:
        return win32com.client.DispatchEx(progid) if fresh else win32com.client.Dispatch(progid)


class ComSession:
    """Owns one COM STA worker thread and the application objects on it."""

    def __init__(self) -> None:
        self._apps: Dict[str, Any] = {}  # alias -> COM app object
        self._queue: "queue.Queue[Callable[[], Any]]" = queue.Queue()
        self._pool = concurrent.futures.ThreadPoolExecutor(max_workers=1)
        self._futures: Dict[int, concurrent.futures.Future] = {}
        self._seq = 0
        self._thread = threading.Thread(target=self._run_loop, name="com-sta", daemon=True)
        self._thread.start()

    def _run_loop(self) -> None:
        """Worker loop: initialise COM and execute submitted closures serially."""
        try:
            pythoncom.CoInitialize()
        except Exception:
            pass
        while True:
            fn = self._queue.get()
            try:
                fn()
            except Exception:
                # closure is responsible for surfacing its own errors; never crash the loop
                traceback.print_exc()

    def submit(self, fn: Callable[[], Any], timeout: float) -> Any:
        """Run fn on the COM STA thread and block up to `timeout` seconds."""
        self._seq += 1
        seq = self._seq
        fut: "concurrent.futures.Future" = concurrent.futures.Future()
        self._futures[seq] = fut

        def runner() -> None:
            try:
                fut.set_result(fn())
            except BaseException as exc:  # noqa: BLE001 - surface all errors to caller
                fut.set_exception(exc)
            finally:
                self._futures.pop(seq, None)

        self._queue.put(runner)
        try:
            return fut.result(timeout=timeout)
        except concurrent.futures.TimeoutError:
            raise TimeoutError(f"COM 调用超时 ({timeout}s)。WPS/Office 可能弹出了模态对话框。")

    # ---- app lifecycle ----

    def start_app(self, alias: str, visible: bool = True, attach: bool = True) -> dict:
        """Launch (or attach to) an application and return its status summary."""
        alias = alias.strip().lower()
        if alias not in APP_PROGIDS:
            raise ValueError(f"未知应用别名 \"{alias}\"。可用: {list(APP_PROGIDS)}")
        if alias in self._apps:
            return self.app_status(alias)

        def _start() -> Any:
            app = None
            last_err: Optional[Exception] = None
            for progid in APP_PROGIDS[alias]:
                try:
                    if attach:
                        # prefer attaching to an already running instance
                        try:
                            app = win32com.client.GetActiveObject(progid)
                        except Exception:
                            app = _dispatch(progid, fresh=False)
                    else:
                        app = _dispatch(progid, fresh=True)  # always a fresh instance
                    break
                except Exception as exc:
                    last_err = exc
                    app = None
            if app is None:
                raise RuntimeError(f"无法启动 {alias}: {last_err}")
            try:
                if visible:
                    app.Visible = True
            except Exception:
                pass
            self._apps[alias] = app
            return app

        app = self.submit(_start, timeout=20)
        return self.app_status(alias)

    def get_app(self, alias: str) -> Any:
        """Return the live COM object for an alias (must be called on the STA thread)."""
        alias = alias.strip().lower()
        if alias not in self._apps:
            raise RuntimeError(f"应用 \"{alias}\" 未启动。先调用 start_app。已启动: {list(self._apps)}")
        return self._apps[alias]

    def quit_app(self, alias: str, save: bool = True) -> dict:
        """Quit an application. Returns its pre-quit status."""
        alias = alias.strip().lower()
        if alias not in self._apps:
            return {"ok": True, "app": alias, "note": "not running"}

        def _quit() -> None:
            app = self._apps.pop(alias, None)
            if app is None:
                return
            try:
                # Suppress modal save prompts so Quit() never blocks on a
                # "Do you want to save?" dialog (WPS/Office shows one whenever
                # a workbook with unsaved changes is quit, which freezes the
                # COM STA thread until the dialog is dismissed).
                try:
                    app.DisplayAlerts = False
                except Exception:
                    pass
                app.Quit()
            except Exception:
                # ignore errors during teardown
                pass

        self.submit(_quit, timeout=15)
        return {"ok": True, "app": alias, "note": "quit requested"}

    def list_apps(self) -> List[str]:
        return sorted(self._apps.keys())

    def app_status(self, alias: str) -> dict:
        alias = alias.strip().lower()
        if alias not in self._apps:
            return {"ok": False, "app": alias, "running": False, "progid": None}

        def _status() -> dict:
            app = self._apps[alias]
            info: dict = {"ok": True, "app": alias, "running": True, "display": APP_DISPLAY.get(alias, alias)}
            for prop in ("Name", "Version", "Visible", "Path"):
                try:
                    v = getattr(app, prop)
                    if not callable(v):
                        info[prop.lower()] = v
                except Exception:
                    pass
            return info

        return self.submit(_status, timeout=10)

    def bind_namespace(self, ns: dict) -> None:
        """Bind live COM app objects into an eval namespace (call on STA thread)."""
        for alias, app in self._apps.items():
            ns[alias] = app
        ns["apps"] = dict(self._apps)


def resolve_progids() -> Dict[str, str]:
    """Return alias -> progid actually registered on this machine (best-effort)."""
    import winreg

    found: Dict[str, str] = {}
    for alias, progids in APP_PROGIDS.items():
        for progid in progids:
            try:
                with winreg.OpenKey(winreg.HKEY_CLASSES_ROOT, progid):
                    found[alias] = progid
                    break
            except OSError:
                continue
    return found

