"""Batch evaluation engine: run Python code inside a persistent COM session.

The session keeps a long-lived namespace (module globals) that holds the live
COM application objects (wps/et/wpp/word/excel/ppt and `app`). eval_python()
executes user code in that namespace on the COM STA thread, so state — an open
document, a selected range, a loop over workbooks — persists between calls.
This enables driving WPS/Office in a scripted, batch manner.
"""

from __future__ import annotations

import ast
import contextlib
import io
import traceback
from typing import Any, Dict, Optional

from .com_session import ComSession
from .serialize import to_jsonable


class EvalEngine:
    """Persistent interpreter bound to one ComSession."""

    def __init__(self, session: ComSession) -> None:
        self._session = session
        self._ns: Dict[str, Any] = {}
        self._reset_ns()

    def _reset_ns(self) -> None:
        """(Re)seed the shared namespace with helpers and live app bindings."""
        ns: Dict[str, Any] = {
            "app": None,  # last-used application
            "apps": {},  # alias -> app object, refreshed before each eval
            "wps": None,
            "et": None,
            "wpp": None,
            "word": None,
            "excel": None,
            "ppt": None,
            "print": print,
            "__name__": "__wpsapi__",
        }
        self._ns = ns

    def refresh_bindings(self) -> None:
        """Re-bind live COM apps into the namespace (must run on STA thread)."""
        self._session.bind_namespace(self._ns)
        # keep `app` pointing at the most recent non-None app
        for alias in ("wps", "et", "wpp", "word", "excel", "ppt"):
            if self._ns.get(alias) is not None:
                self._ns["app"] = self._ns[alias]
                break

    def eval_python(self, code: str, timeout: float = 60.0) -> Dict[str, Any]:
        """Execute `code` in the shared namespace on the COM thread.

        If the code is a single bare expression, its value is evaluated and
        returned. Otherwise the block is executed as statements: stdout is
        captured and the value of `_` (or of an explicit assignment to `result`)
        is returned as the result.
        """

        def _run() -> Dict[str, Any]:
            self.refresh_bindings()
            out = io.StringIO()
            result: Any = None
            error: Optional[str] = None
            try:
                tree = ast.parse(code, mode="exec")
                single_expr = (
                    len(tree.body) == 1
                    and isinstance(tree.body[0], ast.Expr)
                    and isinstance(tree.body[0].value, (ast.Constant, ast.Name, ast.Attribute,
                                                        ast.Call, ast.BinOp, ast.Subscript,
                                                        ast.List, ast.Tuple, ast.Dict,
                                                        ast.UnaryOp, ast.Compare, ast.BoolOp,
                                                        ast.IfExp, ast.Lambda, ast.JoinedStr))
                )
                with contextlib.redirect_stdout(out), contextlib.redirect_stderr(out):
                    if single_expr:
                        expr = tree.body[0].value
                        result = eval(compile(ast.Expression(expr), "<wpsapi-eval>", "eval"), self._ns)
                    else:
                        self._ns.pop("_", None)
                        self._ns.pop("result", None)
                        exec(compile(tree, "<wpsapi-exec>", "exec"), self._ns)
                        result = self._ns.get("_", self._ns.get("result"))
            except Exception:
                error = traceback.format_exc()
            return {
                "ok": error is None,
                "result": to_jsonable(result) if error is None else None,
                "stdout": out.getvalue(),
                "error": error,
            }

        return self._session.submit(_run, timeout=timeout)

    def run_script(self, path: str, timeout: float = 120.0) -> Dict[str, Any]:
        """Execute a .py file inside the shared namespace (batch scripts)."""
        with open(path, "r", encoding="utf-8") as f:
            code = f.read()
        return self.eval_python(code, timeout=timeout)

