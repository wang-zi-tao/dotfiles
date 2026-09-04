"""JSON-safe serialization of values returned by eval / COM calls."""

from __future__ import annotations

import base64
import datetime as _dt
from typing import Any, Dict, List


def _is_com(obj: Any) -> bool:
    """Heuristic: is this a COM object? pywin32 dispatch objects expose _oleobj_."""
    return hasattr(obj, "_oleobj_") or hasattr(obj, "_oleobj__")


def _com_summary(obj: Any, depth: int) -> Dict[str, Any]:
    """Build a compact, JSON-safe summary of a COM object."""
    info: Dict[str, Any] = {
        "__com__": True,
        "type": type(obj).__name__,
        "repr": str(obj)[:300],
    }
    # probe a handful of common, cheap properties
    for prop in ("Name", "Count", "Visible", "Path", "FullName", "Type", "Version"):
        try:
            v = getattr(obj, prop)
            if not callable(v):
                info[prop] = to_jsonable(v, depth + 1)
        except Exception:
            pass
    return info


def to_jsonable(value: Any, depth: int = 0, max_depth: int = 8) -> Any:
    """Convert an arbitrary Python/COM value into a JSON-serializable structure."""
    if depth > max_depth:
        return {"__truncated__": True, "repr": str(value)[:100]}
    if value is None or isinstance(value, (bool, int, float, str)):
        return value
    if isinstance(value, (list, tuple, set)):
        return [to_jsonable(v, depth + 1, max_depth) for v in list(value)[:200]]
    if isinstance(value, dict):
        return {str(k): to_jsonable(v, depth + 1, max_depth) for k, v in list(value.items())[:200]}
    if isinstance(value, bytes):
        return {"__bytes__": True, "b64": base64.b64encode(value).decode("ascii")[:2000]}
    if isinstance(value, (_dt.datetime, _dt.date)):
        return value.isoformat()
    if _is_com(value):
        return _com_summary(value, depth)
    # numpy / array-like fallbacks
    try:
        import numpy as np

        if isinstance(value, np.ndarray):
            return value.tolist()[:200]
    except ImportError:
        pass
    # generic object: use its repr / str
    try:
        return {"__obj__": True, "type": type(value).__name__, "str": str(value)[:300]}
    except Exception:
        return {"__obj__": True, "type": type(value).__name__}

