"""Redirect pywin32's generated COM cache before ``win32com.client`` loads.

pywin32 writes early-binding wrappers (and ``dicts.dat``) into
``%TEMP%\\gen_py\\<major>.<minor>`` unless a path is configured, and importing
``win32com.client`` rebuilds that cache as an import side effect.  The shared
temp directory is not always writable -- the DSH Windows sandbox, for example,
denies creating new entries directly under ``%TEMP%`` -- and then the import
itself dies with::

    PermissionError: [WinError 5] ... 'C:\\Users\\<user>\\AppData\\Local\\Temp\\gen_py'

``configure_gencache()`` must run before the first ``import win32com.client``.
The cache then lands in the first writable candidate:

1. ``$MCP_WPSAPI_GEN_PATH`` -- explicit override, used verbatim;
2. ``<repo>/packages/mcp-wpsapi/.gen_py/<major>.<minor>`` -- checkout-local, so
   it stays writable inside the workspace sandbox;
3. ``%LOCALAPPDATA%\\mcp-wpsapi\\gen_py\\<major>.<minor>`` -- per-user cache;
4. pywin32's own default, ``%TEMP%\\gen_py\\<major>.<minor>``.
"""

from __future__ import annotations

import os
import sys
import tempfile
import uuid
from pathlib import Path

import win32com

_PY_VERSION = f"{sys.version_info.major}.{sys.version_info.minor}"

_configured: Path | None = None


def _is_writable(directory: Path) -> bool:
    """Create *directory* and prove it can hold a file; False when denied."""
    try:
        directory.mkdir(parents=True, exist_ok=True)
        probe = directory / f".write-probe-{uuid.uuid4().hex}"
        try:
            with open(probe, "xb"):
                pass
        finally:
            probe.unlink(missing_ok=True)
        return True
    except OSError:
        return False


def _candidates() -> list[Path]:
    """Return the cache locations to try, in preference order."""
    candidates: list[Path] = []

    override = os.environ.get("MCP_WPSAPI_GEN_PATH")
    if override:
        candidates.append(Path(override).expanduser())

    candidates.append(Path(__file__).resolve().parent.parent / ".gen_py" / _PY_VERSION)

    local_appdata = os.environ.get("LOCALAPPDATA")
    if local_appdata:
        candidates.append(Path(local_appdata) / "mcp-wpsapi" / "gen_py" / _PY_VERSION)

    candidates.append(Path(tempfile.gettempdir()) / "gen_py" / _PY_VERSION)
    return candidates


def configure_gencache() -> Path:
    """Point ``win32com.__gen_path__`` at a writable directory (idempotent)."""
    global _configured
    if _configured is not None:
        return _configured

    chosen = next((path for path in _candidates() if _is_writable(path)), None)
    if chosen is None:
        # Nothing writable: keep pywin32's default so the error still names it.
        default = win32com.__gen_path__
        chosen = Path(default) if default else Path(tempfile.gettempdir()) / "gen_py" / _PY_VERSION

    win32com.__gen_path__ = str(chosen)
    # ``win32com`` pre-registers a synthetic ``win32com.gen_py`` package whose
    # ``__path__`` was fixed at import time; generated modules are imported
    # through it, so it has to follow the retargeted directory.
    gen_py = sys.modules.get("win32com.gen_py")
    if gen_py is not None:
        gen_py.__path__ = [str(chosen)]

    _configured = chosen
    return chosen
