"""Allow running the server as `python -m mcp_wpsapi` or `mcp_wpsapi`."""

import sys


def _main() -> None:
    # Works whether this file is imported as a package module (`python -m
    # mcp_wpsapi`) or executed directly as a script (uv resolves a bare
    # `mcp_wpsapi` the latter way, leaving no parent package).
    if __package__:
        from .server import main
    else:
        sys.path.insert(0, __file__.rsplit("mcp_wpsapi", 1)[0])
        from mcp_wpsapi.server import main
    main()


if __name__ == "__main__":
    _main()
