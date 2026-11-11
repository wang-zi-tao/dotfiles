# Manual end-to-end checks

Scripts here need a built package (`npm run build`) and real language servers;
they are not part of `npm test`.

## Diagnostics always terminate

```bash
node test/manual/diagnostics-e2e.mjs
```

Requires `pyright-langserver` on `PATH` (override with `DSH_LSP_PYRIGHT`).

| Scenario | Setup | Expected |
| --- | --- | --- |
| A | real pyright, `sample.py` | findings returned, no note |
| B | `hang-lsp-stub.cjs diag-hang`: initialize is answered, `textDocument/diagnostic` never is | returns after ~`timeoutMs` with a `note` |
| C | `hang-lsp-stub.cjs init-hang`: initialize is never answered | rejects with `LspDeadlineError` after ~`timeoutMs` |

Every scenario has its own watchdog, so a regression fails the script rather
than hanging it. Against the pre-fix code both B and C hang.

## Teardown / cancellation crash

```bash
node test/manual/teardown-crash.mjs
```

Spawns the stub (initialize answered, pull diagnostics never answered), opens a
pull request, tears the client down, then lets the request's deadline fire. Any
`uncaughtException` fails the script.

The crash it pins: cancelling a request on a closed connection makes
`vscode-jsonrpc` throw `ConnectionError: Connection is closed` from the abort
listener, and Node reports a throwing listener as an uncaught exception — a
plugin hot reload killed the whole host process this way. Point `DSH_LSP_LIB` at
a build without the guard in `LspClient#token` to watch it fail.
