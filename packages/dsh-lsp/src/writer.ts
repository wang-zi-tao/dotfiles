/**
 * A guarded `MessageWriter` for the JSON-RPC connection to a language server.
 *
 * `vscode-jsonrpc` writes a request inside `new Promise(async (resolve, reject)
 * => { ... })` and, when that write fails, rejects the request *and* rethrows
 * from the async executor (`connection.ts`, "Sending request failed."). The
 * executor's own promise is dropped, so the rethrow surfaces as an
 * `unhandledRejection` even though the caller already received a catchable
 * error. The harness installs a fail-loud `unhandledRejection` handler
 * (`dsh-app-boot`, "fatal load failure") and exits the whole process — which is
 * what a language server dying mid-request did:
 *
 *   fatal load failure: Error [ERR_STREAM_DESTROYED]:
 *     Cannot call write after a stream was destroyed
 *
 * A failed write is therefore swallowed here and reported through `onFailure`
 * instead. The client marks the session failed, terminates the subprocess and
 * disposes the connection, so every in-flight request rejects with an ordinary
 * error its call sites already catch.
 */

import type { MessageWriter } from 'vscode-languageserver-protocol'

/**
 * Wrap `base` so that a failed write resolves (`MessageWriter.write` promises
 * only "the write was handled"), handing the failure to `onFailure` for the
 * owner to end the session. `onError` / `onClose` still come from the wrapped
 * writer, so the connection keeps observing stream errors and closes.
 */
export function guardMessageWriter(
  base: MessageWriter,
  onFailure: (error: unknown) => void,
  logFailure?: (message: string, error: unknown) => void,
): MessageWriter {
  return {
    get onError() {
      return base.onError
    },
    get onClose() {
      return base.onClose
    },
    write: (message) =>
      base.write(message).then(
        () => undefined,
        (error: unknown) => {
          // Reporting, not rethrowing: see the module comment.
          try {
            onFailure(error)
          } catch (secondary) {
            // The owner must not turn reporting into a second failure — but a
            // handler that throws is still worth a log line instead of a
            // silent swallow.
            logFailure?.('dsh-lsp: guarded writer failure handler threw', secondary)
          }
          return undefined
        },
      ),
    end: () => {
      base.end()
    },
    dispose: () => {
      base.dispose()
    },
  }
}
