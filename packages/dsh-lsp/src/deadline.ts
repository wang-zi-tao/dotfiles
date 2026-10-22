/**
 * Hard deadlines for individual LSP requests.
 *
 * The plugin's invariant is "a timeout ends the wait, never the server": a
 * deadline aborts *this request* (so the client can cancel it best-effort) and
 * rejects with an {@link LspDeadlineError}; the subprocess, its documents and
 * its index are untouched. Every request the model can end up waiting on is
 * wrapped in one, because a server that accepts a request and simply never
 * answers would otherwise leave the caller awaiting a promise that never
 * settles.
 */

/** Thrown when an LSP operation outlives its deadline. */
export class LspDeadlineError extends Error {
  /** Operation label, e.g. `textDocument/diagnostic (pyright)`. */
  readonly label: string
  /** The deadline that expired, in milliseconds. */
  readonly timeoutMs: number

  constructor(label: string, timeoutMs: number) {
    super(`${label} timed out after ${timeoutMs}ms`)
    this.name = 'LspDeadlineError'
    this.label = label
    this.timeoutMs = timeoutMs
  }
}

/** Whether `value` is a deadline expiry (as opposed to a real request error). */
export function isLspDeadlineError(value: unknown): value is LspDeadlineError {
  return value instanceof LspDeadlineError
}

/** The error used when the caller's own signal aborts the wait. */
export function abortError(): Error {
  const error = new Error('request aborted')
  error.name = 'AbortError'
  return error
}


export interface DeadlineOptions {
  /** Hard upper bound in milliseconds (clamped to `>= 1`). */
  timeoutMs: number
  /** Human-readable operation name, carried by the thrown error. */
  label: string
  /** Caller cancellation: aborting it rejects immediately. */
  signal?: AbortSignal
}

/**
 * Run `run` under a hard deadline.
 *
 * The callback receives an `AbortSignal` that fires on timeout or caller
 * cancellation, so the underlying request can be cancelled. The deadline is
 * absolute — it never restarts when the request makes progress. A task that
 * settles after the deadline has already won the race is ignored, and its
 * rejection swallowed, so a late answer from a slow server can never surface
 * as an unhandled rejection.
 */
export async function withDeadline<T>(
  run: (signal: AbortSignal) => Promise<T>,
  options: DeadlineOptions,
): Promise<T> {
  const timeoutMs = Math.max(1, Math.trunc(options.timeoutMs))
  const controller = new AbortController()
  const outer = options.signal

  let failDeadline: (error: unknown) => void = () => {}
  const deadline = new Promise<never>((_resolve, reject) => {
    failDeadline = reject
  })
  const timer = setTimeout(() => {
    controller.abort()
    failDeadline(new LspDeadlineError(options.label, timeoutMs))
  }, timeoutMs)

  let failAbort: (error: unknown) => void = () => {}
  const aborted = new Promise<never>((_resolve, reject) => {
    failAbort = reject
  })
  const onAbort = (): void => {
    controller.abort()
    failAbort(abortError())
  }

  try {
    if (outer) {
      if (outer.aborted) {
        controller.abort()
        throw abortError()
      }
      outer.addEventListener('abort', onAbort, { once: true })
    }
    const task = Promise.resolve().then(() => run(controller.signal))
    // The loser of the race keeps running: swallow its eventual rejection.
    task.catch(() => {})
    return await Promise.race([task, deadline, aborted])
  } finally {
    clearTimeout(timer)
    if (outer) outer.removeEventListener('abort', onAbort)
  }
}
