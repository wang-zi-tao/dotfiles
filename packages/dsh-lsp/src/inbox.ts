/**
 * Server-pushed diagnostics inbox.
 *
 * LSP servers announce diagnostics for a document with
 * \`textDocument/publishDiagnostics\`; the pull request
 * (\`textDocument/diagnostic\`) is optional and clangd does not implement it
 * (it answers \`method not found\`). The inbox is the single place a push lands,
 * so that:
 *   - a write hook can wait for the *next* push matching the text it just sent
 *     (no polling and no extra request),
 *   - a \`lsp diagnostics\` query can fall back to the most recent push,
 *   - a push carrying a document version older than the one this client sent is
 *     dropped, so stale diagnostics never reach the model.
 *
 * Bookkeeping only: no subprocess, no protocol requests, no timers beyond the
 * per-wait timeout. A timeout settles the promise with \`null\` and never
 * touches the server (see the plugin's \"a timeout must not abort the LSP\"
 * invariant).
 */

import type { Diagnostic } from 'vscode-languageserver-protocol'

import { fileUriToPath, isFileUri } from './protocol.js'

/** One accepted \`publishDiagnostics\` notification. */
export interface PublishRecord {
  /** The \`file:\` URI exactly as the server sent it. */
  readonly uri: string
  /** Absolute filesystem path derived from the URI. */
  readonly path: string
  /** Document version the server reported, when it reports one. */
  readonly version: number | undefined
  readonly diagnostics: readonly Diagnostic[]
  /** Local receive time in ms (\`Date.now()\`). */
  readonly at: number
}

/** Raw shape of a \`textDocument/publishDiagnostics\` payload. */
export interface PublishParams {
  uri: string
  version?: number
  diagnostics: readonly Diagnostic[]
}

export interface WaitDiagnosticsOptions {
  /** Hard upper bound on the wait. On expiry the wait resolves \`null\`. */
  timeoutMs: number
  /** Drop pushes whose version is older than this client-sent version. */
  minVersion?: number
  signal?: AbortSignal
}

interface Waiter {
  key: string
  minVersion: number | undefined
  resolve: (value: Diagnostic[] | null) => void
  timer: ReturnType<typeof setTimeout> | undefined
  signal: AbortSignal | undefined
  onAbort: (() => void) | undefined
}

/**
 * Path key for inbox lookups. Windows paths are case-insensitive and the same
 * document can arrive with different casing (drive letter, \`..\` segments), so
 * keys are case-folded on every platform: a collision between two genuinely
 * different POSIX paths that differ only in case is far less likely than a
 * missed match on Windows, and the consequence of a collision is only that the
 * most recent push wins.
 */
export function normalizePathKey(path: string): string {
  return path.toLowerCase()
}

export class DiagnosticInbox {
  private readonly records = new Map<string, PublishRecord>()
  private readonly waiters = new Set<Waiter>()
  private readonly listeners = new Set<(record: PublishRecord) => void>()

  /**
   * Accept one pushed notification. \`minVersion\` is the version this client
   * last sent for the document; a push that is older is ignored (the server is
   * still catching up with an edit we already made).
   */
  put(uri: string, params: PublishParams, minVersion?: number): void {
    if (!isFileUri(uri)) return
    let path: string
    try {
      path = fileUriToPath(uri)
    } catch {
      return
    }
    const version = typeof params.version === 'number' ? params.version : undefined
    // A push older than the version this client last sent describes text that
    // has already been replaced: it is dropped entirely (not cached, not
    // forwarded, not delivered) so stale findings cannot reach the model.
    if (version !== undefined && minVersion !== undefined && version < minVersion) return

    const record: PublishRecord = {
      uri,
      path,
      version,
      diagnostics: [...params.diagnostics],
      at: Date.now(),
    }
    const key = normalizePathKey(path)
    this.records.set(key, record)

    for (const waiter of [...this.waiters]) {
      if (waiter.key !== key) continue
      if (version !== undefined && waiter.minVersion !== undefined && version < waiter.minVersion) continue
      this.settle(waiter, record.diagnostics as Diagnostic[])
    }
    for (const listener of [...this.listeners]) {
      try {
        listener(record)
      } catch {
        /* a broken listener must never break ingest */
      }
    }
  }

  /** The most recent accepted push for a document, if any. */
  cached(path: string): PublishRecord | undefined {
    return this.records.get(normalizePathKey(path))
  }

  /**
   * Resolve with the next accepted push for \`path\`. Resolves \`null\` on
   * timeout or cancellation — both are cancellations of the *wait*, never of the
   * server.
   */
  wait(path: string, options: WaitDiagnosticsOptions): Promise<Diagnostic[] | null> {
    if (options.signal?.aborted) return Promise.resolve(null)
    const key = normalizePathKey(path)
    return new Promise<Diagnostic[] | null>((resolve) => {
      const waiter: Waiter = {
        key,
        minVersion: options.minVersion,
        resolve,
        timer: undefined,
        signal: options.signal,
        onAbort: undefined,
      }
      waiter.timer = setTimeout(() => this.settle(waiter, null), Math.max(1, options.timeoutMs))
      if (options.signal) {
        waiter.onAbort = () => this.settle(waiter, null)
        options.signal.addEventListener('abort', waiter.onAbort, { once: true })
      }
      this.waiters.add(waiter)
    })
  }

  /** Subscribe to accepted pushes. */
  onPublish(listener: (record: PublishRecord) => void): { dispose(): void } {
    this.listeners.add(listener)
    return { dispose: () => { this.listeners.delete(listener) } }
  }

  /**
   * Settle every pending waiter with \`null\` and drop cached records. Called on
   * client teardown so no promise is left hanging.
   */
  clear(): void {
    for (const waiter of [...this.waiters]) this.settle(waiter, null)
    this.records.clear()
  }

  private settle(waiter: Waiter, value: Diagnostic[] | null): void {
    if (!this.waiters.delete(waiter)) return
    if (waiter.timer !== undefined) clearTimeout(waiter.timer)
    if (waiter.signal && waiter.onAbort) waiter.signal.removeEventListener('abort', waiter.onAbort)
    waiter.resolve(value)
  }
}
