/**
 * LSP server client: one managed subprocess + one JSON-RPC protocol
 * connection.
 *
 * Process management rides the harness's own `ctx.subprocess` seam (the same
 * tree-scoped spawn/terminate primitive the bash executor uses), so helper
 * processes cannot outlive the server and teardown is a single `terminate()`.
 * Protocol framing uses `vscode-jsonrpc`'s stream readers/writers, and every
 * request type is the type-safe `vscode-languageserver-protocol` constant.
 */

import { createProtocolConnection } from 'vscode-languageserver-protocol/node'
import {
  DefinitionRequest,
  DidChangeTextDocumentNotification,
  DidCloseTextDocumentNotification,
  DidOpenTextDocumentNotification,
  DocumentDiagnosticRequest,
  DocumentSymbolRequest,
  ExitNotification,
  HoverRequest,
  ImplementationRequest,
  InitializeRequest,
  InitializedNotification,
  PublishDiagnosticsNotification,
  ReferencesRequest,
  ShutdownRequest,
  TypeDefinitionRequest,
  WorkspaceSymbolRequest,
} from 'vscode-languageserver-protocol/node'
import type {
  CancellationToken,
  ClientCapabilities,
  Diagnostic,
  DocumentDiagnosticParams,
  DocumentSymbol,
  Hover,
  InitializeParams,
  Location,
  LocationLink,
  ProtocolConnection,
  SymbolInformation,
  WorkspaceSymbol,
} from 'vscode-languageserver-protocol'
import { appendFileSync, mkdirSync, readFileSync } from 'node:fs'
import { homedir } from 'node:os'
import { isAbsolute, join, resolve } from 'node:path'

import { withDeadline } from './deadline.js'
import { DiagnosticInbox, type PublishRecord } from './inbox.js'
import { foldIndexProgress, progressToken, type ProgressParamsLike } from './progress.js'
import { fileUriToPath, pathToFileUri, toLspPosition } from './protocol.js'
import type { IndexProgress, LoggerLike, ServerSpec, SubprocessHandle, SubprocessRuntime } from './types.js'

export type LspClientState = 'stopped' | 'starting' | 'running' | 'failed'

/** Default hard deadline for one LSP request (ms); the config's `timeoutMs` overrides it. */
export const DEFAULT_REQUEST_TIMEOUT_MS = 60_000

/**
 * Budget for the graceful `shutdown`/`exit` handshake (ms). Teardown runs on
 * plugin unload and on `/lsp restart`, so it must terminate even when the server
 * stopped answering; the process is tree-terminated right after either way.
 */
export const SHUTDOWN_TIMEOUT_MS = 1_000

/** A listener receiving every tracked `$/progress` update from one client. */
export type ProgressListener = (progress: IndexProgress) => void

/** The three capabilities the plugin actually queries. */
export const CLIENT_CAPABILITIES = {
  // clangd only reports its background index when the client advertises work
  // done progress; without it the index builds silently and the plugin cannot
  // tell "no such symbol" from "the index is not ready yet".
  window: {
    workDoneProgress: true,
  },
  workspace: {
    workspaceFolders: true,
  },
  textDocument: {
    hover: { contentFormat: ['markdown', 'plaintext'] },
    definition: { linkSupport: true },
    typeDefinition: { linkSupport: true },
    implementation: { linkSupport: true },
    references: {},
    documentSymbol: {
      hierarchicalDocumentSymbolSupport: true,
    },
    diagnostic: {},
    publishDiagnostics: {},
  },
} as const

interface InitializationResult {
  serverInfo?: { name?: string; version?: string }
  /** Server capabilities; only the pull-diagnostics advertisement is consulted. */
  capabilities?: { diagnosticProvider?: unknown }
}

/** Minimal shape of a `textDocument/diagnostic` answer. */
type PullDiagnosticsReport = { kind?: string; items?: Diagnostic[] } | null

export class LspClient {
  readonly spec: ServerSpec
  private readonly subprocess: SubprocessRuntime
  private readonly logger: LoggerLike
  private readonly initializeResult: InitializationResult

  state: LspClientState = 'stopped'
  root: string | null = null
  pid: number | null = null
  error: string | null = null

  private connection: ProtocolConnection | null = null
  private handle: SubprocessHandle | null = null
  private initialized = false
  private nextId = 1
  private readonly logDir?: string
  /** Hard deadline for one request and for the initialize handshake (ms). */
  private readonly requestTimeoutMs: number

  /**
   * Pushed diagnostics for this client's documents. One inbox per client (one
   * client per server) so a wait can never consume another server's push.
   */
  private readonly inbox = new DiagnosticInbox()

  /** Last state per `$/progress` token (clangd: `backgroundIndexProgress`). */
  private readonly progress = new Map<string, IndexProgress>()
  private readonly progressListeners = new Set<ProgressListener>()

  constructor(
    spec: ServerSpec,
    subprocess: SubprocessRuntime,
    logger: LoggerLike,
    initializeResult: InitializationResult = {},
    logDir?: string,
    requestTimeoutMs: number = DEFAULT_REQUEST_TIMEOUT_MS,
  ) {
    this.spec = spec
    this.subprocess = subprocess
    this.logger = logger
    this.initializeResult = initializeResult
    this.logDir = logDir
    this.requestTimeoutMs = Math.max(1, Math.trunc(requestTimeoutMs))
  }

  /**
   * Resolve a possibly-relative file path to an absolute one. Queries carry a
   * `filePath` that may be workspace-relative (the tool accepts either form);
   * the server is spawned with cwd=root, so a bare relative path would resolve
   * against the wrong directory inside `readFileSync`/`pathToFileUri` and
   * produce a malformed `file:///Coding/...` URI that crashes clangd.
   */
  private absPath(filePath: string): string {
    if (isAbsolute(filePath)) return filePath
    const base = this.root ?? process.cwd()
    return resolve(base, filePath)
  }

  /** Start the server subprocess and complete the initialize handshake. */
  async start(root: string, signal?: AbortSignal): Promise<void> {
    if (this.state === 'running' || this.state === 'starting') return
    this.state = 'starting'
    this.root = root
    this.error = null

    try {
      const handle = this.subprocess.spawn({
        argv: [this.spec.command, ...this.spec.args],
        cwd: root,
        stdio: {
          stdin: 'pipe',
          stdout: 'pipe',
          stderr: { maxBytes: 64 * 1024 },
        },
        graceMs: 5000,
        ...(signal ? { signal } : {}),
      })
      this.handle = handle
      this.pid = handle.pid

      // A process that exits before initialization fails the start.
      handle.done.then((outcome) => {
        if (this.state === 'starting') {
          this.state = 'failed'
          this.error = `server exited before initialization (exitCode=${outcome.exitCode}, signal=${outcome.signal})`
        }
      }).catch((err) => {
        if (this.state === 'starting') {
          this.state = 'failed'
          this.error = `spawn failed: ${err instanceof Error ? err.message : String(err)}`
        }
      })

      if (!handle.stdout || !handle.stdin) {
        throw new Error('server stdio did not expose piped streams')
      }

      const connection = createProtocolConnection(handle.stdout, handle.stdin)
      this.connection = connection
      connection.listen()

      // Long-running server work (clangd's background index) arrives as
      // `$/progress` under a server-chosen token. clangd first asks for a
      // progress token with `window/workDoneProgress/create` unless the client
      // advertises implicit creation, so the request is accepted with the
      // spec's `null` result; a rejected request would disable progress for the
      // whole session (clangd gives up after one failure).
      connection.onRequest('window/workDoneProgress/create', () => null)
      connection.onNotification('$/progress', (params) => {
        this.acceptProgress(params as ProgressParamsLike)
      })
      // Diagnostics are push-only on clangd: the pull request answers
      // "method not found". The version gate uses what this client last sent
      // for the document, so a push racing ahead of a fresh didChange is
      // dropped instead of describing stale text.
      connection.onNotification(PublishDiagnosticsNotification.type, (params) => {
        let sentVersion: number | undefined
        try {
          sentVersion = this.documentVersions.get(fileUriToPath(params.uri))
        } catch {
          sentVersion = undefined
        }
        this.inbox.put(params.uri, params, sentVersion)
      })

      const rootUri = pathToFileUri(root)
      const initParams: InitializeParams = {
        processId: process.pid,
        rootUri,
        workspaceFolders: [{ uri: rootUri, name: root }],
        capabilities: CLIENT_CAPABILITIES as unknown as ClientCapabilities,
        initializationOptions: undefined,
      }

      // The handshake is bounded: a server that never answers `initialize`
      // would otherwise leave every later query awaiting this promise forever.
      const result: InitializationResult | null = await withDeadline(
        deadlineSignal =>
          connection.sendRequest(InitializeRequest.type, initParams, this.token(deadlineSignal)) as Promise<InitializationResult | null>,
        { timeoutMs: this.requestTimeoutMs, label: `initialize (${this.spec.id})`, ...(signal ? { signal } : {}) },
      )
      this.initializeResult.serverInfo = result?.serverInfo
      this.initializeResult.capabilities = result?.capabilities
      await connection.sendNotification(InitializedNotification.type, {})
      this.initialized = true
      this.state = 'running'
      this.error = null

      // Surface an unexpected exit so the registry can drop the instance and
      // the next query restarts it. Persist the server's stderr tail to the
      // log directory so a crash (e.g. clangd exit code 1) is diagnosable:
      // without it, the fatal error clangd wrote to stderr is silently lost.
      handle.done.then((outcome) => {
        // A failed notification write (see noteWriteFailure) may already have
        // marked the client failed; the process exit is still when its stderr
        // tail is complete, so drain it either way and only attribute the exit
        // here while the client still believed it was running.
        if (this.state !== 'running' && this.state !== 'failed') return
        const stderrText = this.drainStderr()
        if (stderrText) this.writeLog(stderrText)
        if (this.state !== 'running') return
        this.state = 'failed'
        this.error = `server exited unexpectedly (exitCode=${outcome.exitCode}, signal=${outcome.signal})`
        this.logger.warn(`dsh-lsp: ${this.spec.id} exited: ${this.error}${stderrText ? `\n${stderrText.slice(0, 2000)}` : ''}`)
      }).catch(() => { /* handled on the starting path */ })
    } catch (err) {
      this.state = 'failed'
      this.error = err instanceof Error ? err.message : String(err)
      this.logger.warn(`dsh-lsp: failed to start ${this.spec.id}: ${this.error}`)
      // Best-effort cleanup of a half-started process.
      try {
        this.handle?.terminate()
      } catch {
        /* ignore */
      }
      throw err
    }
  }

  /**
   * Fold one `$/progress` notification into the tracked state and fan it out
   * to the registry. A bad notification (foreign token, unknown kind) is
   * ignored: progress is informative, never a reason to fail a session.
   */
  private acceptProgress(params: ProgressParamsLike): void {
    const token = progressToken(params)
    if (token === null) return
    const next = foldIndexProgress(this.progress.get(token), params)
    if (!next) return
    this.progress.set(token, next)
    for (const listener of [...this.progressListeners]) {
      try {
        listener(next)
      } catch (error) {
        this.logger.debug(
          'dsh-lsp: progress listener failed for ' + this.spec.id + ': ' + (error instanceof Error ? error.message : String(error)),
        )
      }
    }
  }

  /**
   * The most relevant tracked progress: a still-running stream wins over a
   * finished one, so `/lsp status` and empty symbol answers report the index
   * that is being built right now rather than a previous run.
   */
  indexProgress(): IndexProgress | null {
    let latest: IndexProgress | null = null
    for (const entry of this.progress.values()) {
      if (latest === null) {
        latest = entry
        continue
      }
      if (latest.done !== entry.done) {
        if (latest.done) latest = entry
        continue
      }
      if (entry.updatedAt >= latest.updatedAt) latest = entry
    }
    return latest
  }

  /** Subscribe to progress updates; the subscription is per client instance. */
  onIndexProgress(listener: ProgressListener): { dispose(): void } {
    this.progressListeners.add(listener)
    return {
      dispose: () => {
        this.progressListeners.delete(listener)
      },
    }
  }

  private requireConnection(): ProtocolConnection {
    if (!this.connection || !this.initialized || this.state !== 'running') {
      throw new Error(`server '${this.spec.id}' is not running`)
    }
    return this.connection
  }

  /**
   * Fire a notification that no caller awaits. The write's rejection must never
   * escape: a server that died (crash, OOM kill, explicit terminate) leaves the
   * pipe closed, and the next write makes node's socket raise `EPIPE` ("This
   * socket has been ended by the other party") from inside the promise
   * `sendNotification` returns. An unhandled rejection is fatal to the harness
   * process, so every fire-and-forget write funnels through here.
   */
  private postNotification(what: string, send: () => Promise<void>): void {
    let written: Promise<void>
    try {
      written = send()
    } catch (err) {
      // A closed/disposed connection throws synchronously rather than rejecting.
      this.noteWriteFailure(what, err)
      return
    }
    written.catch((err: unknown) => this.noteWriteFailure(what, err))
  }

  /**
   * Record a failed write to a server that is supposed to be running. The
   * session is over: mark the client failed so {@link requireConnection} stops
   * handing out a dead transport and the registry respawns on the next query,
   * and reap the process so a half-dead server cannot linger. Never rethrows —
   * this runs on a detached write.
   */
  private noteWriteFailure(what: string, err: unknown): void {
    const message = err instanceof Error ? err.message : String(err)
    if (this.state !== 'running') {
      this.logger.debug(`dsh-lsp: ${this.spec.id} ${what} failed while ${this.state}: ${message}`)
      return
    }
    this.state = 'failed'
    this.error = `${what} failed: ${message}`
    this.logger.warn(`dsh-lsp: ${this.spec.id} ${what} failed: ${message}`)
    try {
      this.handle?.terminate()
    } catch {
      /* ignore */
    }
  }

  /**
   * Ensure a document is open on the server. LSP servers (clangd included)
   * answer position-sensitive queries only for documents they know; without
   * `textDocument/didOpen`, clangd rejects hover/typeDefinition with
   * `trying to get AST for non-added document`. Documents are opened lazily
   * and kept open for the server's lifetime (cheap, and matches how an editor
   * keeps buffers resident).
   */
  private readonly openDocuments = new Set<string>()
  private readonly documentVersions = new Map<string, number>()

  private ensureOpen(filePath: string): void {
    if (this.openDocuments.has(filePath)) return
    const uri = pathToFileUri(filePath)
    let text: string
    try {
      text = readFileSync(filePath, 'utf8')
    } catch {
      // The file may be unsaved/missing; open with empty content so the
      // server still has a buffer. Queries will simply return no result.
      text = ''
    }
    const conn = this.requireConnection()
    this.postNotification('didOpen', () =>
      conn.sendNotification(DidOpenTextDocumentNotification.type, {
        textDocument: {
          uri,
          languageId: this.spec.languageId,
          version: 1,
          text,
        },
      }),
    )
    this.openDocuments.add(filePath)
    this.documentVersions.set(filePath, 1)
  }

  /** Public entry for external callers to didOpen a file (reads current disk content). */
  open(filePath: string): void {
    const abs = this.absPath(filePath)
    this.ensureOpen(abs)
  }

  /**
   * Bring the server's in-memory copy of an already-open document in sync with
   * the current disk content (full-document didChange). Used after the AI
   * writes/edits a file so the server's next push (or a pull) describes the new
   * text, and so the first refresh on a cold project performs the didOpen that
   * activates the project's index. For a not-yet-open document this falls back
   * to didOpen.
   */
  refreshDocument(filePath: string): void {
    const abs = this.absPath(filePath)
    const uri = pathToFileUri(abs)
    let text: string
    try {
      text = readFileSync(abs, 'utf8')
    } catch {
      text = ''
    }
    const conn = this.requireConnection()
    if (this.openDocuments.has(abs)) {
      const version = (this.documentVersions.get(abs) ?? 1) + 1
      this.documentVersions.set(abs, version)
      this.postNotification('didChange', () =>
        conn.sendNotification(DidChangeTextDocumentNotification.type, {
          textDocument: { uri, version },
          contentChanges: [{ text }],
        }),
      )
    } else {
      this.ensureOpen(abs)
    }
  }

  /** Whether the server advertises pull diagnostics (`textDocument/diagnostic`). */
  supportsPullDiagnostics(): boolean {
    return this.initializeResult.capabilities?.diagnosticProvider !== undefined
  }

  /** The most recent pushed diagnostics for a document, if any. */
  cachedDiagnostics(filePath: string): PublishRecord | undefined {
    return this.inbox.cached(this.absPath(filePath))
  }

  /**
   * Resolve with the server's next push for a document. `null` means the wait
   * expired or was cancelled: the server and its documents are untouched.
   */
  waitForDiagnostics(
    filePath: string,
    options: { timeoutMs: number; minVersion?: number; signal?: AbortSignal },
  ): Promise<Diagnostic[] | null> {
    return this.inbox.wait(this.absPath(filePath), options)
  }

  /** Subscribe to this client's pushes (the registry forwards them onward). */
  onPublish(listener: (record: PublishRecord) => void): { dispose(): void } {
    return this.inbox.onPublish(listener)
  }

  /** Whether a cancellation could still reach a live server. */
  private canCancel(): boolean {
    return this.connection !== null && (this.state === 'running' || this.state === 'starting')
  }

  /**
   * The cancellation token handed to one request.
   *
   * `vscode-jsonrpc` answers a cancellation by writing a `$/cancelRequest`
   * notification, and writing to an already-closed connection throws
   * `ConnectionError: Connection is closed`. Cancellation is delivered from an
   * abort listener — i.e. synchronously inside `controller.abort()`, usually
   * called from a timer or a teardown hook — so such a throw escapes as an
   * `uncaughtException` and kills the host process. That is exactly what a hot
   * reload did: teardown aborted in-flight runs whose connections were already
   * gone. Hence: never notify a dead connection, and never let a listener throw
   * out of the abort dispatch.
   */
  private token(signal?: AbortSignal): CancellationToken {
    if (!signal) return { get isCancellationRequested() { return false }, onCancellationRequested() { return { dispose() {} } } }
    return {
      get isCancellationRequested() { return signal.aborted },
      onCancellationRequested: (listener: (error?: unknown) => void) => {
        const notify = (): void => {
          if (!this.canCancel()) {
            this.logger.debug(`dsh-lsp: dropped a cancellation for ${this.spec.id}: connection is closed`)
            return
          }
          try {
            listener(new Error('aborted'))
          } catch (error) {
            this.logger.debug(
              `dsh-lsp: cancellation for ${this.spec.id} failed: ${error instanceof Error ? error.message : String(error)}`,
            )
          }
        }
        if (signal.aborted) {
          notify()
          return { dispose() {} }
        }
        signal.addEventListener('abort', notify, { once: true })
        return { dispose() { signal.removeEventListener('abort', notify) } }
      },
    }
  }

  private async normalizeLocations(
    value: Location | Location[] | LocationLink[] | null,
  ): Promise<Array<{ uri: string; range: import('vscode-languageserver-protocol').Range }>> {
    if (value === null || value === undefined) return []
    const arr: Array<Location | LocationLink> = Array.isArray(value) ? value : [value]
    const out: Array<{ uri: string; range: import('vscode-languageserver-protocol').Range }> = []
    for (const item of arr) {
      if ('targetUri' in item && item.targetUri) {
        // LocationLink
        out.push({ uri: item.targetUri, range: item.targetSelectionRange })
      } else if ('uri' in item && item.uri) {
        out.push({ uri: item.uri, range: item.range })
      }
    }
    return out
  }

  async definition(filePath: string, line: number, character: number, signal?: AbortSignal) {
    const abs = this.absPath(filePath)
    this.ensureOpen(abs)
    const conn = this.requireConnection()
    const params = {
      textDocument: { uri: pathToFileUri(abs) },
      position: toLspPosition(line, character),
    }
    const value = await conn.sendRequest(DefinitionRequest.type, params, this.token(signal))
    return this.normalizeLocations(value as Location | Location[] | LocationLink[] | null)
  }

  async typeDefinition(filePath: string, line: number, character: number, signal?: AbortSignal) {
    const abs = this.absPath(filePath)
    this.ensureOpen(abs)
    const conn = this.requireConnection()
    const params = {
      textDocument: { uri: pathToFileUri(abs) },
      position: toLspPosition(line, character),
    }
    const value = await conn.sendRequest(TypeDefinitionRequest.type, params, this.token(signal))
    return this.normalizeLocations(value as Location | Location[] | LocationLink[] | null)
  }

  async implementation(filePath: string, line: number, character: number, signal?: AbortSignal) {
    const abs = this.absPath(filePath)
    this.ensureOpen(abs)
    const conn = this.requireConnection()
    const params = {
      textDocument: { uri: pathToFileUri(abs) },
      position: toLspPosition(line, character),
    }
    const value = await conn.sendRequest(ImplementationRequest.type, params, this.token(signal))
    return this.normalizeLocations(value as Location | Location[] | LocationLink[] | null)
  }

  async references(filePath: string, line: number, character: number, signal?: AbortSignal) {
    const abs = this.absPath(filePath)
    this.ensureOpen(abs)
    const conn = this.requireConnection()
    const params = {
      textDocument: { uri: pathToFileUri(abs) },
      position: toLspPosition(line, character),
      context: { includeDeclaration: true },
    }
    const value = await conn.sendRequest(ReferencesRequest.type, params, this.token(signal))
    const arr: Array<Location> = Array.isArray(value) ? (value as Location[]) : []
    return arr.map(loc => ({ uri: loc.uri, range: loc.range }))
  }

  async hover(filePath: string, line: number, character: number, signal?: AbortSignal): Promise<Hover | null> {
    const abs = this.absPath(filePath)
    this.ensureOpen(abs)
    const conn = this.requireConnection()
    const params = {
      textDocument: { uri: pathToFileUri(abs) },
      position: toLspPosition(line, character),
    }
    return conn.sendRequest(HoverRequest.type, params, this.token(signal))
  }

  async workspaceSymbol(filePath: string, query: string, signal?: AbortSignal): Promise<SymbolInformation[] | WorkspaceSymbol[] | null> {
    // Warm the dynamic index before querying. clangd's workspace/symbol only
    // searches its Dex index, which is dynamic (didOpen'd files) plus the
    // background index. On a cold start the 1.5 GB background index loads
    // slowly (140s+ of "Failed to load shard" before it answers), but
    // didOpen'ing the target file and forcing an AST parse fills the dynamic
    // index immediately. A documentSymbol round-trip is the parse barrier: it
    // returns only once the translation unit's AST is ready, after which the
    // file's symbols are queryable.
    try {
      await this.documentSymbol(filePath, signal)
    } catch {
      // A broken translation unit may fail the parse; still attempt the query.
    }
    const conn = this.requireConnection()
    return conn.sendRequest(WorkspaceSymbolRequest.type, { query }, this.token(signal))
  }

  async documentSymbol(filePath: string, signal?: AbortSignal): Promise<DocumentSymbol[] | SymbolInformation[] | null> {
    const abs = this.absPath(filePath)
    this.ensureOpen(abs)
    const conn = this.requireConnection()
    return conn.sendRequest(DocumentSymbolRequest.type, { textDocument: { uri: pathToFileUri(abs) } }, this.token(signal))
  }

  /**
   * Parse a bounded set of candidate documents so their symbols enter the
   * server's dynamic index — the same parse barrier `workspaceSymbol` uses for
   * its target file, applied to candidates found by an external search.
   * Fail-open per file: a candidate that fails to parse is skipped rather than
   * aborting the pass. Returns the documents that parsed successfully.
   */
  async warmDocuments(
    paths: readonly string[],
    options: { concurrency?: number; signal?: AbortSignal } = {},
  ): Promise<string[]> {
    const unique: string[] = []
    const seen = new Set<string>()
    for (const path of paths) {
      // Windows paths are case-insensitive: dedupe on the lowercase form.
      const key = path.toLowerCase()
      if (seen.has(key)) continue
      seen.add(key)
      unique.push(path)
    }
    if (unique.length === 0) return []

    const concurrency = Math.max(1, Math.min(options.concurrency ?? 4, unique.length))
    const warmed: string[] = []
    // A shared cursor keeps every worker busy without re-parsing a document.
    let cursor = 0
    const workers = Array.from({ length: concurrency }, async () => {
      for (;;) {
        const index = cursor++
        if (index >= unique.length) return
        if (options.signal?.aborted) return
        const path = unique[index] as string
        try {
          await this.documentSymbol(path, options.signal)
          warmed.push(path)
        } catch {
          // Skipped: not a valid translation unit, or the parse was cancelled.
        }
      }
    })
    await Promise.all(workers)
    return warmed
  }

  /**
   * Pull diagnostics (`textDocument/diagnostic`). The request is optional in
   * LSP and clangd answers `method not found`, so this path is only taken when
   * {@link supportsPullDiagnostics} is true.
   */
  async diagnostics(
    filePath: string,
    signal?: AbortSignal,
    timeoutMs: number = this.requestTimeoutMs,
  ): Promise<Diagnostic[] | null> {
    const abs = this.absPath(filePath)
    this.ensureOpen(abs)
    const conn = this.requireConnection()
    const params: DocumentDiagnosticParams = {
      textDocument: { uri: pathToFileUri(abs) },
      identifier: `dsh-lsp-${this.nextId++}`,
      previousResultId: undefined,
    }
    // A pull request has no server-side deadline. Without one here, a server
    // that accepts `textDocument/diagnostic` and never answers would leave the
    // caller awaiting forever. The deadline cancels this request only: the
    // server keeps running and its documents stay open.
    const report = await withDeadline(
      deadlineSignal =>
        conn.sendRequest(DocumentDiagnosticRequest.type, params, this.token(deadlineSignal)) as Promise<PullDiagnosticsReport>,
      { timeoutMs, label: `textDocument/diagnostic (${this.spec.id})`, ...(signal ? { signal } : {}) },
    )
    if (!report) return null
    if (report.kind === 'full') return report.items ?? []
    // 'unchanged' reports carry no items; return empty rather than erroring.
    return []
  }

  /** Drain the subprocess's collected stderr tail (if any) to a string. */
  private drainStderr(): string {
    const stderr = this.handle?.collected?.stderr
    if (!stderr) return ''
    try {
      const { text } = stderr.finalize()
      return text.trim()
    } catch {
      return ''
    }
  }

  /** Resolve the per-server log file path and append one stderr snapshot. */
  private writeLog(text: string): void {
    if (!this.logDir || !text) return
    try {
      const dir = this.logDir.replace(/^~(?=$|[\\/])/, homedir())
      mkdirSync(dir, { recursive: true })
      const safeId = this.spec.id.replace(/[^a-zA-Z0-9_-]/g, '_')
      const file = join(dir, `${safeId}.log`)
      appendFileSync(file, `\n--- ${new Date().toISOString()} (pid=${this.pid}) ---\n${text}\n`)
    } catch {
      /* logging is best-effort; never break server teardown on a log failure */
    }
  }

  /** Graceful shutdown: `shutdown` request, `exit` notification, then tree termination. */
  async stop(): Promise<void> {
    if (this.state === 'stopped') return
    // Persist whatever stderr was collected before teardown completes.
    const stderrText = this.drainStderr()
    if (stderrText) this.writeLog(stderrText)
    const conn = this.connection
    if (conn && this.initialized) {
      try {
        // Bounded: a server that accepts `shutdown` and never answers must not
        // wedge unload / restart. The process is terminated below regardless.
        await withDeadline(() => conn.sendRequest(ShutdownRequest.type, undefined), {
          timeoutMs: SHUTDOWN_TIMEOUT_MS,
          label: `shutdown (${this.spec.id})`,
        })
        await conn.sendNotification(ExitNotification.type)
      } catch {
        /* server may already be gone, or stopped answering */
      }
    }
    try {
      this.connection?.dispose()
    } catch {
      /* ignore */
    }
    try {
      this.handle?.terminate()
    } catch {
      /* ignore */
    }
    this.connection = null
    this.handle = null
    this.initialized = false
    this.pid = null
    this.state = 'stopped'
    this.error = null
    this.inbox.clear()
  }
}
