/**
 * Server registry: extension→server routing and instance lifecycle.
 *
 * Extension ownership is exclusive (enforced at config load), so a query
 * selects its server deterministically by file extension and never asks the
 * model to choose a provider. Each server has at most one live client;
 * `stop`/`restart` rebuild the client, and an unexpectedly exited client is
 * dropped so the next query lazily respawns it.
 *
 * Two invariants drive the start path:
 *   - starts are deduplicated through `inFlight`. Auto-start, a read hook and a
 *     query can all ask for one server at the same moment and must share a
 *     single subprocess (the previous sleep-50ms-and-recheck could let two
 *     callers build two clients).
 *   - a caller's abort signal never reaches the shared start. A language server
 *     outlives any single query, and a diagnostics timeout must stop the wait,
 *     never the process.
 */

import { extname, resolve } from 'node:path'

import { LspClient, type ProgressListener } from './client.js'
import { isLspDeadlineError } from './deadline.js'
import type { PublishRecord } from './inbox.js'
import { RootResolver } from './root.js'
import { toAbsolutePath, toDiagnosticEntries } from './protocol.js'
import type {
  DiagnosticEntry,
  DiagnosticsReport,
  IndexProgress,
  LspConfig,
  LoggerLike,
  ServerSpec,
  ServerStatus,
  SubprocessRuntime,
} from './types.js'

/** Effective diagnostics transport for a server. */
type Transports = 'push' | 'pull' | 'off'

/** A listener receiving every `publishDiagnostics` a managed client accepts. */
export type PublishListener = (serverId: string, record: PublishRecord) => void

/**
 * A listener receiving every tracked `$/progress` update, with the server and
 * root that produced it (a server id alone is ambiguous once several roots are
 * served by one id).
 */
export type IndexProgressListener = (serverId: string, root: string, progress: IndexProgress) => void

function extensionOf(filePath: string): string {
  const ext = extname(filePath)
  return ext.startsWith('.') ? ext.slice(1).toLowerCase() : ext.toLowerCase()
}

/** Render a millisecond budget for a note ("15000ms"). */
function budgetText(ms: number): string {
  return `${Math.max(0, Math.round(ms))}ms`
}

/** Render the age of a cached push for a note ("12s ago" / "3m ago"). */
function ageText(at: number): string {
  const seconds = Math.max(0, Math.round((Date.now() - at) / 1000))
  return seconds < 90 ? `${seconds}s ago` : `${Math.round(seconds / 60)}m ago`
}

export class ServerRegistry {
  private readonly config: LspConfig
  private readonly subprocess: SubprocessRuntime
  private readonly logger: LoggerLike
  private readonly roots = new RootResolver()

  private readonly byExtension = new Map<string, ServerSpec>()
  private readonly byId = new Map<string, ServerSpec>()
  private readonly clients = new Map<string, LspClient>()
  /** Starts in progress, keyed by server id: concurrent callers share one. */
  private readonly inFlight = new Map<string, Promise<LspClient>>()
  private readonly publishListeners = new Set<PublishListener>()
  private readonly progressListeners = new Set<IndexProgressListener>()

  constructor(config: LspConfig, subprocess: SubprocessRuntime, logger: LoggerLike) {
    this.config = config
    this.subprocess = subprocess
    this.logger = logger
    for (const spec of config.servers) {
      this.byId.set(spec.id, spec)
      for (const ext of spec.extensions) {
        this.byExtension.set(ext, spec)
      }
    }
  }

  get servers(): ServerSpec[] {
    return this.config.servers
  }

  serverForFile(filePath: string): ServerSpec {
    const ext = extensionOf(filePath)
    const spec = this.byExtension.get(ext)
    if (!spec) {
      throw new Error(`no LSP server for extension '.${ext}' (file: ${filePath})`)
    }
    return spec
  }

  /**
   * Start (or reuse) the client for `spec`, deduplicating concurrent callers.
   * The returned promise is shared: everyone awaiting the same start gets the
   * same client, and a failure drops the instance so the next caller retries.
   */
  private ensureStarted(spec: ServerSpec, startDir: string): Promise<LspClient> {
    const running = this.clients.get(spec.id)
    if (running && running.state === 'running') return Promise.resolve(running)
    const pending = this.inFlight.get(spec.id)
    if (pending) return pending

    const root = this.roots.resolve(startDir, spec)
    const fresh = new LspClient(spec, this.subprocess, this.logger, {}, this.config.logDir, this.config.timeoutMs)
    // The forwarder is attached at creation time: clients are built lazily, and
    // a push arriving between start() and a later subscription would be lost.
    fresh.onPublish(record => this.forwardPublish(spec.id, record))
    // Same reason as the diagnostics forwarder: progress from a server that
    // finishes building its index before anyone subscribes must not be lost.
    fresh.onIndexProgress(progress => this.forwardProgress(spec.id, fresh.root ?? root, progress))
    this.clients.set(spec.id, fresh)

    const startPromise = fresh.start(root).then(
      () => fresh,
      (error: unknown) => {
        if (this.clients.get(spec.id) === fresh) this.clients.delete(spec.id)
        throw error
      },
    )
    this.inFlight.set(spec.id, startPromise)
    const release = (): void => {
      if (this.inFlight.get(spec.id) === startPromise) this.inFlight.delete(spec.id)
    }
    void startPromise.then(release, release)
    return startPromise
  }

  /** Resolve the server + client for a file, starting it lazily if needed. */
  async resolve(filePath: string, cwd: string | undefined, _signal?: AbortSignal): Promise<{ spec: ServerSpec; client: LspClient; root: string }> {
    const spec = this.serverForFile(filePath)
    const startDir = toAbsolutePath(filePath, cwd)
    const client = await this.ensureStarted(spec, startDir)
    return { spec, client, root: client.root ?? startDir }
  }

  /**
   * Whether the file's server is already running (no spawn side effects).
   * Lets a read hook synchronously didOpen warm servers without paying a cold
   * start on every read of an unvisited file.
   */
  serverRunning(filePath: string): boolean {
    return this.runningClientFor(filePath) !== undefined
  }

  /** The running client serving a file, if there is one (no spawn side effects). */
  private runningClientFor(filePath: string): LspClient | undefined {
    try {
      const spec = this.serverForFile(filePath)
      const client = this.clients.get(spec.id)
      return client && client.state === 'running' ? client : undefined
    } catch {
      return undefined
    }
  }

  /** The live client for a server id, if any. */
  clientById(id: string): LspClient | undefined {
    return this.clients.get(id)
  }

  /**
   * Open a file on its server (didOpen from disk). Used by the read hook so a
   * file the AI just read is immediately queryable. Returns false when the file
   * has no server or the server could not be reached; never throws.
   */
  async openFile(filePath: string, cwd: string | undefined, signal?: AbortSignal): Promise<boolean> {
    try {
      const { client } = await this.resolve(filePath, cwd, signal)
      client.open(filePath)
      return true
    } catch {
      return false
    }
  }

  /**
   * Bring the server's copy of a file in sync with disk (didOpen/didChange)
   * without waiting for anything. On a cold project this is the call that
   * performs the first didOpen and therefore activates the server's index.
   */
  async refreshDocument(filePath: string, cwd: string | undefined): Promise<void> {
    const { client } = await this.resolve(filePath, cwd)
    client.refreshDocument(filePath)
  }

  /**
   * Subscribe to every accepted `publishDiagnostics` from a managed client.
   * The subscription outlives individual clients (the registry forwards), so a
   * listener registered at mount sees pushes from servers started later.
   */
  onPublishedDiagnostics(listener: PublishListener): { dispose(): void } {
    this.publishListeners.add(listener)
    return {
      dispose: () => {
        this.publishListeners.delete(listener)
      },
    }
  }

  private forwardPublish(serverId: string, record: PublishRecord): void {
    for (const listener of [...this.publishListeners]) {
      try {
        listener(serverId, record)
      } catch (error) {
        this.logger.debug(`dsh-lsp: diagnostics listener failed: ${error instanceof Error ? error.message : String(error)}`)
      }
    }
  }

  /**
   * Subscribe to every tracked `$/progress` update from a managed client.
   * Like the diagnostics subscription it outlives individual clients, so a
   * listener registered at mount sees servers started later.
   */
  onIndexProgress(listener: IndexProgressListener): { dispose(): void } {
    this.progressListeners.add(listener)
    return {
      dispose: () => {
        this.progressListeners.delete(listener)
      },
    }
  }

  private forwardProgress(serverId: string, root: string, progress: IndexProgress): void {
    for (const listener of [...this.progressListeners]) {
      try {
        listener(serverId, root, progress)
      } catch (error) {
        this.logger.debug(`dsh-lsp: progress listener failed: ${error instanceof Error ? error.message : String(error)}`)
      }
    }
  }

  /** Last background-index state of a server, if it reported one. */
  indexProgress(id: string): IndexProgress | null {
    return this.clients.get(id)?.indexProgress() ?? null
  }

  /**
   * Diagnostics for a just-written file.
   *
   * `push` (what clangd requires): refresh the document, then wait for the
   * server's next `publishDiagnostics`. The wait is bounded by
   * `diagnosticsTimeoutMs`; on expiry the most recent cached push is used
   * instead, so a server that does not re-publish still yields its last known
   * findings. Neither path aborts the server or closes a document.
   *
   * `pull`: the request/response path, only valid for a server that advertises
   * `diagnosticProvider`. The request carries the config's `timeoutMs` as a
   * hard deadline; on expiry the last pushed set (if any) is returned with a
   * note, because a server that never answers must not hang the tool.
   *
   * Both transports therefore terminate. The optional note records when the
   * answer is incomplete, so an empty entry list is never silently mistaken
   * for "clean" after a timeout.
   */
  async diagnosticsFor(filePath: string, cwd: string | undefined, signal?: AbortSignal): Promise<DiagnosticsReport> {
    const { spec, client } = await this.resolve(filePath, cwd)
    const mode = this.transportFor(client)
    if (mode === 'off') return { entries: [] }
    client.refreshDocument(filePath)

    if (mode === 'pull') {
      try {
        const items = await client.diagnostics(filePath, signal, this.config.timeoutMs)
        return { entries: toDiagnosticEntries(items) }
      } catch (error) {
        // A pull that outlived `timeoutMs` is not a server failure: the last
        // pushed set (if any) is still the server's own answer for this file,
        // so it is returned with a note instead of hanging or throwing.
        if (!isLspDeadlineError(error)) throw error
        const cached = client.cachedDiagnostics(filePath)
        const budget = budgetText(error.timeoutMs)
        const note = cached
          ? `pull diagnostics timed out after ${budget} (${spec.id}); showing the last pushed set, received ${ageText(cached.at)}`
          : `pull diagnostics timed out after ${budget} (${spec.id}); no pushed diagnostics are cached for this document`
        this.logger.warn(`dsh-lsp: ${note} [${filePath}]`)
        return { entries: toDiagnosticEntries(cached?.diagnostics), note }
      }
    }

    const pushed = await client.waitForDiagnostics(filePath, {
      timeoutMs: this.config.diagnosticsTimeoutMs,
      signal,
    })
    if (pushed) return { entries: toDiagnosticEntries(pushed) }
    const cached = client.cachedDiagnostics(filePath)
    const budget = budgetText(this.config.diagnosticsTimeoutMs)
    const note = cached
      ? `no new push from ${spec.id} within ${budget}; showing the last pushed set, received ${ageText(cached.at)}`
      : `no diagnostics push from ${spec.id} within ${budget}`
    return { entries: toDiagnosticEntries(cached?.diagnostics), note }
  }

  /**
   * Effective transport for a file's server, decided from the config pin and the
   * running client's capabilities. With no client running, `auto` reports
   * `push`: every mainstream server pushes, pull is the opt-in path.
   */
  diagnosticsMode(filePath: string): Transports {
    const pinned = this.config.diagnosticsMode
    if (pinned === 'off') return 'off'
    if (pinned === 'push' || pinned === 'pull') return pinned
    const client = this.runningClientFor(filePath)
    return client?.supportsPullDiagnostics() ? 'pull' : 'push'
  }

  private transportFor(client: LspClient): Transports {
    const pinned = this.config.diagnosticsMode
    if (pinned === 'off') return 'off'
    if (pinned === 'push' || pinned === 'pull') return pinned
    return client.supportsPullDiagnostics() ? 'pull' : 'push'
  }

  /** Resolve a server by id for `/lsp start <id>` (no file to route by). */
  serverById(id: string): ServerSpec | undefined {
    return this.byId.get(id)
  }

  async startById(id: string, cwd: string | undefined): Promise<void> {
    const spec = this.byId.get(id)
    if (!spec) throw new Error(`unknown server id '${id}'`)
    const startDir = cwd ? resolve(cwd) : process.cwd()
    await this.ensureStarted(spec, startDir)
  }

  async stopById(id: string): Promise<void> {
    this.inFlight.delete(id)
    const client = this.clients.get(id)
    if (client) {
      await client.stop()
      this.clients.delete(id)
    }
  }

  async stopAll(): Promise<void> {
    const ids = [...this.clients.keys()]
    await Promise.all(ids.map(id => this.stopById(id)))
  }

  /** Rebuild every running client (used by `/lsp restart`). */
  async restartAll(cwd: string | undefined): Promise<void> {
    const ids = [...this.clients.keys()]
    for (const id of ids) {
      await this.stopById(id)
    }
    this.roots.clear()
  }

  status(): ServerStatus[] {
    const out: ServerStatus[] = []
    for (const spec of this.config.servers) {
      const client = this.clients.get(spec.id)
      out.push({
        id: spec.id,
        languageId: spec.languageId,
        extensions: [...spec.extensions],
        state: client ? client.state : 'stopped',
        root: client ? client.root : null,
        pid: client ? client.pid : null,
        error: client ? client.error : null,
        index: client ? client.indexProgress() : null,
      })
    }
    return out
  }
}
