/**
 * Continuous diagnostics broadcast.
 *
 * A language server pushes diagnostics whenever it re-analyses a file — after
 * our own write, after another session's edit, after a header change cascades
 * through a translation unit. Nothing here is armed per write: every push is
 * observed, diffed against the last state reported for that file, and only the
 * *added* and *removed* findings are injected into the agents listening on that
 * server, so the model is told what changed instead of the same list again.
 *
 * Semantics:
 *   - pushes arrive in bursts (one header edit can re-publish dozens of files):
 *     they are coalesced over `flushMs` into one message per server+root;
 *   - a file's state is diffed as a multiset, so an unchanged re-publish
 *     injects nothing;
 *   - the message goes to every live agent rooted inside the server's root
 *     (`targetsFor`), not just the session that last wrote the file;
 *   - injection uses `Agent.inject`, which queues the message for the agent's
 *     next pre-step and never wakes the driver.
 */

import type { UserMessage } from '@deepseek-ai/dsh-session'

import {
  diffDiagnostics,
  diagnosticsChangeMessage,
  filterDiagnostics,
  type DiagnosticsFileChange,
} from './diagnostics.js'
import { normalizePathKey, type PublishRecord } from './inbox.js'
import { isPathInside, toDiagnosticEntries } from './protocol.js'
import type { DiagnosticEntry, DiagnosticSeverity, LoggerLike } from './types.js'

/** The minimal agent surface the broadcaster needs (kept structural for testability). */
export interface InjectableAgent {
  id: string
  inject(message: UserMessage): void
}

export interface DiagnosticBroadcasterOptions {
  /** Only findings at or above this severity are tracked and injected. */
  minSeverity: DiagnosticSeverity
  /** Coalescing window in which pushes are folded into one message (ms). */
  flushMs: number
  /** Upper bound on diagnostics listed in one injected message. */
  maxDiagnostics: number
  logger: LoggerLike
  /** Live agents that should hear about a change reported under `root`. */
  targetsFor(serverId: string, root: string): readonly InjectableAgent[]
}

/** One file whose latest reported diagnostics have not been flushed yet. */
interface PendingFile {
  serverId: string
  root: string
  pathKey: string
  path: string
  entries: DiagnosticEntry[]
}

/** Changes for one server+root, injected as a single message. */
interface ChangeGroup {
  serverId: string
  root: string
  changes: DiagnosticsFileChange[]
}

/** Bounded so a long session cannot grow the baseline table without limit. */
const MAX_TRACKED_FILES = 2048

/**
 * Trim the changes to `maxDiagnostics` listed entries. Additions win the
 * budget: a finding that appeared is what the model has to fix, a finding that
 * disappeared is only news.
 */
function capChanges(
  changes: readonly DiagnosticsFileChange[],
  maxDiagnostics: number,
): { changes: DiagnosticsFileChange[]; omitted: number } {
  const budget = Math.max(1, Math.floor(maxDiagnostics))
  let used = 0
  let omitted = 0
  const capped: DiagnosticsFileChange[] = []
  for (const change of changes) {
    const added: DiagnosticEntry[] = []
    for (const entry of change.added) {
      if (used < budget) {
        added.push(entry)
        used += 1
      } else {
        omitted += 1
      }
    }
    const removed: DiagnosticEntry[] = []
    for (const entry of change.removed) {
      if (used < budget) {
        removed.push(entry)
        used += 1
      } else {
        omitted += 1
      }
    }
    if (added.length > 0 || removed.length > 0) capped.push({ path: change.path, added, removed })
  }
  return { changes: capped, omitted }
}

export class DiagnosticBroadcaster {
  private readonly options: DiagnosticBroadcasterOptions
  /** Last flushed, severity-filtered findings per server+file. */
  private readonly known = new Map<string, DiagnosticEntry[]>()
  /** Files observed since the last flush, holding their newest report. */
  private readonly dirty = new Map<string, PendingFile>()
  private timer: ReturnType<typeof setTimeout> | undefined
  private disposed = false

  constructor(options: DiagnosticBroadcasterOptions) {
    this.options = options
  }

  /** Handle one push forwarded by the registry (the push transport). */
  accept(serverId: string, root: string, record: PublishRecord): void {
    this.observe(serverId, root, record.path, toDiagnosticEntries(record.diagnostics))
  }

  /**
   * Record a file's newest full diagnostic set. Also the entry point for the
   * pull transport, where the caller already has entries instead of a push.
   */
  observe(
    serverId: string,
    root: string,
    path: string,
    entries: readonly DiagnosticEntry[],
  ): void {
    if (this.disposed) return
    if (!isPathInside(root, path)) {
      // A server may report on files outside its own root (system headers via a
      // translated path); those findings are not actionable in this workspace.
      this.options.logger.debug('dsh-lsp: ignoring diagnostics outside the server root for ' + path)
      return
    }
    const pathKey = normalizePathKey(path)
    this.dirty.set(serverId + '\u0000' + pathKey, {
      serverId,
      root,
      pathKey,
      path,
      entries: filterDiagnostics(entries, this.options.minSeverity),
    })
    this.schedule()
  }

  /** Flush pending changes now (also used by tests and by teardown). */
  flush(): void {
    if (this.timer !== undefined) {
      clearTimeout(this.timer)
      this.timer = undefined
    }
    if (this.dirty.size === 0) return
    const pending = [...this.dirty.values()]
    this.dirty.clear()

    const groups = new Map<string, ChangeGroup>()
    for (const file of pending) {
      const key = file.serverId + '\u0000' + file.pathKey
      const previous = this.known.get(key) ?? []
      const diff = diffDiagnostics(previous, file.entries)
      this.remember(key, file.entries)
      if (diff.added.length === 0 && diff.removed.length === 0) continue
      const groupKey = file.serverId + '\u0000' + file.root.toLowerCase()
      const group = groups.get(groupKey)
      const change: DiagnosticsFileChange = { path: file.path, added: diff.added, removed: diff.removed }
      if (group) group.changes.push(change)
      else groups.set(groupKey, { serverId: file.serverId, root: file.root, changes: [change] })
    }

    for (const group of groups.values()) this.inject(group)
  }

  /** Drop timers and baseline state (plugin unload / HMR). */
  dispose(): void {
    this.disposed = true
    if (this.timer !== undefined) {
      clearTimeout(this.timer)
      this.timer = undefined
    }
    this.dirty.clear()
    this.known.clear()
  }

  private inject(group: ChangeGroup): void {
    let targets: readonly InjectableAgent[]
    try {
      targets = this.options.targetsFor(group.serverId, group.root)
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error)
      this.options.logger.debug('dsh-lsp: diagnostics target resolution failed: ' + message)
      return
    }
    if (targets.length === 0) {
      this.options.logger.debug(
        'dsh-lsp: no agent session rooted at ' + group.root + ' for ' + group.serverId + ' diagnostics',
      )
      return
    }

    const { changes, omitted } = capChanges(group.changes, this.options.maxDiagnostics)
    if (changes.length === 0) return
    const message = diagnosticsChangeMessage(group.serverId, changes, omitted)
    let added = 0
    let removed = 0
    for (const change of changes) {
      added += change.added.length
      removed += change.removed.length
    }
    for (const agent of targets) {
      try {
        agent.inject(message)
      } catch (error) {
        const reason = error instanceof Error ? error.message : String(error)
        this.options.logger.debug('dsh-lsp: diagnostics injection into agent ' + agent.id + ' failed: ' + reason)
      }
    }
    this.options.logger.info(
      'dsh-lsp: injected +' + added + ' / -' + removed + ' diagnostic(s) for ' + changes.length +
      ' file(s) from ' + group.serverId + ' into ' + targets.length + ' agent(s)',
    )
  }

  private remember(key: string, entries: DiagnosticEntry[]): void {
    this.known.delete(key)
    this.known.set(key, entries)
    while (this.known.size > MAX_TRACKED_FILES) {
      const oldest = this.known.keys().next()
      if (oldest.done === true) break
      this.known.delete(oldest.value)
    }
  }

  private schedule(): void {
    if (this.timer !== undefined || this.disposed) return
    const timer = setTimeout(() => {
      this.timer = undefined
      this.flush()
    }, Math.max(0, this.options.flushMs))
    // Never keep the process alive just to flush diagnostics.
    if (typeof timer === 'object' && timer !== null && typeof timer.unref === 'function') timer.unref()
    this.timer = timer
  }
}
