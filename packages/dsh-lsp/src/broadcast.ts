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
 *   - a file is only news to a session whose tools actually read or wrote it
 *     (`scope`, see touched.ts): re-analysing files nobody opened cannot page
 *     a turn, and a file no session has touched keeps no baseline, so the first
 *     report after someone opens it arrives whole;
 *   - among the sessions that qualify, the message goes to every live agent
 *     rooted inside the server's root (`targetsFor`);
 *   - injection uses `Agent.inject`, which queues the message for the agent's
 *     next pre-step and never wakes the driver.
 */

import type { UserMessage } from '@deepseek-ai/dsh-session'

import {
  diffDiagnostics,
  diagnosticsChangeMessage,
  filterDiagnostics,
} from './diagnostics.js'
import { normalizePathKey, type PublishRecord } from './inbox.js'
import { isPathInside, toDiagnosticEntries } from './protocol.js'
import type { TouchedFileScope } from './touched.js'
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
  /**
   * Which sessions may hear about a file. When set, a file is only diffed and
   * injected for agents whose tools read or wrote it, and a file no target has
   * touched keeps no baseline (so its first report arrives whole). Omitted
   * keeps the pre-filter behaviour: every file is tracked and every target
   * hears about it.
   */
  scope?: TouchedFileScope
}

/** One file whose latest reported diagnostics have not been flushed yet. */
interface PendingFile {
  serverId: string
  root: string
  pathKey: string
  path: string
  entries: DiagnosticEntry[]
}

/** One file's pending delta, tagged with the key the touched-file scope answers for. */
interface PendingChange {
  /** `normalizePathKey(path)` — the key `scope.includes` is asked about. */
  pathKey: string
  path: string
  added: DiagnosticEntry[]
  removed: DiagnosticEntry[]
}

/** Changes for one server+root, injected as one message per recipient. */
interface ChangeGroup {
  serverId: string
  root: string
  /** Agents rooted here when the group was built (may be empty). */
  targets: readonly InjectableAgent[]
  changes: PendingChange[]
}

/** Bounded so a long session cannot grow the baseline table without limit. */
const MAX_TRACKED_FILES = 2048

/**
 * Trim the changes to `maxDiagnostics` listed entries. Additions win the
 * budget: a finding that appeared is what the model has to fix, a finding that
 * disappeared is only news.
 */
function capChanges(
  changes: readonly PendingChange[],
  maxDiagnostics: number,
): { changes: PendingChange[]; omitted: number } {
  const budget = Math.max(1, Math.floor(maxDiagnostics))
  let used = 0
  let omitted = 0
  const capped: PendingChange[] = []
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
    if (added.length > 0 || removed.length > 0) {
      capped.push({ pathKey: change.pathKey, path: change.path, added, removed })
    }
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
      if (diff.added.length === 0 && diff.removed.length === 0) {
        // Unchanged: nothing to report, so only refresh a baseline that exists.
        if (file.entries.length > 0) this.remember(key, file.entries)
        continue
      }

      const groupKey = file.serverId + '\u0000' + file.root.toLowerCase()
      let group = groups.get(groupKey)
      if (group === undefined) {
        const targets = this.resolveTargets(file.serverId, file.root)
        if (targets === undefined) continue
        group = { serverId: file.serverId, root: file.root, targets, changes: [] }
        groups.set(groupKey, group)
      }

      if (!group.targets.some(agent => this.reaches(agent.id, file.pathKey))) {
        // No session this message could reach has the file in hand. Recording a
        // baseline here would swallow the file's whole report once a session
        // that did touch it is finally told about it, so only the scope-less
        // (pre-filter) mode keeps tracking such a file.
        if (this.options.scope === undefined) this.remember(key, file.entries)
        continue
      }
      this.remember(key, file.entries)
      group.changes.push({ pathKey: file.pathKey, path: file.path, added: diff.added, removed: diff.removed })
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
    if (group.targets.length === 0) {
      this.options.logger.debug(
        'dsh-lsp: no agent session rooted at ' + group.root + ' for ' + group.serverId + ' diagnostics',
      )
      return
    }
    if (group.changes.length === 0) return

    // Each recipient is told about the subset of these files its own tools
    // touched, so two sessions in one root never read each other's files.
    let delivered = 0
    let added = 0
    let removed = 0
    const files = new Set<string>()
    for (const agent of group.targets) {
      const visible = group.changes.filter(change => this.reaches(agent.id, change.pathKey))
      if (visible.length === 0) continue
      const { changes, omitted } = capChanges(visible, this.options.maxDiagnostics)
      if (changes.length === 0) continue
      try {
        agent.inject(diagnosticsChangeMessage(group.serverId, changes, omitted))
      } catch (error) {
        const reason = error instanceof Error ? error.message : String(error)
        this.options.logger.debug('dsh-lsp: diagnostics injection into agent ' + agent.id + ' failed: ' + reason)
        continue
      }
      delivered += 1
      for (const change of changes) {
        files.add(change.path)
        added += change.added.length
        removed += change.removed.length
      }
    }
    if (delivered === 0) return
    this.options.logger.info(
      'dsh-lsp: injected +' + added + ' / -' + removed + ' diagnostic(s) for ' + files.size +
      ' file(s) from ' + group.serverId + ' into ' + delivered + ' agent(s)',
    )
  }

  private resolveTargets(serverId: string, root: string): readonly InjectableAgent[] | undefined {
    try {
      return this.options.targetsFor(serverId, root)
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error)
      this.options.logger.debug('dsh-lsp: diagnostics target resolution failed: ' + message)
      return undefined
    }
  }

  /** Whether `agentId` may hear about this file (no scope: everyone may). */
  private reaches(agentId: string, pathKey: string): boolean {
    const scope = this.options.scope
    return scope === undefined || scope.includes(agentId, pathKey)
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
