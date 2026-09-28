/**
 * Write → diagnostics → inject watcher.
 *
 * A write cannot "ask" a push-only server for diagnostics, so the write hook
 * arms a watch *before* it refreshes the document: the server's push for the
 * refreshed text then finds the watch and the findings are injected into the
 * calling agent's next pre-step (\`Agent.inject\`, which does not wake the
 * driver).
 *
 * Semantics:
 *   - empty after severity filtering  → nothing is injected (the file is clean);
 *   - nothing arrives within the timeout → the watch is dropped and only a debug
 *     line is logged: the *wait* ends, the server and its documents are untouched;
 *   - identical findings within the dedupe window → injected once;
 *   - a newer write by the same agent supersedes the previous watch
 *     (\`abort\`), so stale findings never follow a newer edit.
 */

import type { UserMessage } from '@deepseek-ai/dsh-session'

import { diagnosticsMessage, filterDiagnostics, formatDiagnostics } from './diagnostics.js'
import { normalizePathKey, type PublishRecord } from './inbox.js'
import { toDiagnosticEntries } from './protocol.js'
import type { DiagnosticSeverity, LoggerLike } from './types.js'

/** The minimal agent surface the watcher needs (kept structural for testability). */
export interface InjectableAgent {
  id: string
  inject(message: UserMessage): void
}

export interface DiagnosticWatcherOptions {
  minSeverity: DiagnosticSeverity
  /** How long a watch waits for one push before giving up. */
  timeoutMs: number
  /** Window in which an identical finding set is injected only once. */
  dedupeMs: number
  /** Upper bound on injected diagnostics per push. */
  maxDiagnostics: number
  /**
   * When true, a push that arrives with no armed watch is still injected into
   * the agent that most recently wrote that file. Off by default: a background
   * re-publish (e.g. a header edit invalidating a translation unit) would
   * otherwise interrupt an unrelated turn.
   */
  injectUnwatched: boolean
  logger: LoggerLike
  /** Clock seam for tests. */
  now?: () => number
}

interface Watch {
  key: string
  pathKey: string
  path: string
  agent: InjectableAgent
  timer: ReturnType<typeof setTimeout> | undefined
}

export class DiagnosticWatcher {
  private readonly options: DiagnosticWatcherOptions
  private readonly now: () => number
  /** Armed watches, keyed by the caller's key (the writing agent's id). */
  private readonly watches = new Map<string, Watch>()
  /** Most recent writing agent per file, for \`injectUnwatched\`. */
  private readonly recentAgents = new Map<string, InjectableAgent>()
  /** Last injected finding set per file, for the dedupe window. */
  private readonly injected = new Map<string, { signature: string; at: number }>()

  constructor(options: DiagnosticWatcherOptions) {
    this.options = options
    this.now = options.now ?? (() => Date.now())
  }

  /** Arm (or re-arm) a watch for \`path\`; the newest watch for \`key\` wins. */
  arm(key: string, path: string, agent: InjectableAgent): void {
    this.abort(key)
    const pathKey = normalizePathKey(path)
    this.recentAgents.set(pathKey, agent)
    const watch: Watch = { key, pathKey, path, agent, timer: undefined }
    watch.timer = setTimeout(() => {
      if (this.watches.get(key) === watch) this.watches.delete(key)
      this.options.logger.debug('dsh-lsp: diagnostics watch timed out for ' + path)
    }, Math.max(1, this.options.timeoutMs))
    this.watches.set(key, watch)
  }

  /** Drop a watch without injecting (superseded write, turn cancelled, unload). */
  abort(key: string): void {
    const watch = this.watches.get(key)
    if (!watch) return
    if (watch.timer !== undefined) clearTimeout(watch.timer)
    this.watches.delete(key)
  }

  /** Drop every watch (plugin unload). */
  abortAll(): void {
    for (const key of [...this.watches.keys()]) this.abort(key)
  }

  /** Handle one accepted push from the inbox. */
  accept(record: PublishRecord): void {
    const pathKey = normalizePathKey(record.path)
    const targets = [...this.watches.values()].filter(watch => watch.pathKey === pathKey)
    if (targets.length === 0) {
      if (!this.options.injectUnwatched) return
      const agent = this.recentAgents.get(pathKey)
      if (!agent) return
      this.deliver(agent, record, undefined)
      return
    }
    for (const watch of targets) this.deliver(watch.agent, record, watch)
  }

  private deliver(agent: InjectableAgent, record: PublishRecord, watch: Watch | undefined): void {
    const relevant = filterDiagnostics(toDiagnosticEntries(record.diagnostics), this.options.minSeverity)
    if (relevant.length === 0) {
      if (watch) this.clear(watch)
      this.options.logger.debug('dsh-lsp: no reportable diagnostics for ' + record.path)
      return
    }

    const shown = relevant.slice(0, Math.max(1, this.options.maxDiagnostics))
    const omitted = relevant.length - shown.length
    const signature = formatDiagnostics(shown)
    const pathKey = normalizePathKey(record.path)
    const previous = this.injected.get(pathKey)
    const at = this.now()
    if (previous && previous.signature === signature && at - previous.at < this.options.dedupeMs) {
      if (watch) this.clear(watch)
      this.options.logger.debug('dsh-lsp: duplicate diagnostics suppressed for ' + record.path)
      return
    }

    agent.inject(diagnosticsMessage(record.path, shown, omitted))
    this.injected.set(pathKey, { signature, at })
    if (watch) this.clear(watch)
    this.options.logger.info(
      'dsh-lsp: injected ' + shown.length + (omitted > 0 ? '+' + omitted : '') +
      ' diagnostic(s) for ' + record.path + ' into agent ' + agent.id,
    )
  }

  private clear(watch: Watch): void {
    if (watch.timer !== undefined) clearTimeout(watch.timer)
    if (this.watches.get(watch.key) === watch) this.watches.delete(watch.key)
  }
}
