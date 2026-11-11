/**
 * Per-session record of the files a tool has read or written.
 *
 * The broadcaster's baseline is process-wide, but a model should only be told
 * about files its own session actually looked at: re-analysing one header makes
 * a language server re-publish dozens of translation units, and injecting all
 * of them into a session that never opened any of them is pure noise. This
 * table answers the one question the broadcast needs — "did a tool in this
 * session read or write that file?" — keyed the same way the broadcaster keys
 * its baseline (`normalizePathKey`, so Windows casing never splits a path).
 *
 * Paths are resolved to absolute before they are recorded: a tool's `file_path`
 * may be workspace-relative while a server publishes absolute URIs.
 */

import { normalizePathKey } from './inbox.js'

/** Bounded so one long-lived session cannot grow this table without limit. */
export const MAX_TOUCHED_FILES_PER_AGENT = 4096

/** Bounded so a long process spawning many short sessions stays flat. */
export const MAX_TOUCHED_AGENTS = 64

/** The predicate the diagnostics broadcast asks before it injects a file. */
export interface TouchedFileScope {
  /** Whether `agentId`'s tools have read or written the file behind `pathKey`. */
  includes(agentId: string, pathKey: string): boolean
}

export class TouchedFiles implements TouchedFileScope {
  /** agent id -> pathKey -> absolute path, in least-recently-touched-first order. */
  private readonly byAgent = new Map<string, Map<string, string>>()

  /** Record that `agentId` read or wrote `path` (absolute, or resolved by the caller). */
  touch(agentId: string, path: string): void {
    if (agentId === '') return
    const key = normalizePathKey(path)
    let files = this.byAgent.get(agentId)
    if (files === undefined) {
      files = new Map()
      this.byAgent.set(agentId, files)
      while (this.byAgent.size > MAX_TOUCHED_AGENTS) {
        const oldest = this.byAgent.keys().next()
        if (oldest.done === true) break
        this.byAgent.delete(oldest.value)
      }
    }
    // Re-insert so iteration order stays a recency order (LRU eviction below).
    files.delete(key)
    files.set(key, path)
    while (files.size > MAX_TOUCHED_FILES_PER_AGENT) {
      const oldest = files.keys().next()
      if (oldest.done === true) break
      files.delete(oldest.value)
    }
  }

  includes(agentId: string, pathKey: string): boolean {
    return this.byAgent.get(agentId)?.has(pathKey) === true
  }

  /** Absolute path recorded for that pairing, or `undefined` when untouched. */
  pathOf(agentId: string, pathKey: string): string | undefined {
    return this.byAgent.get(agentId)?.get(pathKey)
  }

  /** Number of sessions tracked (diagnostics and tests). */
  get agentCount(): number {
    return this.byAgent.size
  }

  /** Drop every recording (plugin unload / HMR). */
  dispose(): void {
    this.byAgent.clear()
  }
}
