/**
 * Pre-write byte snapshots.
 *
 * The harness tool result carries `before`/`after` text that is already
 * LF-normalized and BOM-stripped, so the two facts the repair needs — the
 * original per-line terminators and the original BOM state — exist only in the
 * bytes on disk *before* the write lands. `tools/pre-execute` is the only hook
 * that runs early enough, so it records them here and `tools/post-execute`
 * consumes them.
 *
 * @module dsh-encoding/snapshot
 */

export interface Snapshot {
  /** Raw bytes, or `null` when the target did not exist (a create). */
  readonly bytes: Uint8Array | null
  /** Insertion timestamp, for TTL eviction. */
  readonly at: number
}

/**
 * A bounded, TTL'd, path-keyed store. Reads are destructive: a snapshot exists
 * for exactly one write. Entries are also dropped by insertion order once the
 * configured bound is exceeded, so a tool call that fails after its pre-execute
 * cannot leak memory.
 */
export class SnapshotStore {
  private readonly entries = new Map<string, Snapshot>()

  constructor(
    private readonly maxEntries: number,
    private readonly ttlMs: number,
  ) {}

  get size(): number {
    return this.entries.size
  }

  put(key: string, bytes: Uint8Array | null, now: number = Date.now()): void {
    this.entries.delete(key)
    this.entries.set(key, { bytes, at: now })
    this.evict(now)
  }

  /** Remove and return the snapshot for `key`, or `undefined` when none is usable. */
  take(key: string, now: number = Date.now()): Snapshot | undefined {
    const entry = this.entries.get(key)
    if (entry === undefined) return undefined
    this.entries.delete(key)
    if (this.ttlMs > 0 && now - entry.at > this.ttlMs) return undefined
    return entry
  }

  /** Drop the oldest entry; call repeatedly until the bound holds. */
  private evict(now: number): void {
    if (this.ttlMs > 0) {
      for (const [key, entry] of this.entries) {
        if (now - entry.at > this.ttlMs) this.entries.delete(key)
      }
    }
    while (this.entries.size > this.maxEntries) {
      const oldest = this.entries.keys().next()
      if (oldest.done === true) break
      this.entries.delete(oldest.value)
    }
  }
}
