/**
 * Background-index progress (`$/progress`) and its model-facing notice.
 *
 * A language server that builds an index in the background reports it over
 * `$/progress` with a server-chosen token. clangd uses
 * `backgroundIndexProgress`: `begin` when the first shard is enqueued,
 * `report` while the queue drains (percentage plus a "completed/enqueued"
 * message), and `end` once the queue is idle.
 *
 * This matters because `workspace/symbol` never waits for the index: an empty
 * answer during those windows is expected rather than authoritative. Tracking
 * the stream lets the plugin (a) explain an empty symbol answer instead of
 * reporting a bare "No symbols.", and (b) tell the model — via
 * `Agent.inject` — the moment the index is usable and the query is worth
 * retrying.
 *
 * Everything here is pure so it can be unit-tested without a server.
 */

import type { UserMessage } from '@deepseek-ai/dsh-session'

import type { IndexProgress } from './types.js'

/** The structural shape of a `$/progress` notification's params. */
export interface ProgressParamsLike {
  token?: unknown
  value?: unknown
}

/** Counters as clangd writes them: `formatv("{0}/{1}", completed, enqueued)`. */
const COUNTERS = /(\d+)\s*\/\s*(\d+)/

let indexMessageSeq = 0

/** The token of a `$/progress` params object, or null when it carries none. */
export function progressToken(params: ProgressParamsLike | null | undefined): string | null {
  const token = params?.token
  if (token === undefined || token === null) return null
  const text = String(token)
  return text === '' ? null : text
}

/** Clamp a reported percentage into 0–100; non-numbers are ignored. */
function toPercentage(value: unknown): number | null {
  if (typeof value !== 'number' || !Number.isFinite(value)) return null
  return Math.max(0, Math.min(100, Math.round(value)))
}

/** Parse `"210/500"` into its counters. */
function parseCounters(message: unknown): { completed: number; enqueued: number } | null {
  if (typeof message !== 'string') return null
  const match = COUNTERS.exec(message)
  if (!match) return null
  const completed = Number(match[1])
  const enqueued = Number(match[2])
  if (!Number.isFinite(completed) || !Number.isFinite(enqueued)) return null
  return { completed, enqueued }
}

/**
 * Fold one `$/progress` notification into the tracked state for its token.
 *
 * `begin` starts a fresh round (counters reset: the server may rebuild its
 * index), `report` carries the running counters, and `end` marks the queue as
 * drained. Unknown kinds and token-less notifications return null, so a
 * foreign progress stream never creates phantom state.
 */
export function foldIndexProgress(
  previous: IndexProgress | undefined,
  params: ProgressParamsLike | null | undefined,
  now: number = Date.now(),
): IndexProgress | null {
  const token = progressToken(params)
  if (token === null) return null
  const value = params?.value
  if (!value || typeof value !== 'object') return null
  const record = value as { kind?: unknown; title?: unknown; percentage?: unknown; message?: unknown }
  const kind = typeof record.kind === 'string' ? record.kind : ''
  const base: IndexProgress =
    previous && previous.token === token
      ? previous
      : { token, title: '', percentage: null, completed: null, enqueued: null, done: false, updatedAt: now }

  switch (kind) {
    case 'begin':
      return {
        ...base,
        title: typeof record.title === 'string' ? record.title : base.title,
        percentage: null,
        completed: null,
        enqueued: null,
        done: false,
        updatedAt: now,
      }
    case 'report': {
      const counters = parseCounters(record.message)
      const reported = toPercentage(record.percentage)
      const derived =
        counters && counters.enqueued > 0 ? Math.min(100, Math.round((100 * counters.completed) / counters.enqueued)) : null
      return {
        ...base,
        percentage: reported ?? derived ?? base.percentage,
        completed: counters ? counters.completed : base.completed,
        enqueued: counters ? counters.enqueued : base.enqueued,
        done: false,
        updatedAt: now,
      }
    }
    case 'end': {
      // `end` carries no counters. The queue draining means every enqueued unit
      // is finished, so the enqueued total becomes the completed total; with no
      // totals at all the stream is still reported as done.
      const enqueued = base.enqueued
      return {
        ...base,
        percentage: 100,
        completed: enqueued ?? base.completed,
        enqueued,
        done: true,
        updatedAt: now,
      }
    }
    default:
      return null
  }
}

/** One-line rendering for `/lsp status` and tool notes. */
export function renderIndexProgress(progress: IndexProgress): string {
  const title = progress.title || 'indexing'
  const counters =
    progress.completed !== null && progress.enqueued !== null
      ? ' (' + progress.completed + '/' + progress.enqueued + ')'
      : progress.enqueued !== null
        ? ' (' + progress.enqueued + ')'
        : ''
  if (progress.done) return title + ' done' + counters
  const percent = progress.percentage === null ? '' : ' ' + progress.percentage + '%'
  return title + percent + counters
}

/**
 * Build the user-role message that tells the model a background index is
 * ready, injected through `Agent.inject` at the next pre-step boundary.
 *
 * The completion moment is the only signal that a previously empty
 * `workspaceSymbol` became answerable, so the notice names the retry.
 */
export function indexProgressMessage(serverId: string, root: string, progress: IndexProgress): UserMessage {
  const units = progress.enqueued ?? progress.completed
  const summary = 'LSP 后台索引就绪: ' + serverId + ' (' + root + ')'
  const text = [
    '<dsh-lsp-index>',
    '后台索引已完成 (' + serverId + ', root=' + root + (units !== null ? ', ' + units + ' 个编译单元' : '') + ').',
    'workspaceSymbol 现在可以命中从未打开过的文件; 此前返回 "No symbols." 的查询可以重试。',
    '</dsh-lsp-index>',
  ].join('\n')
  return {
    id: ('dsh-lsp-index-' + Date.now() + '-' + ++indexMessageSeq) as UserMessage['id'],
    role: 'user',
    content: [{ type: 'text', text }],
    source: { kind: 'dsh-lsp', form: 'notice', summary },
  }
}
