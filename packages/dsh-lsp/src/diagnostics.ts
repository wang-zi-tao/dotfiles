/**
 * Diagnostics formatting, diffing and agent-injection helpers.
 *
 * Pure functions only: everything here is unit-testable without touching the
 * LSP client or the plugin wiring. The injected message follows the same
 * structural contract dsh-hindsight uses for recall injection (a user-role
 * message whose source is a `dsh-lsp` notice), consumed by {@link Agent.inject}
 * at the agent's next pre-step boundary.
 */

import type { UserMessage } from '@deepseek-ai/dsh-session'
import type { DiagnosticEntry, DiagnosticSeverity } from './types.js'

/** Numeric rank for severity filtering (higher = more severe). */
export function severityRank(severity: DiagnosticSeverity): number {
  switch (severity) {
    case 'error': return 4
    case 'warning': return 3
    case 'information': return 2
    case 'hint': return 1
  }
}

/** Keep only diagnostics at or above the minimum severity. */
export function filterDiagnostics(
  entries: readonly DiagnosticEntry[],
  minSeverity: DiagnosticSeverity,
): DiagnosticEntry[] {
  const min = severityRank(minSeverity)
  return entries.filter(d => severityRank(d.severity) >= min)
}

/** Format a diagnostic list as compact text lines (no wrapper). */
export function formatDiagnostics(entries: readonly DiagnosticEntry[]): string {
  return entries.map(d => {
    const loc = d.range.startLine + ':' + d.range.startCharacter
    return '[' + d.severity + '] ' + (d.source ? d.source + ': ' : '') + loc + ' ' + d.message
  }).join('\n')
}

/**
 * Stable identity of one diagnostic, used to diff two publishes of the same
 * file. The position key includes the end of the range so an edit that widens
 * a squiggle is a *new* finding rather than the same one.
 */
export function diagnosticKey(entry: DiagnosticEntry): string {
  const range = entry.range
  return [
    entry.severity,
    range.startLine + ':' + range.startCharacter,
    range.endLine + ':' + range.endCharacter,
    entry.source ?? '',
    entry.code ?? '',
    entry.message,
  ].join('\u0000')
}

/** What one file's diagnostics gained and lost between two publishes. */
export interface DiagnosticsDiff {
  added: DiagnosticEntry[]
  removed: DiagnosticEntry[]
}

/**
 * Multiset diff of two publishes of one file: identical findings are matched by
 * count, so a file that reports the same error twice keeps both, and a file
 * that re-publishes an unchanged list yields an empty diff.
 */
export function diffDiagnostics(
  previous: readonly DiagnosticEntry[],
  next: readonly DiagnosticEntry[],
): DiagnosticsDiff {
  const buckets = new Map<string, DiagnosticEntry[]>()
  for (const entry of previous) {
    const key = diagnosticKey(entry)
    const bucket = buckets.get(key)
    if (bucket) bucket.push(entry)
    else buckets.set(key, [entry])
  }
  const added: DiagnosticEntry[] = []
  for (const entry of next) {
    const bucket = buckets.get(diagnosticKey(entry))
    if (bucket !== undefined && bucket.length > 0) bucket.pop()
    else added.push(entry)
  }
  const removed: DiagnosticEntry[] = []
  for (const bucket of buckets.values()) removed.push(...bucket)
  return { added, removed }
}

/** One file's pending added / removed diagnostics, for the injected message. */
export interface DiagnosticsFileChange {
  path: string
  added: DiagnosticEntry[]
  removed: DiagnosticEntry[]
}

let diagnosticsMessageSeq = 0

/**
 * Build a user-role message describing how a language server's diagnostics
 * changed for one or more files. Only the delta is reported: `+` lines are
 * findings that appeared, `-` lines are findings that went away.
 */
export function diagnosticsChangeMessage(
  serverId: string,
  changes: readonly DiagnosticsFileChange[],
  omitted = 0,
): UserMessage {
  let added = 0
  let removed = 0
  for (const change of changes) {
    added += change.added.length
    removed += change.removed.length
  }
  const summary = 'LSP 诊断: +' + added + ' / -' + removed + ' (' + changes.length + ' 个文件) - ' + serverId
  const lines: string[] = ['<dsh-lsp-diagnostics>']
  lines.push('LSP 诊断变更 (' + serverId + '): ' + changes.length + ' 个文件, 新增 ' + added + ', 消失 ' + removed)
  for (const change of changes) {
    lines.push(change.path)
    for (const entry of change.added) lines.push('  + ' + formatDiagnostics([entry]))
    for (const entry of change.removed) lines.push('  - ' + formatDiagnostics([entry]))
  }
  if (omitted > 0) lines.push('… ' + omitted + ' more diagnostic(s) omitted (maxDiagnostics)')
  lines.push('</dsh-lsp-diagnostics>')
  return {
    id: 'dsh-lsp-' + Date.now() + '-' + (++diagnosticsMessageSeq) as UserMessage['id'],
    role: 'user',
    content: [{ type: 'text', text: lines.join('\n') }],
    source: { kind: 'dsh-lsp', form: 'notice', summary },
  }
}
