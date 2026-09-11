/**
 * Diagnostics formatting and agent-injection helpers for the write hook.
 *
 * Pure functions only: everything here is unit-testable without touching the
 * LSP client or the plugin wiring. The injected message follows the same
 * structural contract dsh-hindsight uses for recall injection (a user-role
 * message whose source is a plugin notice), consumed by {@link Agent.inject}
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

let diagnosticsMessageSeq = 0

/**
 * Build a user-role message carrying post-write LSP diagnostics for
 * {@link Agent.inject}. The message is a plain lossless-JSON object (text
 * content block + plugin source), exactly the shape the harness's inbox
 * projection accepts for injected context.
 */
export function diagnosticsMessage(filePath: string, entries: readonly DiagnosticEntry[]): UserMessage {
  const counts: Record<DiagnosticSeverity, number> = { error: 0, warning: 0, information: 0, hint: 0 }
  for (const d of entries) counts[d.severity] += 1
  const summary = 'LSP 诊断: ' + counts.error + ' error(s), ' + counts.warning + ' warning(s) - ' + filePath
  const text = [
    '<dsh-lsp-diagnostics>',
    '写入后 LSP 诊断 (' + filePath + '):',
    formatDiagnostics(entries),
    '</dsh-lsp-diagnostics>',
  ].join('\n')
  return {
    id: 'dsh-lsp-' + Date.now() + '-' + (++diagnosticsMessageSeq) as UserMessage['id'],
    role: 'user',
    content: [{ type: 'text', text }],
    source: { kind: 'plugin', plugin: 'dsh-lsp', form: 'notice', summary },
  }
}
