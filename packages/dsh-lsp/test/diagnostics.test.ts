import test from 'node:test'
import { deepEqual, equal } from 'node:assert/strict'

import { diagnosticsMessage, filterDiagnostics, formatDiagnostics, severityRank } from '../src/diagnostics.js'
import type { DiagnosticEntry } from '../src/types.js'

function entry(severity: DiagnosticEntry['severity'], message: string): DiagnosticEntry {
  return { severity, message, range: { startLine: 1, startCharacter: 1, endLine: 1, endCharacter: 2 } }
}

test('severityRank orders severities', () => {
  equal(severityRank('hint'), 1)
  equal(severityRank('information'), 2)
  equal(severityRank('warning'), 3)
  equal(severityRank('error'), 4)
})

test('filterDiagnostics keeps at-or-above min severity', () => {
  const entries = [entry('error', 'e'), entry('warning', 'w'), entry('hint', 'h')]
  deepEqual(filterDiagnostics(entries, 'warning').map(d => d.message), ['e', 'w'])
  deepEqual(filterDiagnostics(entries, 'error').map(d => d.message), ['e'])
  deepEqual(filterDiagnostics(entries, 'hint').map(d => d.message), ['e', 'w', 'h'])
})

test('filterDiagnostics drops everything below the min severity', () => {
  const entries = [entry('warning', 'w'), entry('information', 'i')]
  equal(filterDiagnostics(entries, 'error').length, 0)
})

test('formatDiagnostics renders severity and location', () => {
  equal(formatDiagnostics([entry('error', 'boom')]), '[error] 1:1 boom')
})

test('diagnosticsMessage builds an injectable user message', () => {
  const entries = [entry('error', 'boom'), entry('warning', 'careful')]
  const msg = diagnosticsMessage('/src/a.ts', entries)
  equal(msg.role, 'user')
  equal(msg.source.kind, 'plugin')
  equal((msg.source as { plugin?: string }).plugin, 'dsh-lsp')
  equal((msg.source as { form?: string }).form, 'notice')
  equal((msg.source as { summary?: string }).summary, 'LSP 诊断: 1 error(s), 1 warning(s) - /src/a.ts')
  equal(msg.content.length, 1)
  const text = (msg.content[0] as { type: 'text'; text: string }).text
  equal(text.includes('[error] 1:1 boom'), true)
  equal(text.includes('[warning] 1:1 careful'), true)
  equal(text.includes('<dsh-lsp-diagnostics>'), true)
})