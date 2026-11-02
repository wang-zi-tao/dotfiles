import test from 'node:test'
import { deepEqual, equal } from 'node:assert/strict'

import {
  diagnosticKey,
  diffDiagnostics,
  diagnosticsChangeMessage,
  filterDiagnostics,
  formatDiagnostics,
  severityRank,
} from '../src/diagnostics.js'
import type { DiagnosticEntry } from '../src/types.js'

function entry(severity: DiagnosticEntry['severity'], message: string, line = 1): DiagnosticEntry {
  return { severity, message, range: { startLine: line, startCharacter: 1, endLine: line, endCharacter: 2 } }
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

test('diagnosticKey is stable for equal findings and varies with every field', () => {
  equal(diagnosticKey(entry('error', 'boom')), diagnosticKey(entry('error', 'boom')))
  equal(diagnosticKey(entry('error', 'boom')) === diagnosticKey(entry('warning', 'boom')), false)
  equal(diagnosticKey(entry('error', 'boom', 1)) === diagnosticKey(entry('error', 'boom', 2)), false)
  equal(diagnosticKey(entry('error', 'boom')) === diagnosticKey(entry('error', 'other')), false)
  equal(
    diagnosticKey(entry('error', 'boom')) === diagnosticKey({ ...entry('error', 'boom'), source: 'clang' }),
    false,
  )
  equal(diagnosticKey(entry('error', 'boom')) === diagnosticKey({ ...entry('error', 'boom'), code: 'E1' }), false)
})

test('diffDiagnostics reports additions and disappearances', () => {
  const diff = diffDiagnostics([entry('error', 'old')], [entry('warning', 'new')])
  deepEqual(diff.added.map(d => d.message), ['new'])
  deepEqual(diff.removed.map(d => d.message), ['old'])
})

test('diffDiagnostics is empty for an unchanged set', () => {
  const entries = [entry('error', 'boom'), entry('warning', 'careful')]
  const diff = diffDiagnostics(entries, entries)
  equal(diff.added.length, 0)
  equal(diff.removed.length, 0)
})

test('diffDiagnostics keeps duplicate findings as a multiset', () => {
  const diff = diffDiagnostics([entry('error', 'boom')], [entry('error', 'boom'), entry('error', 'boom')])
  deepEqual(diff.added.map(d => d.message), ['boom'])
  equal(diff.removed.length, 0)

  const shrunk = diffDiagnostics([entry('error', 'boom'), entry('error', 'boom')], [entry('error', 'boom')])
  equal(shrunk.added.length, 0)
  deepEqual(shrunk.removed.map(d => d.message), ['boom'])
})

test('diffDiagnostics treats a severity change as removal plus addition', () => {
  const diff = diffDiagnostics([entry('error', 'boom')], [entry('warning', 'boom')])
  deepEqual(diff.added.map(d => d.severity), ['warning'])
  deepEqual(diff.removed.map(d => d.severity), ['error'])
})

test('diagnosticsChangeMessage builds an injectable user message', () => {
  const message = diagnosticsChangeMessage('clangd', [
    { path: '/src/a.ts', added: [entry('error', 'boom')], removed: [entry('warning', 'careful')] },
  ])
  equal(message.role, 'user')
  equal(message.source.kind, 'dsh-lsp')
  equal((message.source as { form?: string }).form, 'notice')
  equal((message.source as { summary?: string }).summary, 'LSP 诊断: +1 / -1 (1 个文件) - clangd')
  equal(message.content.length, 1)
  const text = (message.content[0] as { type: 'text'; text: string }).text
  equal(text.includes('<dsh-lsp-diagnostics>'), true)
  equal(text.includes('</dsh-lsp-diagnostics>'), true)
  equal(text.includes('/src/a.ts'), true)
  equal(text.includes('  + [error] 1:1 boom'), true)
  equal(text.includes('  - [warning] 1:1 careful'), true)
})

test('diagnosticsChangeMessage notes omitted diagnostics', () => {
  const message = diagnosticsChangeMessage(
    'clangd',
    [{ path: '/src/a.ts', added: [entry('error', 'boom')], removed: [] }],
    3,
  )
  const text = (message.content[0] as { type: 'text'; text: string }).text
  equal(text.includes('… 3 more diagnostic(s) omitted (maxDiagnostics)'), true)
})
