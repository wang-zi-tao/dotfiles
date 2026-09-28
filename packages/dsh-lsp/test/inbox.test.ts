import test from 'node:test'
import { deepEqual, equal, ok } from 'node:assert/strict'

import { DiagnosticInbox, normalizePathKey } from '../src/inbox.js'
import { pathToFileUri } from '../src/protocol.js'
import type { Diagnostic } from 'vscode-languageserver-protocol'

const FILE = 'C:\\proj\\src\\a.cpp'
const URI = pathToFileUri(FILE)

function diag(message: string): Diagnostic {
  return {
    range: { start: { line: 0, character: 0 }, end: { line: 0, character: 1 } },
    message,
    severity: 1,
  }
}

test('normalizePathKey folds case so Windows paths match', () => {
  equal(normalizePathKey('C:\\Proj\\A.CPP'), normalizePathKey('c:\\proj\\a.cpp'))
})

test('a waiter resolves with the pushed diagnostics', async () => {
  const inbox = new DiagnosticInbox()
  const pending = inbox.wait(FILE, { timeoutMs: 1000 })
  inbox.put(URI, { uri: URI, version: 2, diagnostics: [diag('boom')] })
  const result = await pending
  ok(result)
  deepEqual(result.map(d => d.message), ['boom'])
})

test('a push older than the version this client sent is dropped', async () => {
  const inbox = new DiagnosticInbox()
  const pending = inbox.wait(FILE, { timeoutMs: 40 })
  // The client last sent version 3, so a version-2 report describes replaced text.
  inbox.put(URI, { uri: URI, version: 2, diagnostics: [diag('stale')] }, 3)
  equal(await pending, null)
  equal(inbox.cached(FILE), undefined)
})

test('a push without a version is accepted (nothing to compare)', async () => {
  const inbox = new DiagnosticInbox()
  const pending = inbox.wait(FILE, { timeoutMs: 500 })
  inbox.put(URI, { uri: URI, diagnostics: [diag('unversioned')] }, 7)
  const result = await pending
  ok(result)
  deepEqual(result.map(d => d.message), ['unversioned'])
})

test('a waiter times out with null (the wait ends, nothing else)', async () => {
  const inbox = new DiagnosticInbox()
  equal(await inbox.wait(FILE, { timeoutMs: 20 }), null)
})

test('aborting a waiter settles it with null', async () => {
  const inbox = new DiagnosticInbox()
  const controller = new AbortController()
  const pending = inbox.wait(FILE, { timeoutMs: 5000, signal: controller.signal })
  controller.abort()
  equal(await pending, null)
})

test('an already-aborted signal never registers a waiter', async () => {
  const inbox = new DiagnosticInbox()
  const controller = new AbortController()
  controller.abort()
  equal(await inbox.wait(FILE, { timeoutMs: 5000, signal: controller.signal }), null)
  // A later push must not resurrect the settled wait.
  inbox.put(URI, { uri: URI, diagnostics: [diag('late')] })
  equal(inbox.cached(FILE)?.diagnostics.length, 1)
})

test('the cache keeps only the most recent push per document', () => {
  const inbox = new DiagnosticInbox()
  inbox.put(URI, { uri: URI, version: 1, diagnostics: [diag('one')] })
  inbox.put(URI, { uri: URI, version: 2, diagnostics: [diag('two')] })
  equal(inbox.cached(FILE)?.diagnostics[0]?.message, 'two')
  equal(inbox.cached(FILE)?.version, 2)
})

test('non-file URIs are ignored', () => {
  const inbox = new DiagnosticInbox()
  inbox.put('untitled:Untitled-1', { uri: 'untitled:Untitled-1', diagnostics: [diag('x')] })
  equal(inbox.cached('untitled:Untitled-1'), undefined)
})

test('subscribers see pushes until they dispose', () => {
  const inbox = new DiagnosticInbox()
  let seen = 0
  const subscription = inbox.onPublish(() => { seen += 1 })
  inbox.put(URI, { uri: URI, diagnostics: [] })
  subscription.dispose()
  inbox.put(URI, { uri: URI, diagnostics: [] })
  equal(seen, 1)
})

test('a throwing subscriber does not break ingest', () => {
  const inbox = new DiagnosticInbox()
  inbox.onPublish(() => { throw new Error('boom') })
  inbox.put(URI, { uri: URI, diagnostics: [diag('kept')] })
  equal(inbox.cached(FILE)?.diagnostics.length, 1)
})

test('clear settles pending waiters and drops cached pushes', async () => {
  const inbox = new DiagnosticInbox()
  inbox.put(URI, { uri: URI, diagnostics: [diag('one')] })
  const pending = inbox.wait(FILE, { timeoutMs: 5000 })
  inbox.clear()
  equal(await pending, null)
  equal(inbox.cached(FILE), undefined)
})
