import test from 'node:test'
import { equal, ok, rejects } from 'node:assert/strict'

import { resolveConfig } from '../src/config.js'
import { LspDeadlineError } from '../src/deadline.js'
import type { LspClient } from '../src/client.js'
import type { PublishRecord } from '../src/inbox.js'
import { ServerRegistry } from '../src/registry.js'
import type { LoggerLike, ServerSpec, SubprocessHandle, SubprocessRuntime, SubprocessSpawnSpec } from '../src/types.js'

const logger = {
  debug: () => {},
  info: () => {},
  warn: () => {},
  error: () => {},
} as unknown as LoggerLike

const config = resolveConfig({
  servers: [{ id: 'clangd', command: 'clangd', args: [], extensions: ['cpp'], languageId: 'cpp', rootMarkers: ['compile_commands.json'] }],
})

/**
 * A subprocess seam whose handles expose no streams, so every start fails at
 * the "stdio did not expose piped streams" check. That failure is enough to
 * observe how many times the registry actually spawned.
 */
function countingSubprocess(counter: { spawns: number }): SubprocessRuntime {
  return {
    spawn(_spec: SubprocessSpawnSpec): SubprocessHandle {
      counter.spawns += 1
      return {
        pid: 4242,
        stdin: undefined,
        stdout: undefined,
        stderr: undefined,
        done: Promise.resolve({ exitCode: 1, signal: null }),
        terminate: () => {},
        waitForExit: async () => true,
      }
    },
  }
}

test('concurrent starts of one server share a single subprocess', async () => {
  const counter = { spawns: 0 }
  const registry = new ServerRegistry(config, countingSubprocess(counter), logger)
  const first = registry.startById('clangd', process.cwd())
  const second = registry.startById('clangd', process.cwd())
  await rejects(() => first)
  await rejects(() => second)
  equal(counter.spawns, 1)
  equal(registry.status().find(s => s.id === 'clangd')?.state, 'stopped')
})

test('a failed start is dropped so the next attempt spawns again', async () => {
  const counter = { spawns: 0 }
  const registry = new ServerRegistry(config, countingSubprocess(counter), logger)
  await rejects(() => registry.startById('clangd', process.cwd()))
  await rejects(() => registry.startById('clangd', process.cwd()))
  equal(counter.spawns, 2)
})

test('an unknown server id is rejected', async () => {
  const registry = new ServerRegistry(config, countingSubprocess({ spawns: 0 }), logger)
  await rejects(() => registry.startById('nope', process.cwd()), /unknown server id/)
})

/** The client surface the diagnostics policy uses; nothing is spawned. */
type FakeClient = Pick<
  LspClient,
  'refreshDocument' | 'diagnostics' | 'cachedDiagnostics' | 'waitForDiagnostics' | 'supportsPullDiagnostics'
>

/**
 * A registry whose single server is served by `client`. Overriding `resolve`
 * keeps the policy under test (timeout → fallback → note) free of subprocesses.
 */
function registryWith(client: FakeClient): ServerRegistry {
  const registry = new ServerRegistry(config, countingSubprocess({ spawns: 0 }), logger)
  const spec = config.servers[0] as ServerSpec
  ;(registry as unknown as { resolve: ServerRegistry['resolve'] }).resolve =
    async () => ({ spec, client: client as LspClient, root: process.cwd() })
  return registry
}

/** One push record as the inbox stores it. */
function pushRecord(at: number): PublishRecord {
  return {
    uri: 'file:///tmp/dsh-lsp-dead-server.cpp',
    path: '/tmp/dsh-lsp-dead-server.cpp',
    version: 1,
    at,
    diagnostics: [
      {
        range: { start: { line: 0, character: 0 }, end: { line: 0, character: 1 } },
        severity: 1,
        source: 'clang',
        message: "unknown type name 'foo'",
      },
    ],
  }
}

test('a pull diagnostics timeout falls back to the last push and explains itself', async () => {
  const registry = registryWith({
    refreshDocument: () => {},
    diagnostics: async () => {
      throw new LspDeadlineError('textDocument/diagnostic (clangd)', 1234)
    },
    cachedDiagnostics: () => pushRecord(Date.now() - 5000),
    waitForDiagnostics: async () => null,
    supportsPullDiagnostics: () => true,
  })
  const report = await registry.diagnosticsFor('a.cpp', process.cwd())
  equal(report.entries.length, 1)
  equal(report.entries[0]?.severity, 'error')
  ok(report.note?.includes('timed out after 1234ms'), report.note)
  ok(report.note?.includes('clangd'), report.note)
  ok(report.note?.includes('last pushed set'), report.note)
})

test('a pull diagnostics timeout without a cached push still returns, with a note', async () => {
  const registry = registryWith({
    refreshDocument: () => {},
    diagnostics: async () => {
      throw new LspDeadlineError('textDocument/diagnostic (clangd)', 900)
    },
    cachedDiagnostics: () => undefined,
    waitForDiagnostics: async () => null,
    supportsPullDiagnostics: () => true,
  })
  const report = await registry.diagnosticsFor('a.cpp', process.cwd())
  equal(report.entries.length, 0)
  ok(report.note?.includes('no pushed diagnostics are cached'), report.note)
})

test('a pull failure that is not a deadline still propagates', async () => {
  const registry = registryWith({
    refreshDocument: () => {},
    diagnostics: async () => {
      throw new Error('server exploded')
    },
    cachedDiagnostics: () => undefined,
    waitForDiagnostics: async () => null,
    supportsPullDiagnostics: () => true,
  })
  await rejects(() => registry.diagnosticsFor('a.cpp', process.cwd()), /server exploded/)
})

test('a push wait that expires without a push reports the empty result honestly', async () => {
  const registry = registryWith({
    refreshDocument: () => {},
    diagnostics: async () => null,
    cachedDiagnostics: () => undefined,
    waitForDiagnostics: async () => null,
    supportsPullDiagnostics: () => false,
  })
  const report = await registry.diagnosticsFor('a.cpp', process.cwd())
  equal(report.entries.length, 0)
  ok(report.note?.includes('no diagnostics push from clangd'), report.note)
  ok(report.note?.includes('15000ms'), report.note)
})

test('a push that arrives answers without a note', async () => {
  const pushed = pushRecord(Date.now())
  const registry = registryWith({
    refreshDocument: () => {},
    diagnostics: async () => null,
    cachedDiagnostics: () => undefined,
    waitForDiagnostics: async () => [...pushed.diagnostics],
    supportsPullDiagnostics: () => false,
  })
  const report = await registry.diagnosticsFor('a.cpp', process.cwd())
  equal(report.entries.length, 1)
  equal(report.note, undefined)
})
