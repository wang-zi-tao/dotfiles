import test from 'node:test'
import { deepEqual, equal, throws } from 'node:assert/strict'

import { resolveConfig } from '../src/config.js'

test('resolveConfig uses built-in servers when no override', () => {
  const config = resolveConfig({})
  equal(config.servers.length, 6)
  equal(config.autoStart, 'session')
  deepEqual(config.autoStartServers, [])
  deepEqual(config.autoStartRoots, [])
  equal(config.diagnosticsMode, 'auto')
  equal(config.diagnosticsTimeoutMs, 15000)
  equal(config.diagnosticsDedupeMs, 60000)
  equal(config.maxDiagnostics, 50)
  equal(config.diagnosticsOnAnyPublish, false)
  const clangd = config.servers.find(s => s.id === 'clangd')!
  ok(clangd)
  equal(clangd.languageId, 'cpp')
  equal(clangd.extensions.includes('cpp'), true)
  const ra = config.servers.find(s => s.id === 'rust-analyzer')!
  equal(ra.languageId, 'rust')
})

test('resolveConfig rejects duplicate extension across servers', () => {
  throws(() => resolveConfig({
    servers: [
      { id: 'a', command: 'a', extensions: ['ts', 'js'], languageId: 'x', rootMarkers: [] },
      { id: 'b', command: 'b', extensions: ['js'], languageId: 'y', rootMarkers: [] },
    ],
  }), /claimed by both/)
})

test('resolveConfig rejects a server without command', () => {
  throws(() => resolveConfig({
    servers: [{ id: 'a', extensions: ['ts'], languageId: 'x', rootMarkers: [] }],
  }), /no command/)
})

test('resolveConfig merges a row override over a built-in server', () => {
  const config = resolveConfig({
    servers: [{ id: 'clangd', command: 'clangd', args: ['--foo'], extensions: ['c', 'h'], languageId: 'cpp', rootMarkers: ['compile_commands.json'] }],
  })
  const clangd = config.servers.find(s => s.id === 'clangd')!
  deepEqual(clangd.args, ['--foo'])
  deepEqual(clangd.extensions, ['c', 'h'])
  // other built-ins still present
  equal(config.servers.find(s => s.id === 'rust-analyzer') !== undefined, true)
})

test('resolveConfig normalizes extensions (lowercase, strip dot)', () => {
  const config = resolveConfig({
    servers: [{ id: 'x', command: 'x', extensions: ['.FOO', 'Bar'], languageId: 'x', rootMarkers: [] }],
  })
  const x = config.servers.find(s => s.id === 'x')!
  deepEqual(x.extensions, ['foo', 'bar'])
})

test('resolveConfig defaults for file-access integration', () => {
  const config = resolveConfig({})
  equal(config.syncLoadOnRead, true)
  equal(config.diagnosticsOnWrite, true)
  equal(config.diagnosticsMinSeverity, 'warning')
})

test('resolveConfig parses file-access integration options', () => {
  const config = resolveConfig({
    syncLoadOnRead: false,
    diagnosticsOnWrite: 'no',
    diagnosticsMinSeverity: 'error',
  })
  equal(config.syncLoadOnRead, false)
  equal(config.diagnosticsOnWrite, false)
  equal(config.diagnosticsMinSeverity, 'error')
})

test('resolveConfig normalizes severity aliases', () => {
  equal(resolveConfig({ diagnosticsMinSeverity: 'warn' }).diagnosticsMinSeverity, 'warning')
  equal(resolveConfig({ diagnosticsMinSeverity: 'info' }).diagnosticsMinSeverity, 'information')
  equal(resolveConfig({ diagnosticsMinSeverity: 'WARNING' }).diagnosticsMinSeverity, 'warning')
  equal(resolveConfig({ diagnosticsMinSeverity: 'bogus' }).diagnosticsMinSeverity, 'warning')
})

test('resolveConfig can disable a built-in server', () => {
  const config = resolveConfig({
    servers: [{ id: 'lua-language-server', enabled: false }],
  })
  equal(config.servers.find(s => s.id === 'lua-language-server'), undefined)
  equal(config.servers.find(s => s.id === 'clangd') !== undefined, true)
})

test('resolveConfig rejects an unknown autoStart mode', () => {
  throws(() => resolveConfig({ autoStart: 'always' }), /invalid autoStart/)
})

test('resolveConfig rejects an unknown diagnostics mode', () => {
  throws(() => resolveConfig({ diagnosticsMode: 'events' }), /invalid diagnosticsMode/)
})

test('resolveConfig accepts the explicit auto-start and diagnostics pins', () => {
  const config = resolveConfig({
    autoStart: 'off',
    autoStartServers: 'clangd, rust-analyzer',
    autoStartRoots: ['D:\\repo'],
    diagnosticsMode: 'push',
    diagnosticsTimeoutMs: 500,
    maxDiagnostics: 5,
    diagnosticsOnAnyPublish: 'yes',
  })
  equal(config.autoStart, 'off')
  deepEqual(config.autoStartServers, ['clangd', 'rust-analyzer'])
  deepEqual(config.autoStartRoots, ['D:\\repo'])
  equal(config.diagnosticsMode, 'push')
  equal(config.diagnosticsTimeoutMs, 500)
  equal(config.maxDiagnostics, 5)
  equal(config.diagnosticsOnAnyPublish, true)
})

function ok(value: unknown, message?: string): asserts value {
  if (!value) throw new Error(message ?? 'expected truthy')
}
