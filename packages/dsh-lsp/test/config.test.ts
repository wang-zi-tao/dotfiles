import test from 'node:test'
import { deepEqual, equal, throws } from 'node:assert/strict'

import { resolveConfig } from '../src/config.js'

test('resolveConfig uses built-in servers when no override', () => {
  const config = resolveConfig({})
  equal(config.servers.length, 6)
  equal(config.lazyStart, true)
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

test('resolveConfig can disable a built-in server', () => {
  const config = resolveConfig({
    servers: [{ id: 'lua-language-server', enabled: false }],
  })
  equal(config.servers.find(s => s.id === 'lua-language-server'), undefined)
  equal(config.servers.find(s => s.id === 'clangd') !== undefined, true)
})

function ok(value: unknown, message?: string): asserts value {
  if (!value) throw new Error(message ?? 'expected truthy')
}
