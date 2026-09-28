import test from 'node:test'
import { equal, rejects } from 'node:assert/strict'

import { resolveConfig } from '../src/config.js'
import { ServerRegistry } from '../src/registry.js'
import type { LoggerLike, SubprocessHandle, SubprocessRuntime, SubprocessSpawnSpec } from '../src/types.js'

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
