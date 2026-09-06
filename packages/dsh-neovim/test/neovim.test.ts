/**
 * dsh-neovim connection tests.
 *
 * Port of the OpenCode `neovim-api` index.test.ts to node:test. The suite is
 * self-contained: it spawns its own `nvim --embed --clean` child, attaches over
 * its stdio (msgpack-rpc), and tears it down afterwards — no external Neovim
 * instance or listen address is required. If `nvim` is not on PATH the whole
 * suite skips.
 *
 * The debugger cases below only exercise the plain RPC/Lua surface; the
 * `luaAsyncEval` bridge needs the user's `core.agent` Lua module, which the
 * embed child does not load, so `before` installs a minimal `run_async` stub
 * (synchronous pcall + `async_task_finish` rpcnotify) under that name.
 */

import test, { type TestContext } from 'node:test'
import assert from 'node:assert/strict'
import { spawn, type ChildProcess } from 'node:child_process'
import { attach } from 'neovim'
import { Neovim } from '../src/neovim.js'

/** Shared embed instance owned by the suite (single connection for all cases). */
let proc: ChildProcess | undefined
let nvim: Neovim | undefined
let spawnFailure: string | undefined

/**
 * Minimal `core.agent.run_async` so `luaAsyncEval` works in a clean embed.
 * Single line: the `:lua` Ex command cannot span lines.
 */
const CORE_AGENT_STUB =
  'package.loaded["core.agent"] = { run_async = function(fn, channel_id, task_id) ' +
  'local ok, res = pcall(fn) ' +
  'vim.rpcnotify(channel_id, "async_task_finish", { succ = ok, ret = res, error = tostring(res), task_id = task_id }) end }'

function withTimeout<T>(promise: Promise<T>, ms: number, what: string): Promise<T> {
  let timer: NodeJS.Timeout | undefined
  return Promise.race([
    promise,
    new Promise<never>((_, reject) => {
      timer = setTimeout(() => reject(new Error(`${what} timed out after ${ms}ms`)), ms)
    }),
  ]).finally(() => {
    if (timer !== undefined) clearTimeout(timer)
  }) as Promise<T>
}

test.before(async () => {
  let child: ChildProcess
  try {
    child = spawn('nvim', ['--embed', '--clean'], {
      stdio: ['pipe', 'pipe', 'pipe'],
      windowsHide: true,
    })
  } catch (error) {
    spawnFailure = `failed to spawn nvim: ${String(error)}`
    return
  }
  proc = child

  try {
    // A missing executable surfaces as an asynchronous 'error' event (ENOENT),
    // not a synchronous throw; race it against 'spawn' so a dead child cannot
    // leave `channelId()` pending forever.
    await new Promise<void>((resolve, reject) => {
      child.once('error', reject)
      child.once('spawn', () => resolve())
    })
    const client = attach({ proc: child })
    const nv = new Neovim(client)
    await withTimeout(nv.channelId(), 10_000, 'channelId')
    await nv.command(`lua ${CORE_AGENT_STUB}`)
    nvim = nv
  } catch (error) {
    spawnFailure = `nvim --embed setup failed: ${String(error)}`
    child.kill()
  }
})

test.after(async () => {
  if (nvim) {
    await withTimeout(nvim.close(), 2_000, 'close').catch(() => {})
  }
  proc?.kill()
})

function requireNvim(t: TestContext): Neovim | null {
  if (nvim) return nvim
  t.skip(spawnFailure ?? 'embedded nvim unavailable')
  return null
}

test('connect neovim', (t) => {
  const nv = requireNvim(t)
  if (!nv) return
  assert.ok(!nv.disconnected)
})

test('run neovim command eval', async (t) => {
  const nv = requireNvim(t)
  if (!nv) return
  const result = await nv.eval('1+1')
  assert.equal(result, 2)
})

test('run neovim command sync', async (t) => {
  const nv = requireNvim(t)
  if (!nv) return
  const result = await nv.command('echo 1+1')
  assert.equal(result, '2')
})

test('run neovim command lua', async (t) => {
  const nv = requireNvim(t)
  if (!nv) return
  const result = await nv.luaEval('1+1')
  assert.equal(result, 2)
})

test('run neovim command lua args', async (t) => {
  const nv = requireNvim(t)
  if (!nv) return
  const result = await nv.luaEval('a*b', { a: 2, b: 3 })
  assert.equal(result, 6)
})

test('run neovim command lua complex args round-trip', async (t) => {
  const nv = requireNvim(t)
  if (!nv) return
  const args = {
    s: 'he said "hi" \\\n\ttab',
    n: 1.5,
    bool: true,
    arr: [1, 'two', false],
    obj: { a: 1, 'k with space': 'v', nested: { x: [true] } },
  }
  const command =
    '{s=s, n=n, bool=bool, arr=arr, obj=obj}'
  const result = await nv.luaEval(command, args)
  assert.deepEqual(result, args)
})

test('jsonToLuaCode encodes null and undefined as nil', (t) => {
  const nv = requireNvim(t)
  if (!nv) return
  assert.equal(nv.jsonToLuaCode(null), 'nil')
  assert.equal(nv.jsonToLuaCode(undefined), 'nil')
})

test('run neovim command lua in sync', async (t) => {
  const nv = requireNvim(t)
  if (!nv) return
  const result = await nv.luaEval('coroutine.running()==nil')
  assert.equal(result, true)
})

test('run neovim command async', async (t) => {
  const nv = requireNvim(t)
  if (!nv) return
  const result = await nv.command("lua coroutine.wrap(function() print('12') end)()")
  assert.equal(result, '12')
})

test('run neovim command lua async', async (t) => {
  const nv = requireNvim(t)
  if (!nv) return
  const result = await nv.luaAsyncEval('a*b', 'core.agent', { a: 2, b: 3 })
  assert.equal(result, 6)
})

test('run neovim command lua error', async (t) => {
  const nv = requireNvim(t)
  if (!nv) return
  await assert.rejects(
    () => nv.luaAsyncEval("error('error message')", 'core.agent'),
    (error: unknown) => error !== null && error !== undefined,
  )
})