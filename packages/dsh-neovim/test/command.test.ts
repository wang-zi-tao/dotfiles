/**
 * /neovim command tests — drive the registered handler through a minimal fake
 * Cordis context.
 *
 * The fake row config points the socket at a nonexistent Windows pipe, so the
 * lazy connect probe fails fast and the suite never touches a real Neovim.
 */

import test from 'node:test'
import assert from 'node:assert/strict'
import { apply } from '../src/index.js'
import type { Context } from '../src/types.js'

/** Unreachable RPC address; probe returns false immediately on Windows. */
const SOCKET = '\\\\.\\pipe\\dsh-neovim-test-nonexistent'
const OTHER = '\\\\.\\pipe\\dsh-neovim-test-other'
const SPACED = '\\\\.\\pipe\\dsh-neovim test spaced'

interface FakeResult {
  kind: string
  text: string
}

interface FakeCommand {
  description: string
  input?: { hint?: string }
  handler(invocation: { rawInput: string }): Promise<FakeResult>
}

interface Setup {
  command: FakeCommand
  logs: string[]
}

function setup(rawConfig: Record<string, unknown> = { socket: SOCKET }): Setup {
  let registered: unknown
  const logs: string[] = []
  const logger = {
    debug: (...args: unknown[]) => logs.push(`debug ${args.join(' ')}`),
    info: (...args: unknown[]) => logs.push(`info ${args.join(' ')}`),
    warn: (...args: unknown[]) => logs.push(`warn ${args.join(' ')}`),
    error: (...args: unknown[]) => logs.push(`error ${args.join(' ')}`),
  }
  const ctx = {
    logger: () => logger,
    effect: (fn: () => unknown) => {
      fn()
      return () => {}
    },
    tools: { register: () => {} },
    commands: {
      register: (definition: unknown) => {
        registered = definition
      },
    },
    on: () => {},
    agents: { list: () => [], roots: () => [] },
  }

  apply(ctx as unknown as Context, rawConfig)
  assert.ok(registered, '/neovim command should be registered')
  return { command: registered as FakeCommand, logs }
}

const run = (command: FakeCommand, rawInput: string): Promise<FakeResult> =>
  command.handler({ rawInput })

test('/neovim advertises the connect subcommand', () => {
  const { command } = setup()
  assert.match(command.description, /connect/)
  assert.match(command.input?.hint ?? '', /connect \[path\]/)
})

test('/neovim status reports the resolved socket in auto mode', async () => {
  const { command } = setup()
  const result = await run(command, 'status')
  assert.equal(result.kind, 'success')
  assert.match(result.text, /mode: auto \(config \/ env\)/)
  assert.ok(result.text.includes(SOCKET), result.text)
  assert.match(result.text, /connected: false/)
})

test('/neovim connect keeps the previous address when the path is dead', async () => {
  const { command } = setup()
  const failed = await run(command, `connect ${OTHER}`)
  assert.equal(failed.kind, 'error')
  assert.match(failed.text, /no Neovim RPC listener/)
  assert.ok(failed.text.includes(OTHER), failed.text)

  const status = await run(command, 'status')
  assert.match(status.text, /mode: auto \(config \/ env\)/)
  assert.ok(status.text.includes(SOCKET), status.text)
})

test('/neovim connect accepts a path containing spaces', async () => {
  const { command } = setup()
  const failed = await run(command, `connect ${SPACED}`)
  assert.equal(failed.kind, 'error')
  assert.ok(failed.text.includes(SPACED), failed.text)
})

test('/neovim connect without a path falls back to config resolution', async () => {
  const { command } = setup()
  const result = await run(command, 'connect')
  assert.equal(result.kind, 'error')
  assert.match(result.text, /failed: nvim not connected/)
  assert.ok(result.text.includes(SOCKET), result.text)
})

test('/neovim reconnect reports the connect failure', async () => {
  const { command } = setup()
  const result = await run(command, 'reconnect')
  assert.equal(result.kind, 'error')
  assert.match(result.text, /nvim not connected/)
})

test('/neovim rejects unknown subcommands and stray arguments', async () => {
  const { command } = setup()
  const unknown = await run(command, 'bogus')
  assert.equal(unknown.kind, 'error')
  assert.match(unknown.text, /unknown \/neovim subcommand 'bogus'/)
  assert.match(unknown.text, /connect \[path\]/)

  const statusArgs = await run(command, 'status extra')
  assert.equal(statusArgs.kind, 'error')
  assert.match(statusArgs.text, /takes no arguments/)

  const reconnectArgs = await run(command, 'reconnect extra')
  assert.equal(reconnectArgs.kind, 'error')
  assert.match(reconnectArgs.text, /takes no arguments/)
})

test('/neovim status defaults to the status report when no input is given', async () => {
  const { command } = setup()
  const result = await run(command, '')
  assert.equal(result.kind, 'success')
  assert.match(result.text, /Neovim bridge:/)
})
