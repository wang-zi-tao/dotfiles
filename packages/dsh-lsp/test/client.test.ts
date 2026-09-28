import test from 'node:test'
import { equal, match, rejects } from 'node:assert/strict'
import { resolve } from 'node:path'

import { LspClient } from '../src/client.js'
import type { LoggerLike, ServerSpec, SubprocessRuntime } from '../src/types.js'

const logger = {
  debug: () => {},
  info: () => {},
  warn: () => {},
  error: () => {},
} as unknown as LoggerLike

const spec: ServerSpec = {
  id: 'fake',
  command: 'fake-ls',
  args: [],
  extensions: ['cpp'],
  languageId: 'cpp',
  rootMarkers: ['compile_commands.json'],
}

/** These tests exercise the write path only; nothing is ever spawned. */
const subprocess = {
  spawn: () => {
    throw new Error('spawn is not used by these tests')
  },
} as unknown as SubprocessRuntime

/** The exact failure a closed pipe produces: net.js Socket.writeAfterFIN. */
function brokenPipe(): Error {
  return Object.assign(new Error('This socket has been ended by the other party'), { code: 'EPIPE' })
}

/**
 * Reach into the client to install a fake connection and pretend the
 * initialize handshake succeeded, which is the only state where a notification
 * write happens.
 */
interface ClientInternals {
  connection: { sendNotification: () => Promise<void> } | null
  initialized: boolean
  state: string
  openDocuments: Set<string>
}

function runningClient(sendNotification: () => Promise<void>): {
  client: LspClient
  internals: ClientInternals
} {
  const client = new LspClient(spec, subprocess, logger)
  const internals = client as unknown as ClientInternals
  internals.connection = { sendNotification }
  internals.initialized = true
  internals.state = 'running'
  return { client, internals }
}

/** Let rejected promises settle so an unhandled rejection would be observed. */
async function settle(): Promise<void> {
  for (let i = 0; i < 5; i += 1) {
    await new Promise<void>(done => setTimeout(() => done(), 5))
  }
}

/** Collect unhandled rejections while `body` runs; Node would otherwise abort. */
async function unhandledWhile(body: () => Promise<void>): Promise<unknown[]> {
  const seen: unknown[] = []
  const listener = (reason: unknown): void => {
    seen.push(reason)
  }
  process.on('unhandledRejection', listener)
  try {
    await body()
    await settle()
  } finally {
    process.off('unhandledRejection', listener)
  }
  return seen
}

const document = resolve(process.cwd(), 'dsh-lsp-dead-server.cpp')

test('a didOpen write to a dead server is contained, not thrown at the process', async () => {
  const { client } = runningClient(() => Promise.reject(brokenPipe()))
  const unhandled = await unhandledWhile(async () => {
    client.open(document)
  })
  equal(unhandled.length, 0)
  equal(client.state, 'failed')
  match(client.error ?? '', /didOpen failed: This socket has been ended by the other party/)
  // The failed client refuses further work, which is what makes the registry
  // drop it and respawn on the next query.
  await rejects(() => client.definition(document, 1, 1), /is not running/)
})

test('a didChange write to a dead server is contained too', async () => {
  const { client, internals } = runningClient(() => Promise.reject(brokenPipe()))
  internals.openDocuments.add(document)
  const unhandled = await unhandledWhile(async () => {
    client.refreshDocument(document)
  })
  equal(unhandled.length, 0)
  equal(client.state, 'failed')
  match(client.error ?? '', /didChange failed: This socket has been ended by the other party/)
})

test('a write that throws synchronously (disposed connection) is contained', async () => {
  const { client } = runningClient(() => {
    throw new Error('Connection is disposed.')
  })
  const unhandled = await unhandledWhile(async () => {
    client.open(document)
  })
  equal(unhandled.length, 0)
  equal(client.state, 'failed')
  match(client.error ?? '', /Connection is disposed/)
})
