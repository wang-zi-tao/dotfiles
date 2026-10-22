import test from 'node:test'
import { doesNotThrow, equal, match, ok, rejects } from 'node:assert/strict'
import { resolve } from 'node:path'

import { LspClient } from '../src/client.js'
import { isLspDeadlineError } from '../src/deadline.js'
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

/** Await a rejection and hand back what it rejected with. */
async function failure(promise: Promise<unknown>): Promise<unknown> {
  try {
    await promise
  } catch (error) {
    return error
  }
  throw new Error('expected the promise to reject')
}

/**
 * The shape of a server that accepts the request and never answers it: a
 * connection whose `sendRequest` returns a promise nobody ever settles.
 */
interface PullInternals {
  connection: { sendNotification: () => Promise<void>; sendRequest: () => Promise<unknown> } | null
  initialized: boolean
  state: string
}

test('a pull diagnostics request that is never answered is rejected at its deadline', async () => {
  // 80ms request deadline, passed through the constructor (config.timeoutMs).
  const client = new LspClient(spec, subprocess, logger, {}, undefined, 80)
  const internals = client as unknown as PullInternals
  internals.connection = {
    sendNotification: async () => {},
    sendRequest: () => new Promise<unknown>(() => {}),
  }
  internals.initialized = true
  internals.state = 'running'

  const started = Date.now()
  const error = await failure(client.diagnostics(document))
  const elapsed = Date.now() - started
  equal(isLspDeadlineError(error), true)
  equal((error as { label?: string }).label, 'textDocument/diagnostic (fake)')
  equal((error as { timeoutMs?: number }).timeoutMs, 80)
  match((error as Error).message, /timed out after 80ms/)
  equal(elapsed < 5000, true)
  // A deadline ends the request, never the server: the client stays usable.
  equal(client.state, 'running')
})

/**
 * The cancellation seam: what `token()` hands to `sendRequest`. These tests pin
 * the hot-reload crash — cancelling on a closed connection must neither write to
 * it (vscode-jsonrpc's `$/cancelRequest` throws `Connection is closed`) nor let a
 * listener's throw escape the abort dispatch, where it becomes an
 * `uncaughtException` and kills the host process.
 */
interface CancelInternals {
  connection: { sendNotification: () => Promise<void> } | null
  initialized: boolean
  state: string
  token(signal?: AbortSignal): { onCancellationRequested(listener: (error?: unknown) => void): { dispose(): void } }
}

function cancelInternals(client: LspClient): CancelInternals {
  return client as unknown as CancelInternals
}

test('cancelling a request on a closed connection never notifies it', () => {
  const client = new LspClient(spec, subprocess, logger)
  const internals = cancelInternals(client)
  internals.connection = null
  internals.state = 'stopped'
  const controller = new AbortController()
  let notified = 0
  internals.token(controller.signal).onCancellationRequested(() => {
    notified += 1
  })
  doesNotThrow(() => controller.abort())
  equal(notified, 0)
})

test('a cancellation listener that throws does not escape the abort dispatch', () => {
  const { client } = runningClient(() => Promise.resolve())
  const internals = cancelInternals(client)
  const controller = new AbortController()
  internals.token(controller.signal).onCancellationRequested(() => {
    throw new Error('Connection is closed.')
  })
  doesNotThrow(() => controller.abort())
})

test('cancellation still reaches a live connection', () => {
  const { client } = runningClient(() => Promise.resolve())
  const internals = cancelInternals(client)
  const controller = new AbortController()
  let seen: unknown
  internals.token(controller.signal).onCancellationRequested((error) => {
    seen = error
  })
  controller.abort()
  match(String((seen as Error | undefined)?.message), /aborted/)
})

test('a request outstanding across stop() cannot crash the process when its deadline fires', async () => {
  // Reproduces the hot-reload crash: teardown disposes the connection, and a
  // request that is still outstanding has its deadline fire afterwards. The
  // cancellation listener (vscode-jsonrpc's $/cancelRequest write) throws
  // "Connection is closed." if it is invoked on the dead connection - and a
  // throw from a signal listener is reported by Node as an uncaughtException,
  // which node:test would fail the test on.
  let disposed = false
  const client = new LspClient(spec, subprocess, logger, {}, undefined, 1500)
  const internals = client as unknown as {
    connection: unknown
    initialized: boolean
    state: string
  }
  internals.connection = {
    // Never answers, and mimics the library's cancellation write semantics.
    sendRequest: (_type: unknown, _params: unknown, token?: { onCancellationRequested?: (fn: () => void) => unknown }) =>
      new Promise<unknown>(() => {
        token?.onCancellationRequested?.(() => {
          if (disposed) throw new Error('Connection is closed.')
        })
      }),
    sendNotification: async () => {},
    dispose: () => {
      disposed = true
    },
  }
  internals.initialized = true
  internals.state = 'running'

  const inFlight = client.diagnostics(document)
  inFlight.catch(() => {})
  // stop() takes its bounded shutdown budget (~1s) before disposing, so the
  // 1.5s request deadline fires strictly after the connection is gone.
  await client.stop()
  const error = await failure(inFlight)
  equal(isLspDeadlineError(error), true)
  equal(disposed, true)
})

test('stop() terminates even when the server never answers shutdown', async () => {
  const client = new LspClient(spec, subprocess, logger)
  const internals = client as unknown as {
    connection: { sendRequest: () => Promise<unknown>; sendNotification: () => Promise<void>; dispose: () => void } | null
    initialized: boolean
    state: string
  }
  internals.connection = {
    sendRequest: () => new Promise<unknown>(() => {}),
    sendNotification: async () => {},
    dispose: () => {},
  }
  internals.initialized = true
  internals.state = 'running'
  const started = Date.now()
  await client.stop()
  const elapsed = Date.now() - started
  equal(client.state, 'stopped')
  ok(elapsed < 3000, 'stop() took ' + elapsed + 'ms')
})
