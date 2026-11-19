import test from 'node:test'
import { equal, match, ok } from 'node:assert/strict'
import { PassThrough, Writable } from 'node:stream'

import { StreamMessageWriter } from 'vscode-languageserver-protocol/node'

import { guardMessageWriter } from '../src/writer.js'

/** A stdin whose far end is gone: every write fails with ERR_STREAM_DESTROYED. */
function destroyedStdin(): Writable {
  const stream = new Writable({
    write(_chunk, _encoding, done) {
      done()
    },
  })
  stream.destroy()
  return stream
}

/** A stdin that accepts writes normally. */
function openStdin(): Writable {
  return new Writable({
    write(_chunk, _encoding, done) {
      done()
    },
  })
}

const request = { jsonrpc: '2.0' as const, id: 1, method: 'textDocument/hover', params: {} }

/** Let rejected promises settle so an unhandled rejection would be observed. */
async function settle(): Promise<void> {
  for (let i = 0; i < 5; i += 1) {
    await new Promise<void>(done => setTimeout(() => done(), 5))
  }
}

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

test('a failed write resolves and reports the error instead of rejecting', async () => {
  const failures: unknown[] = []
  const writer = guardMessageWriter(new StreamMessageWriter(destroyedStdin()), error => failures.push(error))

  const unhandled = await unhandledWhile(async () => {
    await writer.write(request)
  })

  equal(unhandled.length, 0)
  equal(failures.length, 1)
  match(String((failures[0] as { code?: unknown } | null)?.code ?? (failures[0] as Error).message), /ERR_STREAM_DESTROYED/)
})

test('a successful write still resolves through the wrapper', async () => {
  const failures: unknown[] = []
  const sink = new PassThrough()
  const writer = guardMessageWriter(new StreamMessageWriter(sink), error => failures.push(error))

  const unhandled = await unhandledWhile(async () => {
    await writer.write(request)
  })

  equal(unhandled.length, 0)
  equal(failures.length, 0)
})

test('a throwing failure handler is logged instead of escaping', async () => {
  const logged: string[] = []
  const writer = guardMessageWriter(
    new StreamMessageWriter(destroyedStdin()),
    () => {
      throw new Error('owner blew up')
    },
    (message, error) => logged.push(`${message}: ${(error as Error).message}`),
  )

  const unhandled = await unhandledWhile(async () => {
    await writer.write(request)
  })

  equal(unhandled.length, 0)
  equal(logged.length, 1)
  match(logged[0] ?? '', /failure handler threw: owner blew up/)
})

test('the wrapper keeps forwarding the wrapped writer\'s error and close events', () => {
  const base = new StreamMessageWriter(openStdin())
  const writer = guardMessageWriter(base, () => {})
  ok(typeof writer.onError === 'function')
  ok(typeof writer.onClose === 'function')
})
