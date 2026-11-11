import test from 'node:test'
import { equal, match, ok } from 'node:assert/strict'

import { abortError, isLspDeadlineError, LspDeadlineError, withDeadline } from '../src/deadline.js'

/** Await a rejection and hand back what it rejected with. */
async function failure(promise: Promise<unknown>): Promise<unknown> {
  try {
    await promise
  } catch (error) {
    return error
  }
  throw new Error('expected the promise to reject')
}

/** Let pending microtasks/timers land so an unhandled rejection would surface. */
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

test('a task that settles in time resolves with its value', async () => {
  const value = await withDeadline(async () => 'answer', { timeoutMs: 1000, label: 'fast' })
  equal(value, 'answer')
})

test('a task that never settles is rejected at the deadline', async () => {
  const started = Date.now()
  const error = await failure(withDeadline(
    () => new Promise<string>(() => {}),
    { timeoutMs: 60, label: 'textDocument/diagnostic (fake)' },
  ))
  const elapsed = Date.now() - started
  ok(isLspDeadlineError(error))
  equal(error.label, 'textDocument/diagnostic (fake)')
  equal(error.timeoutMs, 60)
  match(error.message, /textDocument\/diagnostic \(fake\) timed out after 60ms/)
  ok(elapsed >= 50, 'waited for the deadline: ' + elapsed)
  ok(elapsed < 2000, 'did not overshoot the deadline: ' + elapsed)
})

test('the callback signal aborts when the deadline expires, so the request can be cancelled', async () => {
  let seen: AbortSignal | undefined
  const error = await failure(withDeadline(
    signal => {
      seen = signal
      return new Promise<string>(() => {})
    },
    { timeoutMs: 40, label: 'cancel' },
  ))
  ok(isLspDeadlineError(error))
  equal(seen?.aborted, true)
})

test('a caller aborts the wait immediately, without waiting for the deadline', async () => {
  const controller = new AbortController()
  const started = Date.now()
  setTimeout(() => controller.abort(), 20)
  const error = await failure(withDeadline(
    () => new Promise<string>(() => {}),
    { timeoutMs: 5000, label: 'caller', signal: controller.signal },
  ))
  equal((error as Error).name, 'AbortError')
  equal((error as Error).message, abortError().message)
  ok(Date.now() - started < 2000, 'aborted well before the 5s deadline')
})

test('an already-aborted signal never runs the task', async () => {
  const controller = new AbortController()
  controller.abort()
  let ran = false
  const error = await failure(withDeadline(
    async () => {
      ran = true
      return 'nope'
    },
    { timeoutMs: 5000, label: 'pre-aborted', signal: controller.signal },
  ))
  equal(ran, false)
  equal((error as Error).name, 'AbortError')
})

test('a rejection arriving after the deadline is swallowed, not reported as unhandled', async () => {
  const unhandled = await unhandledWhile(async () => {
    const error = await failure(withDeadline(
      () => new Promise<string>((_resolve, reject) => {
        setTimeout(() => reject(new Error('late answer from a slow server')), 30)
      }),
      { timeoutMs: 10, label: 'late' },
    ))
    ok(isLspDeadlineError(error))
  })
  equal(unhandled.length, 0)
})

