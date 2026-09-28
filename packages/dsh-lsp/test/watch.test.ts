import test from 'node:test'
import { deepEqual, equal, ok } from 'node:assert/strict'

import type { PublishRecord } from '../src/inbox.js'
import type { LoggerLike } from '../src/types.js'
import { DiagnosticWatcher, type InjectableAgent } from '../src/watch.js'

const FILE = 'C:\\proj\\a.cpp'

interface FakeAgent {
  agent: InjectableAgent
  messages: unknown[]
}

function makeAgent(id = 'agent-1'): FakeAgent {
  const messages: unknown[] = []
  const agent: InjectableAgent = {
    id,
    inject: (message) => { messages.push(message) },
  }
  return { agent, messages }
}

function makeLogger(): { logger: LoggerLike; debugLines: string[]; infoLines: string[] } {
  const debugLines: string[] = []
  const infoLines: string[] = []
  const logger = {
    debug: (message: string) => { debugLines.push(message) },
    info: (message: string) => { infoLines.push(message) },
    warn: () => {},
    error: () => {},
  } as unknown as LoggerLike
  return { logger, debugLines, infoLines }
}

function makeWatcher(overrides: Partial<{
  minSeverity: 'hint' | 'information' | 'warning' | 'error'
  timeoutMs: number
  dedupeMs: number
  maxDiagnostics: number
  injectUnwatched: boolean
}> = {}) {
  const { logger, debugLines, infoLines } = makeLogger()
  const watcher = new DiagnosticWatcher({
    minSeverity: 'warning',
    timeoutMs: 5000,
    dedupeMs: 60000,
    maxDiagnostics: 50,
    injectUnwatched: false,
    logger,
    ...overrides,
  })
  return { watcher, debugLines, infoLines }
}

function record(messages: Array<{ message: string; severity?: 1 | 2 | 3 | 4 }>, path = FILE): PublishRecord {
  return {
    uri: 'file:///C:/proj/a.cpp',
    path,
    version: 2,
    at: Date.now(),
    diagnostics: messages.map(m => ({
      range: { start: { line: 0, character: 0 }, end: { line: 0, character: 1 } },
      message: m.message,
      severity: m.severity ?? 1,
    })),
  }
}

function textOf(message: unknown): string {
  const blocks = (message as { content?: Array<{ text?: string }> }).content ?? []
  return blocks.map(b => b.text ?? '').join('\n')
}

test('a matching push injects once and clears the watch', () => {
  const { watcher, infoLines } = makeWatcher()
  const { agent, messages } = makeAgent()
  watcher.arm('agent-1', FILE, agent)
  watcher.accept(record([{ message: 'boom' }]))
  equal(messages.length, 1)
  // The watch is gone, so the next push has nothing to answer (unwatched off).
  watcher.accept(record([{ message: 'boom' }]))
  equal(messages.length, 1)
  equal(infoLines.length, 1)
})

test('a clean report injects nothing and clears the watch', () => {
  const { watcher } = makeWatcher()
  const { agent, messages } = makeAgent()
  watcher.arm('agent-1', FILE, agent)
  watcher.accept(record([]))
  equal(messages.length, 0)
})

test('findings below the minimum severity are not injected', () => {
  const { watcher } = makeWatcher()
  const { agent, messages } = makeAgent()
  watcher.arm('agent-1', FILE, agent)
  watcher.accept(record([{ message: 'note', severity: 3 }]))
  equal(messages.length, 0)
})

test('a watch that times out is dropped and later pushes are ignored', async () => {
  const { watcher, debugLines } = makeWatcher({ timeoutMs: 10 })
  const { agent, messages } = makeAgent()
  watcher.arm('agent-1', FILE, agent)
  await new Promise<void>(resolve => setTimeout(resolve, 40))
  watcher.accept(record([{ message: 'late' }]))
  equal(messages.length, 0)
  equal(debugLines.some(line => /timed out/.test(line)), true)
})

test('abort drops the watch without injecting', () => {
  const { watcher } = makeWatcher()
  const { agent, messages } = makeAgent()
  watcher.arm('agent-1', FILE, agent)
  watcher.abort('agent-1')
  watcher.accept(record([{ message: 'boom' }]))
  equal(messages.length, 0)
})

test('abortAll drops every watch', () => {
  const { watcher, debugLines } = makeWatcher({ timeoutMs: 10 })
  const a = makeAgent('agent-a')
  const b = makeAgent('agent-b')
  watcher.arm('agent-a', FILE, a.agent)
  watcher.arm('agent-b', FILE, b.agent)
  watcher.abortAll()
  watcher.accept(record([{ message: 'boom' }]))
  equal(a.messages.length, 0)
  equal(b.messages.length, 0)
  // The per-watch timers were cancelled, so nothing logs a timeout afterwards.
  return new Promise<void>(resolve => setTimeout(() => {
    equal(debugLines.some(line => /timed out/.test(line)), false)
    resolve()
  }, 40))
})

test('an identical finding set inside the dedupe window is injected once', () => {
  const { watcher } = makeWatcher()
  const { agent, messages } = makeAgent()
  watcher.arm('agent-1', FILE, agent)
  watcher.accept(record([{ message: 'boom' }]))
  watcher.arm('agent-1', FILE, agent)
  watcher.accept(record([{ message: 'boom' }]))
  equal(messages.length, 1)
})

test('a different finding set is injected again', () => {
  const { watcher } = makeWatcher()
  const { agent, messages } = makeAgent()
  watcher.arm('agent-1', FILE, agent)
  watcher.accept(record([{ message: 'boom' }]))
  watcher.arm('agent-1', FILE, agent)
  watcher.accept(record([{ message: 'bang' }]))
  equal(messages.length, 2)
})

test('re-arming the same key supersedes the previous watch', () => {
  const { watcher, infoLines } = makeWatcher()
  const { agent, messages } = makeAgent()
  watcher.arm('agent-1', FILE, agent)
  watcher.arm('agent-1', FILE, agent)
  watcher.accept(record([{ message: 'boom' }]))
  equal(messages.length, 1)
  equal(infoLines.length, 1)
})

test('maxDiagnostics caps the injection and reports the remainder', () => {
  const { watcher } = makeWatcher({ maxDiagnostics: 2 })
  const { agent, messages } = makeAgent()
  watcher.arm('agent-1', FILE, agent)
  watcher.accept(record([{ message: 'first' }, { message: 'second' }, { message: 'third' }]))
  equal(messages.length, 1)
  const text = textOf(messages[0])
  ok(/first/.test(text))
  ok(/second/.test(text))
  equal(/third/.test(text), false)
  ok(/1 more diagnostic\(s\) omitted/.test(text))
})

test('pushes for another file never answer this watch', () => {
  const { watcher } = makeWatcher()
  const { agent, messages } = makeAgent()
  watcher.arm('agent-1', FILE, agent)
  watcher.accept(record([{ message: 'other' }], 'C:\\proj\\b.cpp'))
  equal(messages.length, 0)
})

test('an unwatched push is ignored by default', () => {
  const { watcher } = makeWatcher()
  const { agent, messages } = makeAgent()
  watcher.arm('agent-1', FILE, agent)
  watcher.accept(record([{ message: 'boom' }]))
  watcher.accept(record([{ message: 'later' }]))
  equal(messages.length, 1)
})

test('injectUnwatched delivers to the most recent writer', () => {
  const { watcher } = makeWatcher({ injectUnwatched: true })
  const { agent, messages } = makeAgent()
  watcher.arm('agent-1', FILE, agent)
  watcher.accept(record([{ message: 'boom' }]))
  watcher.accept(record([{ message: 'later' }]))
  equal(messages.length, 2)
})
