import test from 'node:test'
import { equal, ok } from 'node:assert/strict'

import { DiagnosticBroadcaster, type DiagnosticBroadcasterOptions, type InjectableAgent } from '../src/broadcast.js'
import type { DiagnosticEntry, DiagnosticSeverity, LoggerLike } from '../src/types.js'

const SERVER = 'clangd'
const ROOT = 'C:\\proj'
const PATH_A = 'C:\\proj\\src\\a.cpp'
const PATH_B = 'C:\\proj\\src\\b.cpp'

function entry(severity: DiagnosticSeverity, message: string, line = 1): DiagnosticEntry {
  return {
    severity,
    message,
    range: { startLine: line, startCharacter: 1, endLine: line, endCharacter: 2 },
  }
}

interface RecordingAgent extends InjectableAgent {
  messages: Array<{ text: string; summary: string }>
}

function makeAgent(id: string): RecordingAgent {
  const messages: RecordingAgent['messages'] = []
  return {
    id,
    messages,
    inject(message) {
      const block = message.content[0] as { type: 'text'; text: string }
      const source = message.source as { summary?: string }
      messages.push({ text: block.text, summary: source.summary ?? '' })
    },
  }
}

function makeLogger(): { logger: LoggerLike; lines: string[] } {
  const lines: string[] = []
  const logger = {
    debug: (message: string) => lines.push('debug ' + message),
    info: (message: string) => lines.push('info ' + message),
    warn: (message: string) => lines.push('warn ' + message),
  } as unknown as LoggerLike
  return { logger, lines }
}

interface Harness {
  broadcaster: DiagnosticBroadcaster
  agents: RecordingAgent[]
  lines: string[]
}

function makeHarness(overrides: Partial<DiagnosticBroadcasterOptions> = {}, targets?: RecordingAgent[]): Harness {
  const agents = targets ?? [makeAgent('agent-1')]
  const { logger, lines } = makeLogger()
  const broadcaster = new DiagnosticBroadcaster({
    minSeverity: 'warning',
    flushMs: 0,
    maxDiagnostics: 50,
    logger,
    targetsFor: () => agents,
    ...overrides,
  })
  return { broadcaster, agents, lines }
}

function texts(agent: RecordingAgent): string[] {
  return agent.messages.map(message => message.text)
}

test('injects a newly added diagnostic once', () => {
  const { broadcaster, agents } = makeHarness()
  broadcaster.observe(SERVER, ROOT, PATH_A, [entry('error', 'boom')])
  broadcaster.flush()

  equal(agents[0]!.messages.length, 1)
  const text = texts(agents[0]!)[0]!
  equal(text.includes('</dsh-lsp-diagnostics>'), true)
  equal(text.includes(PATH_A), true)
  equal(text.includes('  + [error] 1:1 boom'), true)
  equal(text.includes('  - '), false)
  equal(agents[0]!.messages[0]!.summary.includes('+1'), true)
})

test('an unchanged re-publish injects nothing', () => {
  const { broadcaster, agents } = makeHarness()
  const entries = [entry('error', 'boom')]
  broadcaster.observe(SERVER, ROOT, PATH_A, entries)
  broadcaster.flush()
  equal(agents[0]!.messages.length, 1)

  broadcaster.observe(SERVER, ROOT, PATH_A, entries)
  broadcaster.flush()
  equal(agents[0]!.messages.length, 1)
})

test('an empty re-publish reports the diagnostics that disappeared', () => {
  const { broadcaster, agents } = makeHarness()
  broadcaster.observe(SERVER, ROOT, PATH_A, [entry('error', 'boom'), entry('warning', 'careful')])
  broadcaster.flush()
  broadcaster.observe(SERVER, ROOT, PATH_A, [entry('error', 'boom')])
  broadcaster.flush()

  equal(agents[0]!.messages.length, 2)
  const text = texts(agents[0]!)[1]!
  equal(text.includes('  - [warning] 1:1 careful'), true)
  equal(text.includes('  + '), false)
  equal(agents[0]!.messages[1]!.summary.includes('-1'), true)
})

test('a severity change is one removal plus one addition', () => {
  const { broadcaster, agents } = makeHarness()
  broadcaster.observe(SERVER, ROOT, PATH_A, [entry('error', 'boom')])
  broadcaster.flush()
  broadcaster.observe(SERVER, ROOT, PATH_A, [entry('warning', 'boom')])
  broadcaster.flush()

  const text = texts(agents[0]!)[1]!
  equal(text.includes('  + [warning] 1:1 boom'), true)
  equal(text.includes('  - [error] 1:1 boom'), true)
})

test('identical findings are diffed as a multiset', () => {
  const { broadcaster, agents } = makeHarness()
  broadcaster.observe(SERVER, ROOT, PATH_A, [entry('error', 'boom'), entry('error', 'boom')])
  broadcaster.flush()
  broadcaster.observe(SERVER, ROOT, PATH_A, [entry('error', 'boom')])
  broadcaster.flush()

  const text = texts(agents[0]!)[1]!
  equal(text.includes('  - [error] 1:1 boom'), true)
  equal(text.includes('  + '), false)
})

test('findings below minSeverity are neither tracked nor injected', () => {
  const { broadcaster, agents } = makeHarness({ minSeverity: 'error' })
  broadcaster.observe(SERVER, ROOT, PATH_A, [entry('warning', 'meh')])
  broadcaster.flush()
  equal(agents[0]!.messages.length, 0)
})

test('a burst of files is coalesced into one message', () => {
  const { broadcaster, agents } = makeHarness()
  broadcaster.observe(SERVER, ROOT, PATH_A, [entry('error', 'a')])
  broadcaster.observe(SERVER, ROOT, PATH_B, [entry('error', 'b')])
  broadcaster.flush()

  equal(agents[0]!.messages.length, 1)
  const text = texts(agents[0]!)[0]!
  equal(text.includes('2 个文件'), true)
  equal(text.includes(PATH_A), true)
  equal(text.includes(PATH_B), true)
})

test('the coalescing timer flushes without an explicit call', async () => {
  const { broadcaster, agents } = makeHarness({ flushMs: 5 })
  broadcaster.observe(SERVER, ROOT, PATH_A, [entry('error', 'boom')])
  equal(agents[0]!.messages.length, 0)
  await new Promise(resolve => setTimeout(resolve, 80))
  equal(agents[0]!.messages.length, 1)
  broadcaster.dispose()
})

test('diagnostics outside the server root are ignored', () => {
  const { broadcaster, agents } = makeHarness()
  broadcaster.observe(SERVER, ROOT, 'C:\\other\\x.cpp', [entry('error', 'boom')])
  broadcaster.flush()
  equal(agents[0]!.messages.length, 0)
})

test('changes are routed to the agents of that server and root', () => {
  const a = makeAgent('agent-a')
  const b = makeAgent('agent-b')
  const { logger } = makeLogger()
  const broadcaster = new DiagnosticBroadcaster({
    minSeverity: 'warning',
    flushMs: 0,
    maxDiagnostics: 50,
    logger,
    targetsFor: (_serverId, root) => (root === ROOT ? [a] : [b]),
  })

  broadcaster.observe(SERVER, ROOT, PATH_A, [entry('error', 'boom')])
  broadcaster.flush()
  equal(a.messages.length, 1)
  equal(b.messages.length, 0)

  broadcaster.observe('rust-analyzer', 'C:\\other', 'C:\\other\\main.rs', [entry('error', 'nope')])
  broadcaster.flush()
  equal(b.messages.length, 1)
  equal(a.messages.length, 1)
  broadcaster.dispose()
})

test('a server with no target agent injects nothing but keeps its baseline', () => {
  const { broadcaster, agents, lines } = makeHarness({ targetsFor: () => [] })
  broadcaster.observe(SERVER, ROOT, PATH_A, [entry('error', 'boom')])
  broadcaster.flush()
  equal(agents[0]!.messages.length, 0)
  equal(lines.some(line => line.startsWith('debug dsh-lsp: no agent session rooted at')), true)
})

test('the injected list is capped and the omission is noted', () => {
  const { broadcaster, agents } = makeHarness({ maxDiagnostics: 2 })
  broadcaster.observe(SERVER, ROOT, PATH_A, [
    entry('error', 'one', 1),
    entry('error', 'two', 2),
    entry('error', 'three', 3),
  ])
  broadcaster.flush()

  const text = texts(agents[0]!)[0]!
  equal(text.split('\n').filter(line => line.startsWith('  + ')).length, 2)
  equal(text.includes('… 1 more diagnostic(s) omitted (maxDiagnostics)'), true)
})

test('dispose clears pending changes and stops injection', () => {
  const { broadcaster, agents } = makeHarness({ flushMs: 1000 })
  broadcaster.observe(SERVER, ROOT, PATH_A, [entry('error', 'boom')])
  broadcaster.dispose()
  broadcaster.flush()
  broadcaster.observe(SERVER, ROOT, PATH_B, [entry('error', 'b')])
  broadcaster.flush()
  equal(agents[0]!.messages.length, 0)
})

test('a throwing agent does not stop the others', () => {
  const bad: InjectableAgent = {
    id: 'bad',
    inject() {
      throw new Error('nope')
    },
  }
  const good = makeAgent('good')
  const { logger } = makeLogger()
  const broadcaster = new DiagnosticBroadcaster({
    minSeverity: 'warning',
    flushMs: 0,
    maxDiagnostics: 50,
    logger,
    targetsFor: () => [bad, good],
  })

  broadcaster.observe(SERVER, ROOT, PATH_A, [entry('error', 'boom')])
  broadcaster.flush()
  equal(good.messages.length, 1)
  broadcaster.dispose()
})

test('the injected message is a user-role notice', () => {
  const agent = makeAgent('agent-1')
  const { logger } = makeLogger()
  const broadcaster = new DiagnosticBroadcaster({
    minSeverity: 'warning',
    flushMs: 0,
    maxDiagnostics: 50,
    logger,
    targetsFor: () => [agent],
  })
  broadcaster.observe(SERVER, ROOT, PATH_A, [entry('error', 'boom')])
  broadcaster.flush()
  const message = agent.messages[0]!
  ok(message.text.length > 0)
  equal(message.summary.includes(SERVER), true)
  broadcaster.dispose()
})