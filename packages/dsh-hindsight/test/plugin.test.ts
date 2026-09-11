import test from 'node:test'
import assert from 'node:assert/strict'
import { mkdtempSync, readFileSync, readdirSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { DEFAULTS, resolveConfig, apply, inject, name } from '../src/index.js'
import type { ToolDefinition } from '../src/index.js'
import type { Context } from '@deepseek-ai/cordis'
import type { ToolRunContext } from '@deepseek-ai/dsh-tools'
import type { CommandInvocation, CommandResult } from '@deepseek-ai/dsh-commands'

interface SessionEventLike {
  type: string
  seq: number
  time: number
  data: any
}

interface SessionLike {
  id: string
  events: readonly SessionEventLike[]
  header?: {
    cwd?: string
    origin?: string
  }
}

const envKeys = [
  'HINDSIGHT_API_URL',
  'HINDSIGHT_API_KEY',
  'HINDSIGHT_BANK_ID',
  'HINDSIGHT_BUDGET',
  'HINDSIGHT_TIMEOUT',
  'HINDSIGHT_MEMORY_MODE',
  'HINDSIGHT_RETAIN_TAGS',
  'HINDSIGHT_RECALL_TAGS',
  'HINDSIGHT_RECALL_TYPES',
]

function withCleanEnv(run: () => Promise<void>): () => Promise<void> {
  return async () => {
    const saved = new Map(envKeys.map(key => [key, process.env[key]]))
    for (const key of envKeys) delete process.env[key]
    try {
      await run()
    } finally {
      for (const [key, value] of saved) {
        if (value === undefined) delete process.env[key]
        else process.env[key] = value
      }
    }
  }
}

interface PromptSectionLike {
  name: string
  order: number
  text: string
}

interface PreStepListener {
  (payload: any, next: () => Promise<any>): Promise<any>
}

interface CommandDefinitionLike {
  name: string
  description: string
  input?: { hint: string }
  handler: (invocation: CommandInvocation) => Promise<CommandResult>
}

interface FakeCtx {
  tools: {
    registered: ToolDefinition[]
    register(definition: ToolDefinition): () => void
  }
  commands: {
    registered: CommandDefinitionLike[]
    register(definition: CommandDefinitionLike): () => void
  }
  systemPrompt: {
    sections: PromptSectionLike[]
    section(section: PromptSectionLike): () => void
  }
  listeners: Map<string, any[]>
  on(event: string, listener: any, opts?: any): () => void
  sessionPersistence?: {
    list: () => Promise<Array<{ id: string; cwd?: string; createdAt: number }>>
    inspect: (sessionId: string) => Promise<{ meta: Record<string, unknown>; events: SessionEventLike[] }>
  }
}

function fakeCtx(): FakeCtx {
  const ctx: FakeCtx = {
    tools: {
      registered: [],
      register(definition) {
        this.registered.push(definition)
        return () => {}
      },
    },
    commands: {
      registered: [],
      register(definition) {
        this.registered.push(definition)
        return () => {}
      },
    },
    systemPrompt: {
      sections: [],
      section(section) { this.sections.push(section); return () => {} },
    },
    listeners: new Map(),
    on(event, listener, _opts) {
      const list = this.listeners.get(event) ?? []
      list.push(listener)
      this.listeners.set(event, list)
      return () => {}
    },
  }
  return ctx
}

// Helper: build realistic events for one turn. user/message data does NOT
// carry a `turn` field (as in harness 0.1.0-rc.6). assistant/message does.
function turnStartEvent(turn: number): SessionEventLike {
  return { type: 'turn/start', seq: 1, time: Date.now(), data: { turn } }
}

function turnEndEvent(turn: number, reason = 'completed'): SessionEventLike {
  return { type: 'turn/end', seq: 5, time: Date.now(), data: { turn, reason: { kind: reason } } }
}

function userMessage(text: string): SessionEventLike {
  return {
    type: 'user/message', seq: 2, time: Date.now(),
    data: { source: { kind: 'user' }, content: [{ type: 'text', text }] },
  }
}

function assistantMessage(turn: number, text: string): SessionEventLike {
  return {
    type: 'assistant/message', seq: 3, time: Date.now(),
    data: { turn, message: { content: [{ type: 'text', text }] } },
  }
}

function toolResult(turn: number, text: string): SessionEventLike {
  return {
    type: 'tool/result', seq: 4, time: Date.now(),
    data: { turn, message: { content: [{ type: 'tool-result', content: [{ type: 'text', text }] }] } },
  }
}

function makeSession(id: string, events: SessionEventLike[], header: SessionLike['header'] = {}): SessionLike {
  return { id, events, header }
}

interface FetchCall {
  url: string
  init: RequestInit
}

function fakeFetch(calls: FetchCall[]): typeof globalThis.fetch {
  const store: { mentalModels: Array<{id: string; name: string; source_query: string; content: string | null}> } = {
    mentalModels: [
      { id: 'user_advise', name: '用户偏好', source_query: '用户偏好', content: '## 用户偏好\n- 喜欢函数式编程' },
    ],
  }
  return (async (url: RequestInfo | URL, init: RequestInit = {}) => {
    const target = String(url)
    calls.push({ url: target, init })
    const body = init.body ? JSON.parse(String(init.body)) as Record<string, unknown> : undefined
    if (target.endsWith('/memories')) {
      return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({ success: true, items_count: (body?.items as unknown[] | undefined)?.length ?? 1 }) } as Response
    }
    if (target.endsWith('/memories/recall')) {
      return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({ results: [{ id: 'm1', text: 'Memory 1', type: 'observation' }] }) } as Response
    }
    if (target.endsWith('/reflect')) {
      return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({ text: 'Reflected answer' }) } as Response
    }
    if (target.endsWith('/version')) {
      return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({ version: '0.6.1' }) } as Response
    }
    if (/\/mental-models\/[^\/]+$/.test(target) && init.method === 'GET') {
      const id = decodeURIComponent(target.split('/').pop()!)
      const mm = store.mentalModels.find(m => m.id === id)
      if (mm) return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify(mm) } as Response
      return { ok: false, status: 404, statusText: 'not found', text: async () => JSON.stringify({ detail: 'not found' }) } as Response
    }
    if (target.endsWith('/mental-models') && init.method === 'GET') {
      return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({ items: store.mentalModels }) } as Response
    }
    if (target.endsWith('/mental-models') && init.method === 'POST') {
      const created = {
        id: String(body?.id ?? 'mm-created'),
        name: String(body?.name ?? ''),
        source_query: String(body?.source_query ?? ''),
        // Simulate the background reflect completing: content is present when
        // the model is fetched after the operation finishes.
        content: '## ' + String(body?.name ?? '') + '\n- 模拟生成内容',
      }
      store.mentalModels.push(created)
      return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({ mental_model_id: created.id, operation_id: 'op-1' }) } as Response
    }
    if (/\/operations\/[^\/]+$/.test(target) && init.method === 'GET') {
      return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({ status: 'completed' }) } as Response
    }
    return { ok: false, status: 404, statusText: 'not found', text: async () => JSON.stringify({ detail: 'not found' }) } as Response
  }) as unknown as typeof globalThis.fetch
}

async function waitFor(predicate: () => boolean, timeoutMs = 1000): Promise<void> {
  const deadline = Date.now() + timeoutMs
  while (!predicate()) {
    if (Date.now() > deadline) throw new Error('timed out waiting for condition')
    await new Promise(resolve => setTimeout(resolve, 5))
  }
}

// ----- A: config retainDocumentId null bug -----

test('resolveConfig default retainDocumentId is null (not string "null")', () => {
  const c = resolveConfig({}, {})
  assert.equal(c.retainDocumentId, null)
  // empty string and null/undefined also resolve to null
  assert.equal(resolveConfig({ retainDocumentId: '' }, {}).retainDocumentId, null)
  assert.equal(resolveConfig({ retainDocumentId: null }, {}).retainDocumentId, null)
  // explicit value is preserved
  assert.equal(resolveConfig({ retainDocumentId: 'my-doc' }, {}).retainDocumentId, 'my-doc')
})

test('resolveConfig recallTimeoutMs default is 6000', () => {
  const c = resolveConfig({}, {})
  assert.equal(c.recallTimeoutMs, 6000)
  assert.equal(resolveConfig({ recallTimeoutMs: 1000 }, {}).recallTimeoutMs, 1000)
})

test('resolveConfig recallPrefetch/recallOrder removed from config', () => {
  const c = resolveConfig({}, {})
  assert.ok(!('recallPrefetch' in c))
  assert.ok(!('recallOrder' in c))
})

// ----- Main plugin integration test (realistic event shapes) -----

test('plugin registers hooks, tools, and retains on turn/end (realistic events)', withCleanEnv(async () => {
  const ctx = fakeCtx()
  const calls: FetchCall[] = []
  const originalFetch = globalThis.fetch
  globalThis.fetch = fakeFetch(calls)
  try {
    apply(ctx as unknown as Context, {
      apiUrl: 'http://hindsight.test',
      bankId: 'test-bank',
      timeoutMs: 1000,
      autoRetain: true,
      retainEveryNTurns: 1,
      autoRecall: true,
      retainAsync: false,
    })

    assert.equal(name, 'dsh-hindsight')
    assert.deepEqual(inject, ['tools', 'systemPrompt', 'commands', 'sessionPersistence'])

    const toolNames = ctx.tools.registered.map(def => def.name)
    assert.ok(toolNames.includes('hindsight_retain'))
    assert.ok(toolNames.includes('hindsight_recall'))
    assert.ok(toolNames.includes('hindsight_reflect'))
    assert.ok(toolNames.includes('hindsight_status'))
    assert.ok(!toolNames.includes('hindsight_session_import'), 'session import is a slash command, not a tool')

    const commandNames = ctx.commands.registered.map(def => def.name)
    assert.ok(commandNames.includes('hindsight-import'))

    // Realistic events: user/message has NO turn field, only source.kind
    const session = makeSession('sess-1', [
      turnStartEvent(1),
      userMessage('Hello, remember my favorite color is blue'),
      assistantMessage(1, 'I will remember that.'),
      turnEndEvent(1),
    ])

    const onSessionEvent = ctx.listeners.get('session/event')?.[0]
    assert.ok(onSessionEvent)
    onSessionEvent(session, session.events[3]!) // turn/end

    await waitFor(() => calls.length >= 1)

    const retainCall = calls.find(call => call.url.endsWith('/memories'))
    assert.ok(retainCall, 'expected a retain request')
    assert.equal(retainCall.url, 'http://hindsight.test/v1/default/banks/test-bank/memories')

    const retainBody = JSON.parse(String(retainCall.init.body)) as { async: boolean; items: Array<{ content: string; document_id: string }> }
    assert.equal(retainBody.async, false)
    assert.match(retainBody.items[0]?.content ?? '', /User: Hello, remember my favorite color is blue/)
    assert.match(retainBody.items[0]?.content ?? '', /Assistant: I will remember that\./)
    // document_id should be sessionId-turn-N
    assert.match(retainBody.items[0]?.document_id ?? '', /^sess-1-turn-1$/)

    // Verify there is NO recallCache / systemPrompt.context hook
    assert.ok(!ctx.listeners.has('hindsight:recall'), 'should not have old recall context')

    // Verify the static section text is updated
    const section = ctx.systemPrompt.sections.find(s => s.name === 'hindsight:memory')
    assert.ok(section)
    assert.ok(!section!.text.includes('automatically injected as runtime context before a turn'),
      'old text should be gone')
    assert.ok(section!.text.includes('Hindsight reflect API'),
      'new text should mention reflect')
  } finally {
    globalThis.fetch = originalFetch
  }
}))

// ----- C: agent/pre-step hook -----

test('agent/pre-step schedules async recall and injects via agent.inject', withCleanEnv(async () => {
  const ctx = fakeCtx()
  const calls: FetchCall[] = []
  const originalFetch = globalThis.fetch
  globalThis.fetch = fakeFetch(calls)
  try {
    apply(ctx as unknown as Context, {
      apiUrl: 'http://hindsight.test',
      bankId: 'test-bank',
      timeoutMs: 1000,
      autoRecall: true,
      recallTimeoutMs: 5000,
    })

    const preStepListener = ctx.listeners.get('agent/pre-step')?.[0] as PreStepListener | undefined
    assert.ok(preStepListener, 'agent/pre-step listener should be registered')

    // Simulate a pre-step call with an agent exposing inject()
    const injectedMessages: any[] = []
    const decision = await preStepListener(
      {
        agent: { id: 'agent-1', inject: (msg: any) => injectedMessages.push(msg) },
        messages: [{ source: { kind: 'user' }, content: [{ type: 'text', text: 'What is my favorite color?' }] }],
        turn: 1,
        step: 1,
      },
      async () => ({ kind: 'enter', messages: [{ role: 'system', content: 'runtime context' }] }),
    )

    assert.equal(decision.kind, 'enter')
    assert.ok(Array.isArray(decision.messages))
    // The decision itself is unchanged — injection is async via agent.inject()
    assert.equal(decision.messages.length, 1, 'decision must not block on recall')

    // Reflect runs in the background and injects once it returns
    await waitFor(() => injectedMessages.length >= 1)
    const injected = injectedMessages[0] as any
    assert.equal(injected.role, 'user')
    assert.ok(injected.content[0].text.includes('hindsight-recall'))
    assert.ok(injected.content[0].text.includes('Memory 1'))

    // Verify recall was called
    const recallCall = calls.find(call => call.url.endsWith('/memories/recall'))
    assert.ok(recallCall, 'expected a recall request')
  } finally {
    globalThis.fetch = originalFetch
  }
}))

// ----- Dedup in agent/pre-step -----

test('agent/pre-step does not schedule recall twice for the same agent+turn', withCleanEnv(async () => {
  const ctx = fakeCtx()
  const calls: FetchCall[] = []
  const originalFetch = globalThis.fetch
  globalThis.fetch = fakeFetch(calls)
  try {
    apply(ctx as unknown as Context, {
      apiUrl: 'http://hindsight.test',
      bankId: 'test-bank',
      timeoutMs: 1000,
      autoRecall: true,
      recallTimeoutMs: 5000,
    })

    const preStepListener = ctx.listeners.get('agent/pre-step')?.[0] as PreStepListener | undefined
    assert.ok(preStepListener)

    const injectedMessages: any[] = []
    const agent = { id: 'agent-1', inject: (msg: any) => injectedMessages.push(msg) }

    // First call — decision unchanged, recall scheduled in background
    const d1 = await preStepListener(
      { agent, messages: [{ source: { kind: 'user' }, content: [{ type: 'text', text: 'Query' }] }], turn: 1, step: 1 },
      async () => ({ kind: 'enter', messages: [{ role: 'system', content: 'ctx' }] }),
    )
    assert.equal(d1.messages.length, 1, 'decision is unchanged (async injection)')

    // Second call — same agent+turn, should not schedule another reflect
    const d2 = await preStepListener(
      { agent, messages: [{ source: { kind: 'user' }, content: [{ type: 'text', text: 'Query' }] }], turn: 1, step: 2 },
      async () => ({ kind: 'enter', messages: [{ role: 'system', content: 'ctx' }] }),
    )
    assert.equal(d2.messages.length, 1, 'second call should skip scheduling')

    // Exactly one recall call and exactly one recall injection (mental-model
    // injections are scheduled separately and are not counted here).
    await waitFor(() => injectedMessages.length >= 1)
    assert.equal(calls.filter(call => call.url.endsWith('/memories/recall')).length, 1)
    const recallInjections = injectedMessages.filter((m: any) => String(m.content?.[0]?.text ?? '').includes('hindsight-recall'))
    assert.equal(recallInjections.length, 1, 'recall injected exactly once per agent+turn')
  } finally {
    globalThis.fetch = originalFetch
  }
}))


// ----- D: mental model injection in agent/pre-step -----

test('agent/pre-step injects user + project mental models (auto-create when missing)', withCleanEnv(async () => {
  const ctx = fakeCtx()
  const calls: FetchCall[] = []
  const originalFetch = globalThis.fetch
  globalThis.fetch = fakeFetch(calls)
  try {
    apply(ctx as unknown as Context, {
      apiUrl: 'http://hindsight.test',
      bankId: 'test-bank',
      timeoutMs: 1000,
      autoRecall: true,
      recallTimeoutMs: 5000,
      autoMentalModel: true,
      mentalModelUserQuery: '用户偏好',
      mentalModelProjectQueryTemplate: '项目 {cwd} 的\n- 概述\n- 项目架构',
      mentalModelAutoCreate: true,
      mentalModelTimeoutMs: 2000,
      mentalModelPollIntervalMs: 100,
    })

    const preStepListener = ctx.listeners.get('agent/pre-step')?.[0] as PreStepListener | undefined
    assert.ok(preStepListener, 'agent/pre-step listener should be registered')

    const injectedMessages: any[] = []
    const agent = {
      id: 'agent-mm',
      session: { header: { cwd: 'D:\\repo\\proj', origin: 'user' } },
      inject: (msg: any) => injectedMessages.push(msg),
    }
    const decision = await preStepListener(
      { agent, messages: [{ source: { kind: 'user' }, content: [{ type: 'text', text: 'Continue the work' }] }], turn: 1, step: 1 },
      async () => ({ kind: 'enter', messages: [{ role: 'system', content: 'ctx' }] }),
    )
    assert.equal(decision.kind, 'enter', 'decision must not block on mental models')

    // User model already exists (store has user_advise); project model is auto-created.
    await waitFor(() => injectedMessages.length >= 2, 3000)
    const mentalTexts = injectedMessages.map((m: any) => String(m.content?.[0]?.text ?? ''))
    const userInjected = mentalTexts.find(t => t.includes('hindsight-mental-model') && t.includes('用户偏好'))
    assert.ok(userInjected, 'user mental model should be injected')
    assert.ok(userInjected!.includes('喜欢函数式编程'), 'user mental model content should be injected')
    const projectInjected = mentalTexts.find(t => t.includes('hindsight-mental-model') && t.includes('项目'))
    assert.ok(projectInjected, 'project mental model should be injected')

    // A create request for the project model was issued (auto-create on missing).
    const createCall = calls.find(call => call.url.endsWith('/mental-models') && call.init.method === 'POST')
    assert.ok(createCall, 'expected a mental-model create request for the missing project model')
    const createBody = JSON.parse(String(createCall!.init.body)) as { name: string; source_query: string; trigger: Record<string, unknown> }
    assert.match(createBody.source_query, /项目 D:\\repo\\proj 的/)
    assert.equal(createBody.trigger.mode, 'delta', 'auto-created models should use the configured refresh mode (delta)')
    assert.equal(createBody.trigger.refresh_after_consolidation, true, 'auto-created models refresh after consolidation')
  } finally {
    globalThis.fetch = originalFetch
  }
}))

test('mental models are injected at most once per session', withCleanEnv(async () => {
  const ctx = fakeCtx()
  const calls: FetchCall[] = []
  const originalFetch = globalThis.fetch
  globalThis.fetch = fakeFetch(calls)
  try {
    apply(ctx as unknown as Context, {
      apiUrl: 'http://hindsight.test',
      bankId: 'test-bank',
      timeoutMs: 1000,
      autoRecall: false,
      autoMentalModel: true,
      mentalModelAutoCreate: false,
    })
    const preStepListener = ctx.listeners.get('agent/pre-step')?.[0] as PreStepListener | undefined
    assert.ok(preStepListener)
    const injectedMessages: any[] = []
    const agent = {
      id: 'agent-mm2',
      // No cwd → only the user mental model is wanted (no project model).
      session: { header: { origin: 'user' } },
      inject: (msg: any) => injectedMessages.push(msg),
    }
    const mkMsg = (i: number) => [{ source: { kind: 'user' }, content: [{ type: 'text', text: 'turn ' + i }] }]
    // First turn: the user model exists in the store, so it is injected.
    await preStepListener({ agent, messages: mkMsg(1), turn: 1, step: 1 }, async () => ({ kind: 'enter', messages: [{ role: 'system', content: 'c' }] }))
    await waitFor(() => injectedMessages.length >= 1)
    const mentalInjections = () => injectedMessages.filter((m: any) => String(m.content?.[0]?.text ?? '').includes('hindsight-mental-model'))
    assert.equal(mentalInjections().length, 1, 'first turn injects the user mental model')
    // Second turn, same agent: the plugin must not re-query or re-inject.
    const callsBefore = calls.length
    await preStepListener({ agent, messages: mkMsg(2), turn: 2, step: 1 }, async () => ({ kind: 'enter', messages: [{ role: 'system', content: 'c' }] }))
    await new Promise(resolve => setTimeout(resolve, 150))
    assert.equal(mentalInjections().length, 1, 'second turn does not re-inject the mental model')
    // No new /mental-models traffic after the first-turn injection.
    const mmCallsAfter = calls.slice(callsBefore).filter(c => c.url.includes('/mental-models'))
    assert.equal(mmCallsAfter.length, 0, 'same-session later turns skip mental-model queries')
  } finally {
    globalThis.fetch = originalFetch
  }
}))

test('each session (agent) gets its own mental-model injection', withCleanEnv(async () => {
  const ctx = fakeCtx()
  const calls: FetchCall[] = []
  const originalFetch = globalThis.fetch
  globalThis.fetch = fakeFetch(calls)
  try {
    apply(ctx as unknown as Context, {
      apiUrl: 'http://hindsight.test',
      bankId: 'test-bank',
      timeoutMs: 1000,
      autoRecall: false,
      autoMentalModel: true,
      mentalModelAutoCreate: false,
    })
    const preStepListener = ctx.listeners.get('agent/pre-step')?.[0] as PreStepListener | undefined
    assert.ok(preStepListener)
    const injectedMessages: any[] = []
    const mkAgent = (id: string) => ({
      id,
      session: { header: { cwd: 'C:\\work\\app', origin: 'user' } },
      inject: (msg: any) => injectedMessages.push(msg),
    })
    const mkMsg = (i: number) => [{ source: { kind: 'user' }, content: [{ type: 'text', text: 'turn ' + i }] }]
    const run = async (agent: any) => {
      await preStepListener({ agent, messages: mkMsg(1), turn: 1, step: 1 }, async () => ({ kind: 'enter', messages: [{ role: 'system', content: 'c' }] }))
    }
    await run(mkAgent('session-A'))
    await waitFor(() => injectedMessages.length >= 1)
    await run(mkAgent('session-B'))
    await waitFor(() => injectedMessages.length >= 2)
    const mentalInjections = injectedMessages.filter((m: any) => String(m.content?.[0]?.text ?? '').includes('hindsight-mental-model'))
    assert.equal(mentalInjections.length, 2, 'each session receives its own injection')
  } finally {
    globalThis.fetch = originalFetch
  }
}))

// ----- memoryMode=context hides tools but still has auto-recall -----

test('memoryMode=context hides tools but keeps pre-step hook', withCleanEnv(async () => {
  const ctx = fakeCtx()
  apply(ctx as unknown as Context, { apiUrl: 'http://hindsight.test', memoryMode: 'context', timeoutMs: 1000 })
  assert.deepEqual(ctx.tools.registered, [])
  assert.equal(ctx.systemPrompt.sections.length, 0)
  // slash commands are user-driven, so they stay registered in context mode
  assert.ok(ctx.commands.registered.some(def => def.name === 'hindsight-import'))
  // pre-step hook should still be registered in context mode
  const preStepListener = ctx.listeners.get('agent/pre-step')?.[0]
  assert.ok(preStepListener)
}))

// ----- E: /hindsight-import slash command -----

function invocation(rawInput: string): CommandInvocation {
  return {
    commandId: 'hindsight-import' as CommandInvocation['commandId'],
    agent: {} as CommandInvocation['agent'],
    rawInput,
    attachments: [],
    signal: new AbortController().signal,
  }
}

test('hindsight-import command is registered and lists sessions', withCleanEnv(async () => {
  const ctx = fakeCtx()
  ctx.sessionPersistence = {
    list: async () => [{ id: 's1', cwd: '/proj', createdAt: 1735689600000 }],
    inspect: async (_id: string) => ({ meta: {}, events: [] }),
  }
  const originalFetch = globalThis.fetch
  globalThis.fetch = fakeFetch([])
  try {
    apply(ctx as unknown as Context, { apiUrl: 'http://hindsight.test', bankId: 'test-bank', timeoutMs: 1000 })

    const importCommand = ctx.commands.registered.find(def => def.name === 'hindsight-import')
    assert.ok(importCommand, 'hindsight-import should be registered')
    assert.ok(importCommand!.input?.hint, 'command should expose an input hint')

    // List mode
    const listResult = await importCommand!.handler(invocation(''))
    assert.equal(listResult.kind, 'success')
    assert.match((listResult as { text: string }).text, /Found 1 session/)
    assert.match((listResult as { text: string }).text, /s1/)

    // Import mode with no events
    ctx.sessionPersistence!.inspect = async () => ({ meta: {}, events: [] })
    const emptyResult = await importCommand!.handler(invocation('s1'))
    assert.equal(emptyResult.kind, 'success')
    assert.match((emptyResult as { text: string }).text, /has no events to import/)
  } finally {
    globalThis.fetch = originalFetch
  }
}))

test('hindsight-import imports turns from historical session', withCleanEnv(async () => {
  const ctx = fakeCtx()
  ctx.sessionPersistence = {
    list: async () => [],
    inspect: async (_id: string) => ({
      meta: {},
      events: [
        turnStartEvent(1),
        userMessage('Historical query'),
        assistantMessage(1, 'Historical answer'),
        turnEndEvent(1),
      ],
    }),
  }
  const calls: FetchCall[] = []
  const originalFetch = globalThis.fetch
  globalThis.fetch = fakeFetch(calls)
  try {
    apply(ctx as unknown as Context, { apiUrl: 'http://hindsight.test', bankId: 'test-bank', timeoutMs: 1000, retainAsync: false })

    const importCommand = ctx.commands.registered.find(def => def.name === 'hindsight-import')
    assert.ok(importCommand)

    const result = await importCommand!.handler(invocation('s1'))
    assert.equal(result.kind, 'success')
    assert.match((result as { text: string }).text, /Imported 1 of 1 turn/)
    assert.match((result as { text: string }).text, /s1/)

    const retainCall = calls.find(call => call.url.endsWith('/memories'))
    assert.ok(retainCall)
    const body = JSON.parse(String(retainCall.init.body)) as { items: Array<{ document_id: string; content: string }> }
    assert.match(body.items[0]!.document_id, /^s1-turn-1$/)
    assert.match(body.items[0]!.content, /Historical query/)
    assert.match(body.items[0]!.content, /Historical answer/)
  } finally {
    globalThis.fetch = originalFetch
  }
}))

test('hindsight-import parses options from raw input', withCleanEnv(async () => {
  const ctx = fakeCtx()
  let inspectedId = ''
  ctx.sessionPersistence = {
    list: async () => [],
    inspect: async (id: string) => {
      inspectedId = id
      return { meta: {}, events: [] }
    },
  }
  const originalFetch = globalThis.fetch
  globalThis.fetch = fakeFetch([])
  try {
    apply(ctx as unknown as Context, { apiUrl: 'http://hindsight.test', bankId: 'test-bank', timeoutMs: 1000 })
    const importCommand = ctx.commands.registered.find(def => def.name === 'hindsight-import')
    assert.ok(importCommand)
    // --bank / --max-turns / --turn-kinds are accepted but no events, so no retain happens
    const r = await importCommand!.handler(invocation('s2 --bank other-bank --max-turns 5 --turn-kinds completed,aborted'))
    assert.equal(r.kind, 'success')
    assert.equal(inspectedId, 's2')
  } finally {
    globalThis.fetch = originalFetch
  }
}))

// ----- buildTurnRecord with realistic events -----

test('buildTurnRecord builds from realistic events (user/message without turn)', withCleanEnv(async () => {
  // Import the buildTurnRecord function to test directly
  const { buildTurnRecord } = await import('../src/transcript.js')
  const config = { ...DEFAULTS } as any
  const session = {
    id: 'sess-test',
    events: [
      turnStartEvent(1),
      userMessage('Hello world'),
      assistantMessage(1, 'I hear you'),
      turnEndEvent(1),
    ],
  }
  const record = buildTurnRecord(session as any, 1, config)
  assert.ok(record)
  assert.equal(record!.turn, 1)
  assert.ok(record!.query.includes('Hello world'))
  assert.ok(record!.messages.length >= 2)
}))

// ----- File logging (Plan A) -----

test('apply installs a file logger exporter that writes hindsight logs', withCleanEnv(async () => {
  const dir = mkdtempSync(join(tmpdir(), 'hindsight-log-'))
  try {
    const ctx = fakeCtx() as unknown as { logger?: unknown }
    const exporters: Array<{ export(message: unknown): void }> = []
    ;(ctx as Record<string, unknown>).logger = Object.assign(
      () => ({ error() {}, info() {}, warn() {}, debug() {} }),
      { exporter: (exporter: { export(message: unknown): void }) => { exporters.push(exporter); return () => {} } },
    )

    apply(ctx as unknown as Context, {
      apiUrl: 'http://hindsight.test',
      bankId: 'test-bank',
      logDir: dir,
      memoryMode: 'context',
      timeoutMs: 1000,
    })

    assert.equal(exporters.length, 1, 'file exporter should be registered')
    exporters[0]!.export({
      sn: 1,
      ts: 1726000000000,
      type: 'info',
      level: 1,
      name: 'hindsight',
      args: ['retained 2 turn(s) for session sess-1'],
    })

    const files = readdirSync(dir).filter(f => f.endsWith('.log'))
    assert.equal(files.length, 1)
    const content = readFileSync(join(dir, files[0]!), 'utf8')
    assert.match(content, /\[info\] hindsight: retained 2 turn\(s\) for session sess-1/)
  } finally {
    rmSync(dir, { recursive: true, force: true })
  }
}))

