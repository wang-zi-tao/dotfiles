import test from 'node:test'
import assert from 'node:assert/strict'
import { mkdtempSync, readFileSync, readdirSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { DEFAULTS, resolveConfig, apply, inject, name } from '../src/index.js'
import { expandCronHash } from '../src/client.js'
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
  'HINDSIGHT_RETAIN_CWD_TAG_PREFIX',
  'HINDSIGHT_RECALL_TAGS',
  'HINDSIGHT_RECALL_TYPES',
  'HINDSIGHT_INITIATIVE_CAPTURE',
  'HINDSIGHT_INITIATIVE_FOLDER',
  'HINDSIGHT_INITIATIVE_REFRESH_CRON',
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
    list: () => Promise<Array<{ header: { id: string; cwd?: string; createdAt: number } }>>
    open: (sessionId: string, access: 'read') => Promise<{
      read: () => Promise<{ events: SessionEventLike[] }>
      close: () => Promise<void>
    }>
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

interface FakeMentalModel {
  id: string
  name: string
  source_query: string
  content: string | null
  tags: string[]
  trigger?: Record<string, unknown>
}

function fakeFetch(calls: FetchCall[], seed: FakeMentalModel[] = []): typeof globalThis.fetch {
  const store: {
    mentalModels: FakeMentalModel[]
    folders: Array<Record<string, unknown>>
    pages: Array<Record<string, unknown>>
  } = {
    mentalModels: [
      { id: 'user_advise', name: '用户偏好', source_query: '用户偏好', content: '## 用户偏好\n- 喜欢函数式编程', tags: [] },
      ...seed,
    ],
    folders: [],
    pages: [],
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
    if (target.includes('/knowledge-base/tree')) {
      const created = store.folders.map(folder => ({
        ...folder,
        children: store.pages.filter(page => page.parent_id === folder.id),
      }));
      return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({
        roots: [...created, {
          id: 'kf-wpsmain', kind: 'folder', name: 'wpsmain', parent_id: 'kf-root', description: '', tags: [],
          children: [
            {
              id: 'kp-component', kind: 'page', name: 'Component map', parent_id: 'kf-wpsmain',
              mental_model_id: 'mm-component', description: '为仓库 wpsmain 产出组件地图：列出核心模块、职责、依赖和改动的入口。这一句很长很长很长很长很长很长很长很长很长很长',
              tags: ['project:wpsmain', 'knowledge:component'], is_stale: true,
            },
            {
              id: 'kp-conventions', kind: 'page', name: 'Conventions', parent_id: 'kf-wpsmain',
              mental_model_id: 'mm-conventions', description: '产出编码与构建约定：命名、格式化、构建命令和测试流程。',
              tags: ['project:wpsmain', 'knowledge:convention'], is_stale: false,
            },
          ],
        }],
      }) } as Response
    }
    if (target.endsWith('/knowledge-base/folders') && init.method === 'POST') {
      const folder = {
        id: 'kf-initiatives', kind: 'folder', name: String(body?.name ?? ''),
        parent_id: body?.parent_id ?? null, children: [],
      };
      store.folders.push(folder);
      return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify(folder) } as Response;
    }
    if (target.endsWith('/knowledge-base/pages') && init.method === 'POST') {
      const page = {
        id: 'kp-new', kind: 'page', name: String(body?.name ?? ''),
        parent_id: body?.parent_id ?? null, mental_model_id: 'mm-new',
        description: String(body?.source_query ?? ''),
        tags: Array.isArray(body?.tags) ? body!.tags : [], is_stale: false,
      };
      store.pages.push(page);
      return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({ page_id: 'kp-new', mental_model_id: 'mm-new', operation_id: 'op-page' }) } as Response;
    }
    if (target.includes('/knowledge-base/search')) {
      return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({
        results: [{
          id: 'kp-component', name: 'Component map', mental_model_id: 'mm-component',
          source_query: '组件地图', snippet: '- 模块地图 TL;DR：kso 是外壳，et 是表格内核', score: 12.5,
          updated_at: '2026-11-10T00:00:00Z',
        }],
      }) } as Response
    }
    if (target.includes('/knowledge-base/pages/')) {
      const pageId = decodeURIComponent(target.split('/').pop()!)
      const name = pageId === 'kp-component' ? 'Component map' : 'Conventions'
      const body = '# ' + name + '\n' + 'x'.repeat(2000)
      return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({
        id: pageId, name, type: 'knowledge-page', description: 'source query', tags: [], timestamp: '2026-11-10T00:00:00Z',
        body, markdown: body,
      }) } as Response
    }
    if (target.endsWith('/version')) {
      return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({ version: '0.6.1' }) } as Response
    }
    if (/\/mental-models\/[^\/]+$/.test(target) && init.method === 'PATCH') {
      const id = decodeURIComponent(target.split('/').pop()!)
      const mm = store.mentalModels.find(m => m.id === id)
      if (!mm) return { ok: false, status: 404, statusText: 'not found', text: async () => JSON.stringify({ detail: 'not found' }) } as Response
      if (Array.isArray(body?.tags)) mm.tags = (body!.tags as unknown[]).map(String)
      if (body?.trigger && typeof body.trigger === 'object') {
        mm.trigger = {...(mm.trigger ?? {}), ...(body.trigger as Record<string, unknown>)}
      }
      return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify(mm) } as Response
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
        tags: Array.isArray(body?.tags) ? (body!.tags as unknown[]).map(String) : [],
        ...(body?.trigger && typeof body.trigger === 'object'
          ? {trigger: body.trigger as Record<string, unknown>}
          : {}),
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

test('resolveConfig recallTimeoutMs default is the soft 20s threshold', () => {
  const c = resolveConfig({}, {})
  assert.equal(c.recallTimeoutMs, 20000)
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
    assert.ok(section!.text.includes('pull-based'),
      'new text should describe pull-based memory')
    assert.ok(section!.text.includes('hindsight_list_pages'),
      'new text should advertise the knowledge page tools')
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


// ----- Recall diagnostics: slow calls and failures must be observable -----

test('recall slower than recallTimeoutMs still injects and logs a slow-call warning', withCleanEnv(async () => {
  const ctx = fakeCtx()
  const logs: string[] = []
  ;(ctx as any).logger = () => ({
    debug() {},
    info() {},
    warn: (message: string) => logs.push(message),
    error() {},
  })
  const originalFetch = globalThis.fetch
  globalThis.fetch = (async (input: unknown) => {
    const url = String(input)
    if (url.endsWith('/memories/recall')) {
      // Respond well past recallTimeoutMs but inside the client timeout.
      await new Promise(resolve => setTimeout(resolve, 300))
      return {
        ok: true,
        status: 200,
        statusText: '',
        text: async () => JSON.stringify({ results: [{ id: 'm1', text: 'Memory 1', type: 'observation' }] }),
      } as Response
    }
    return { ok: false, status: 404, statusText: 'not found', text: async () => '{}' } as Response
  }) as unknown as typeof globalThis.fetch
  try {
    apply(ctx as unknown as Context, {
      apiUrl: 'http://hindsight.test',
      bankId: 'test-bank',
      timeoutMs: 2000,
      autoRecall: true,
      recallTimeoutMs: 100,
      autoRetain: false,
      autoMentalModel: false,
      knowledgePagesEnabled: false,
    })
    const preStepListener = ctx.listeners.get('agent/pre-step')?.[0] as PreStepListener | undefined
    assert.ok(preStepListener)

    const injected: any[] = []
    const decision = await preStepListener(
      {
        agent: { id: 'agent-slow', inject: (msg: any) => injected.push(msg) },
        messages: [{ source: { kind: 'user' }, content: [{ type: 'text', text: 'slow query' }] }],
        turn: 1,
        step: 1,
      },
      async () => ({ kind: 'enter', messages: [{ role: 'system', content: 'ctx' }] }),
    )
    assert.equal(decision.messages.length, 1, 'pre-step must not block on a slow recall')

    await waitFor(() => injected.length >= 1, 3000)
    assert.ok(String(injected[0].content[0].text).includes('Memory 1'))
    assert.ok(logs.some(line => line.includes('recall is slow')), 'a slow recall must be logged')
  } finally {
    globalThis.fetch = originalFetch
  }
}))

test('a failing recall is logged and never breaks the pre-step', withCleanEnv(async () => {
  const ctx = fakeCtx()
  const logs: string[] = []
  ;(ctx as any).logger = () => ({
    debug() {},
    info() {},
    warn: (message: string) => logs.push(message),
    error() {},
  })
  const originalFetch = globalThis.fetch
  globalThis.fetch = (async () => {
    throw new Error('network down')
  }) as unknown as typeof globalThis.fetch
  try {
    apply(ctx as unknown as Context, {
      apiUrl: 'http://hindsight.test',
      bankId: 'test-bank',
      timeoutMs: 2000,
      autoRecall: true,
      recallTimeoutMs: 5000,
      autoRetain: false,
      autoMentalModel: false,
      knowledgePagesEnabled: false,
    })
    const preStepListener = ctx.listeners.get('agent/pre-step')?.[0] as PreStepListener | undefined
    assert.ok(preStepListener)

    const injected: any[] = []
    const decision = await preStepListener(
      {
        agent: { id: 'agent-fail', inject: (msg: any) => injected.push(msg) },
        messages: [{ source: { kind: 'user' }, content: [{ type: 'text', text: 'query' }] }],
        turn: 1,
        step: 1,
      },
      async () => ({ kind: 'enter', messages: [{ role: 'system', content: 'ctx' }] }),
    )
    assert.equal(decision.kind, 'enter')

    await waitFor(() => logs.some(line => line.includes('hindsight recall failed')), 3000)
    assert.equal(injected.length, 0, 'nothing is injected when recall fails')
    assert.ok(
      logs.some(line => line.includes('network down')),
      'the underlying error must reach the log',
    )
  } finally {
    globalThis.fetch = originalFetch
  }
}))

// ----- D: mental model injection in agent/pre-step -----

// Project mental models are off by default: knowledge pages carry project
// knowledge, so pushing a per-project model would double the context cost.
test('agent/pre-step injects only the user mental model by default', withCleanEnv(async () => {
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

    // Only the user model (already in the store) is injected.
    await waitFor(() => injectedMessages.length >= 1, 3000)
    await new Promise(resolve => setTimeout(resolve, 200))
    const mentalTexts = injectedMessages.map((m: any) => String(m.content?.[0]?.text ?? ''))
    const userInjected = mentalTexts.find(t => t.includes('hindsight-mental-model') && t.includes('用户偏好'))
    assert.ok(userInjected, 'user mental model should be injected')
    assert.ok(userInjected!.includes('喜欢函数式编程'), 'user mental model content should be injected')
    assert.equal(
      mentalTexts.filter(t => t.includes('hindsight-mental-model')).length,
      1,
      'the project mental model must not be injected unless mentalModelProjectEnabled is on',
    )

    // No project model is created and nothing is patched (no cwd model exists).
    assert.equal(
      calls.find(call => call.url.endsWith('/mental-models') && call.init.method === 'POST'),
      undefined,
      'project models are not auto-created while mentalModelProjectEnabled is off',
    )
    assert.equal(
      calls.find(call => call.url.includes('/mental-models/') && call.init.method === 'PATCH'),
      undefined,
      'there is no cwd-tagged model to mute',
    )
  } finally {
    globalThis.fetch = originalFetch
  }
}))

test('mentalModelProjectEnabled injects the project model with auto-refresh off', withCleanEnv(async () => {
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
      mentalModelProjectEnabled: true,
      mentalModelTimeoutMs: 2000,
      mentalModelPollIntervalMs: 100,
    })

    const preStepListener = ctx.listeners.get('agent/pre-step')?.[0] as PreStepListener | undefined
    assert.ok(preStepListener, 'agent/pre-step listener should be registered')

    const injectedMessages: any[] = []
    const agent = {
      id: 'agent-mm-enabled',
      session: { header: { cwd: 'D:\\repo\\proj', origin: 'user' } },
      inject: (msg: any) => injectedMessages.push(msg),
    }
    await preStepListener(
      { agent, messages: [{ source: { kind: 'user' }, content: [{ type: 'text', text: 'Continue the work' }] }], turn: 1, step: 1 },
      async () => ({ kind: 'enter', messages: [{ role: 'system', content: 'ctx' }] }),
    )

    await waitFor(() => injectedMessages.length >= 2, 3000)
    const mentalTexts = injectedMessages.map((m: any) => String(m.content?.[0]?.text ?? ''))
    assert.ok(mentalTexts.some(t => t.includes('hindsight-mental-model') && t.includes('用户偏好')), 'user model injected')
    assert.ok(mentalTexts.some(t => t.includes('hindsight-mental-model') && t.includes('项目')), 'project model injected when enabled')

    const createCall = calls.find(call => call.url.endsWith('/mental-models') && call.init.method === 'POST')
    assert.ok(createCall, 'expected a mental-model create request for the missing project model')
    const createBody = JSON.parse(String(createCall!.init.body)) as {
      name: string
      source_query: string
      tags: string[]
      trigger: Record<string, unknown>
    }
    assert.match(createBody.source_query, /项目 D:\\repo\\proj 的/)
    assert.equal(createBody.trigger.mode, 'delta', 'auto-created models should use the configured refresh mode (delta)')
    assert.equal(
      createBody.trigger.refresh_after_consolidation,
      false,
      'a project model is created without auto-refresh (mentalModelProjectAutoRefresh defaults to false)',
    )
    assert.equal(createBody.trigger.refresh_cron, null, 'no refresh cron is scheduled for a project model')
    assert.deepEqual(createBody.tags, ['cwd:D:\\repo\\proj'], 'the project model is scoped by the session cwd tag')
    assert.equal(createBody.trigger.tags_match, 'all_strict', 'a tagged model must exclude untagged memories')
  } finally {
    globalThis.fetch = originalFetch
  }
}))

test('an existing project mental model is muted and not injected by default', withCleanEnv(async () => {
  const ctx = fakeCtx()
  const calls: FetchCall[] = []
  const originalFetch = globalThis.fetch
  globalThis.fetch = fakeFetch(calls, [{
    id: 'mm-proj',
    name: '项目 D:\\repo\\proj',
    source_query: '项目 D:\\repo\\proj 的概述',
    content: '## 项目 D:\\repo\\proj\n- 旧的项目模型正文',
    tags: ['cwd:D:\\repo\\proj'],
    trigger: { mode: 'delta', refresh_after_consolidation: true, refresh_cron: null },
  }])
  try {
    apply(ctx as unknown as Context, {
      apiUrl: 'http://hindsight.test',
      bankId: 'test-bank',
      timeoutMs: 1000,
      autoRecall: false,
      autoMentalModel: true,
      mentalModelUserQuery: '用户偏好',
      mentalModelProjectQueryTemplate: '项目 {cwd} 的\n- 概述',
      mentalModelAutoCreate: true,
      mentalModelTimeoutMs: 2000,
      mentalModelPollIntervalMs: 100,
    })

    const preStepListener = ctx.listeners.get('agent/pre-step')?.[0] as PreStepListener | undefined
    assert.ok(preStepListener, 'agent/pre-step listener should be registered')

    const injectedMessages: any[] = []
    const agent = {
      id: 'agent-mm-mute',
      session: { header: { cwd: 'D:\\repo\\proj', origin: 'user' } },
      inject: (msg: any) => injectedMessages.push(msg),
    }
    await preStepListener(
      { agent, messages: [{ source: { kind: 'user' }, content: [{ type: 'text', text: 'Continue the work' }] }], turn: 1, step: 1 },
      async () => ({ kind: 'enter', messages: [{ role: 'system', content: 'ctx' }] }),
    )

    await waitFor(
      () => calls.some(call => call.url.includes('/mental-models/mm-proj') && call.init.method === 'PATCH'),
      3000,
    )
    const patchCall = calls.find(call => call.url.includes('/mental-models/mm-proj') && call.init.method === 'PATCH')!
    const patchBody = JSON.parse(String(patchCall.init.body)) as { trigger: Record<string, unknown> }
    assert.equal(
      patchBody.trigger.refresh_after_consolidation,
      false,
      'the existing project model must stop refreshing after consolidation',
    )
    assert.equal(patchBody.trigger.refresh_cron, null, 'no refresh cron remains on the muted project model')
    assert.equal(
      calls.find(call => call.url.endsWith('/mental-models') && call.init.method === 'POST'),
      undefined,
      'the existing model is reused, not re-created',
    )

    await new Promise(resolve => setTimeout(resolve, 200))
    const mentalTexts = injectedMessages.map((m: any) => String(m.content?.[0]?.text ?? ''))
    assert.ok(mentalTexts.some(t => t.includes('用户偏好')), 'the user model is still injected')
    assert.ok(
      !mentalTexts.some(t => t.includes('旧的项目模型正文')),
      'the muted project model is not injected',
    )
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
    // Wait on the mental-model injection itself: the page roster also injects
    // messages, so a bare length check can pass too early.
    const mentalTexts = () => injectedMessages
      .map((m: any) => String(m.content?.[0]?.text ?? ''))
      .filter(text => text.includes('hindsight-mental-model'))
    await run(mkAgent('session-A'))
    await waitFor(() => mentalTexts().length >= 1)
    await run(mkAgent('session-B'))
    await waitFor(() => mentalTexts().length >= 2)
    assert.equal(mentalTexts().length, 2, 'each session receives its own injection')
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

/**
 * 0.1.7 read handle standing in for `inspect`, which was removed: a stored
 * session is opened read-only and its log slice read through the handle.
 */
function fakeReadHandle(events: SessionEventLike[]) {
  return {
    read: async () => ({ events }),
    close: async () => {},
  }
}

test('hindsight-import command is registered and lists sessions', withCleanEnv(async () => {
  const ctx = fakeCtx()
  ctx.sessionPersistence = {
    list: async () => [{ header: { id: 's1', cwd: '/proj', createdAt: 1735689600000 } }],
    open: async () => fakeReadHandle([]),
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
    ctx.sessionPersistence!.open = async () => fakeReadHandle([])
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
    open: async () => fakeReadHandle([
      turnStartEvent(1),
      userMessage('Historical query'),
      assistantMessage(1, 'Historical answer'),
      turnEndEvent(1),
    ]),
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
    open: async (id: string) => {
      inspectedId = id
      return fakeReadHandle([])
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

// ----- E: cwd visibility tags (retain scope + mental-model scope) -----

const flushSession = (ctx: FakeCtx, session: SessionLike) => {
  const listener = ctx.listeners.get('session/event')?.[0]
  assert.ok(listener, 'session/event listener should be registered')
  listener(session, session.events[session.events.length - 1]!)
}

const retainItemsOf = (calls: FetchCall[]) => {
  const call = calls.find(c => c.url.endsWith('/memories'))
  assert.ok(call, 'expected a retain request')
  return JSON.parse(String(call!.init.body)) as { items: Array<{ tags: string[]; metadata: Record<string, string> }> }
}

test('auto-retain writes the session cwd tag on every memory', withCleanEnv(async () => {
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
      autoRecall: false,
      retainAsync: false,
    })

    const session = makeSession('sess-tag', [
      turnStartEvent(1),
      userMessage('remember the tag'),
      assistantMessage(1, 'ok'),
      turnEndEvent(1),
    ], { cwd: 'C:\\repo\\tagged' })
    flushSession(ctx, session)
    await waitFor(() => calls.length >= 1)

    const body = retainItemsOf(calls)
    assert.deepEqual(body.items[0]?.tags, ['cwd:C:\\repo\\tagged', 'session:sess-tag'])
    assert.equal(body.items[0]?.metadata.cwd, 'C:\\repo\\tagged', 'cwd metadata stays for traceability')
  } finally {
    globalThis.fetch = originalFetch
  }
}))

test('a session without a cwd gets only the session tag', withCleanEnv(async () => {
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
      autoRecall: false,
      retainAsync: false,
    })

    const session = makeSession('sess-nocwd', [
      turnStartEvent(1),
      userMessage('no cwd here'),
      assistantMessage(1, 'ok'),
      turnEndEvent(1),
    ])
    flushSession(ctx, session)
    await waitFor(() => calls.length >= 1)

    const body = retainItemsOf(calls)
    assert.deepEqual(body.items[0]?.tags, ['session:sess-nocwd'])
  } finally {
    globalThis.fetch = originalFetch
  }
}))

test('retainCwdTagPrefix can disable the cwd tag', withCleanEnv(async () => {
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
      autoRecall: false,
      retainAsync: false,
      retainCwdTagPrefix: '',
    })

    const session = makeSession('sess-off', [
      turnStartEvent(1),
      userMessage('cwd but tagging off'),
      assistantMessage(1, 'ok'),
      turnEndEvent(1),
    ], { cwd: 'C:\\repo\\off' })
    flushSession(ctx, session)
    await waitFor(() => calls.length >= 1)

    const body = retainItemsOf(calls)
    assert.deepEqual(body.items[0]?.tags, ['session:sess-off'])
  } finally {
    globalThis.fetch = originalFetch
  }
}))

test('hindsight_retain adds the calling session cwd tag', withCleanEnv(async () => {
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
      autoRetain: false,
      retainAsync: false,
    })
    const retainTool = ctx.tools.registered.find(def => def.name === 'hindsight_retain')
    assert.ok(retainTool, 'hindsight_retain should be registered')

    await retainTool!.execute(
      { content: 'a note worth scoping' },
      { agent: { session: { header: { cwd: 'D:\\proj\\a' } } } } as unknown as ToolRunContext,
    )

    const body = retainItemsOf(calls)
    assert.deepEqual(body.items[0]?.tags, ['cwd:D:\\proj\\a'])
    assert.equal(body.items[0]?.metadata.cwd, 'D:\\proj\\a')
  } finally {
    globalThis.fetch = originalFetch
  }
}))

test('an existing mental model is re-scoped to the session cwd tag', withCleanEnv(async () => {
  const ctx = fakeCtx()
  const calls: FetchCall[] = []
  const originalFetch = globalThis.fetch
  const sourceQuery = '项目 C:\\work\\app 的\n- 概述'
  globalThis.fetch = fakeFetch(calls, [{
    id: 'mm-legacy',
    name: '项目 C:\\work\\app',
    source_query: sourceQuery,
    content: '## 项目 C:\\work\\app\n- 旧内容',
    tags: [],
  }])
  try {
    apply(ctx as unknown as Context, {
      apiUrl: 'http://hindsight.test',
      bankId: 'test-bank',
      timeoutMs: 1000,
      autoRecall: false,
      autoMentalModel: true,
      mentalModelProjectQueryTemplate: '项目 {cwd} 的\n- 概述',
      mentalModelAutoCreate: true,
      mentalModelProjectEnabled: true,
      mentalModelTimeoutMs: 2000,
      mentalModelPollIntervalMs: 100,
    })
    const preStepListener = ctx.listeners.get('agent/pre-step')?.[0] as PreStepListener | undefined
    assert.ok(preStepListener)

    const injectedMessages: any[] = []
    const agent = {
      id: 'agent-legacy',
      session: { header: { cwd: 'C:\\work\\app', origin: 'user' } },
      inject: (msg: any) => injectedMessages.push(msg),
    }
    await preStepListener(
      { agent, messages: [{ source: { kind: 'user' }, content: [{ type: 'text', text: 'continue' }] }], turn: 1, step: 1 },
      async () => ({ kind: 'enter', messages: [{ role: 'system', content: 'ctx' }] }),
    )
    await waitFor(() => injectedMessages.length >= 1, 3000)

    const patchCall = calls.find(c => c.url.endsWith('/mental-models/mm-legacy') && c.init.method === 'PATCH')
    assert.ok(patchCall, 'an untagged legacy model should be patched with the cwd tag')
    assert.deepEqual(JSON.parse(String(patchCall!.init.body)), {
      tags: ['cwd:C:\\work\\app'],
      // Project models default to no auto-refresh, so the same patch also
      // silences the legacy model's consolidation refresh.
      trigger: { mode: 'delta', refresh_after_consolidation: false, refresh_cron: null },
    })

    // The model is not re-created and its content is still injected.
    assert.ok(!calls.some(c => c.url.endsWith('/mental-models') && c.init.method === 'POST'))
    const mental = injectedMessages
      .map((m: any) => String(m.content?.[0]?.text ?? ''))
      .find(t => t.includes('hindsight-mental-model') && t.includes('项目'))
    assert.ok(mental, 'the reconciled model content is injected')
    assert.ok(mental!.includes('旧内容'), 'tagging does not wipe the stored content')

    // A model already carrying the wanted tags is left alone.
    const callsBefore = calls.length
    const agent2 = { ...agent, id: 'agent-legacy-2' }
    await preStepListener(
      { agent: agent2, messages: [{ source: { kind: 'user' }, content: [{ type: 'text', text: 'again' }] }], turn: 1, step: 1 },
      async () => ({ kind: 'enter', messages: [{ role: 'system', content: 'ctx' }] }),
    )
    await new Promise(resolve => setTimeout(resolve, 150))
    assert.equal(
      calls.slice(callsBefore).filter(c => c.init.method === 'PATCH').length,
      0,
      'an already-scoped model is not patched again',
    )
  } finally {
    globalThis.fetch = originalFetch
  }
}))

// ----- E: knowledge pages (read-only pull surface) -----

test('knowledge page tools are registered only when enabled', withCleanEnv(async () => {
  const ctx = fakeCtx()
  apply(ctx as unknown as Context, {
    apiUrl: 'http://hindsight.test',
    bankId: 'test-bank',
    timeoutMs: 1000,
    autoRecall: false,
    autoRetain: false,
    autoMentalModel: false,
  })
  const names = ctx.tools.registered.map(def => def.name)
  for (const expected of ['hindsight_list_pages', 'hindsight_search_pages', 'hindsight_read_page']) {
    assert.ok(names.includes(expected), expected + ' should be registered')
  }

  const off = fakeCtx()
  apply(off as unknown as Context, {
    apiUrl: 'http://hindsight.test',
    bankId: 'test-bank',
    timeoutMs: 1000,
    autoRecall: false,
    autoRetain: false,
    autoMentalModel: false,
    knowledgePagesEnabled: false,
  })
  const offNames = off.tools.registered.map(def => def.name)
  assert.ok(!offNames.includes('hindsight_list_pages'), 'pages tools are not registered when disabled')
  assert.ok(offNames.includes('hindsight_recall'), 'memory tools stay available')
}))

test('hindsight_list_pages returns a trimmed roster', withCleanEnv(async () => {
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
      autoRetain: false,
      autoMentalModel: false,
    })
    const tool = ctx.tools.registered.find(def => def.name === 'hindsight_list_pages')
    assert.ok(tool, 'hindsight_list_pages should be registered')

    const result = await tool!.execute({}, {} as unknown as ToolRunContext) as any
    assert.equal(result.count, 2)
    assert.deepEqual(result.pages.map((p: any) => p.name), ['Component map', 'Conventions'])
    assert.equal(result.pages[0].folder, 'wpsmain')
    assert.equal(result.pages[0].pageId, 'kp-component')
    assert.equal(result.pages[0].stale, true)
    assert.ok(result.pages[0].summary.length <= 81, 'summary is capped to one short line')
    assert.ok(result.pages[0].summary.startsWith('组件地图：'), 'summary keeps the descriptive clause')
    assert.ok(result.pages[1].summary.startsWith('编码与构建约定：'), 'the bare 产出 prefix is stripped')

    const treeCall = calls.find(call => call.url.includes('/knowledge-base/tree'))
    assert.ok(treeCall, 'expected a tree request')
    assert.ok(treeCall!.url.endsWith('/v1/default/banks/test-bank/knowledge-base/tree'))
  } finally {
    globalThis.fetch = originalFetch
  }
}))

test('hindsight_read_page resolves by name and enforces maxChars', withCleanEnv(async () => {
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
      autoRetain: false,
      autoMentalModel: false,
    })
    const tool = ctx.tools.registered.find(def => def.name === 'hindsight_read_page')
    assert.ok(tool, 'hindsight_read_page should be registered')

    const result = await tool!.execute(
      {page: 'component', maxChars: 500},
      {} as unknown as ToolRunContext,
    ) as any
    assert.equal(result.name, 'Component map')
    assert.equal(result.pageId, 'kp-component')
    assert.equal(result.folder, 'wpsmain')
    assert.equal(result.truncated, true)
    assert.ok(result.body.length <= 520, 'body respects maxChars')
    assert.ok(result.body.startsWith('# Component map'))

    // An ambiguous reference fails with candidate names instead of guessing.
    await assert.rejects(
      () => tool!.execute({page: 'nope'}, {} as unknown as ToolRunContext),
      /no page matches "nope"/,
    )
  } finally {
    globalThis.fetch = originalFetch
  }
}))

test('hindsight_capture_initiative creates one page and its marker', withCleanEnv(async () => {
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
      autoRetain: false,
      autoMentalModel: false,
    })
    const tool = ctx.tools.registered.find(def => def.name === 'hindsight_capture_initiative')
    assert.ok(tool, 'hindsight_capture_initiative should be registered')

    const result = await tool!.execute(
      {title: 'Knowledge page capture', summary: 'Add the write side so initiatives get a page.'},
      {} as unknown as ToolRunContext,
    ) as any
    assert.equal(result.ok, true)
    assert.equal(result.pageId, 'kp-new')
    assert.equal(result.created, true)
    assert.equal(result.markerDocumentId, 'initiative-marker-kp-new')

    const folderCall = calls.find(call => call.url.endsWith('/knowledge-base/folders'))
    assert.ok(folderCall, 'expected a folder create request')
    assert.deepEqual(JSON.parse(String(folderCall!.init.body)), {name: 'Initiatives'})

    const pageCall = calls.find(call => call.url.endsWith('/knowledge-base/pages'))
    assert.ok(pageCall, 'expected a page create request')
    const pageBody = JSON.parse(String(pageCall!.init.body)) as any
    assert.equal(pageBody.name, 'Knowledge page capture')
    assert.match(pageBody.source_query, /"Knowledge page capture" initiative/)
    assert.equal(pageBody.parent_id, 'kf-initiatives')
    assert.deepEqual(pageBody.tags, ['knowledge:feature-work'])
    assert.equal(pageBody.max_tokens, 4096)
    assert.equal(pageBody.trigger.mode, 'delta')
    assert.equal(pageBody.trigger.tags_match, 'all_strict')
    assert.deepEqual(pageBody.trigger.fact_types, ['world', 'experience', 'observation'])
    assert.match(pageBody.trigger.refresh_cron, /^\d+ \* \* \* \*$/)

    const markerCall = calls.find(call => call.url.endsWith('/memories'))
    assert.ok(markerCall, 'expected a marker retain request')
    const markerBody = JSON.parse(String(markerCall!.init.body)) as any
    const item = markerBody.items[0]
    assert.equal(item.document_id, 'initiative-marker-kp-new')
    assert.equal(item.context, 'initiative marker for [[page:kp-new]]')
    assert.equal(item.metadata.relatedPageId, 'kp-new')
    assert.equal(item.content, 'New initiative: Knowledge page capture. Add the write side so initiatives get a page.')
    assert.deepEqual(item.tags, ['knowledge:feature-work'])
    assert.equal(markerBody.async, true)
  } finally {
    globalThis.fetch = originalFetch
  }
}))

test('hindsight_capture_initiative reuses the folder and updates in place', withCleanEnv(async () => {
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
      autoRetain: false,
      autoMentalModel: false,
    })
    const tool = ctx.tools.registered.find(def => def.name === 'hindsight_capture_initiative')
    assert.ok(tool, 'hindsight_capture_initiative should be registered')

    await tool!.execute(
      {title: 'First plan', summary: 'Initial intent.'},
      {} as unknown as ToolRunContext,
    )
    const updated = await tool!.execute(
      {title: 'First plan', summary: 'Scope narrowed to the write side.', relates_to_page_id: 'kp-component'},
      {} as unknown as ToolRunContext,
    ) as any

    assert.equal(updated.created, false)
    assert.equal(updated.pageId, 'kp-component')
    assert.equal(updated.markerDocumentId, 'initiative-marker-kp-component-update')

    const folderPosts = calls.filter(call => call.url.endsWith('/knowledge-base/folders'))
    assert.equal(folderPosts.length, 1, 'an existing Initiatives folder is not created twice')
    const pagePosts = calls.filter(call => call.url.endsWith('/knowledge-base/pages') && call.init.method === 'POST')
    assert.equal(pagePosts.length, 1, 'an update never creates a second page')

    const markers = calls.filter(call => call.url.endsWith('/memories'))
    assert.equal(markers.length, 2)
    const updateBody = JSON.parse(String(markers[1]!.init.body)) as any
    assert.equal(updateBody.items[0].content, 'Update to an existing initiative: First plan. Scope narrowed to the write side.')
    assert.equal(updateBody.items[0].document_id, 'initiative-marker-kp-component-update')
    assert.equal(updateBody.items[0].metadata.relatedPageId, 'kp-component')
  } finally {
    globalThis.fetch = originalFetch
  }
}))

test('hindsight_capture_initiative is hidden when capture is disabled', withCleanEnv(async () => {
  const ctx = fakeCtx()
  apply(ctx as unknown as Context, {
    apiUrl: 'http://hindsight.test',
    bankId: 'test-bank',
    timeoutMs: 1000,
    autoRecall: false,
    autoRetain: false,
    autoMentalModel: false,
    knowledgePageCaptureEnabled: false,
  })
  const names = ctx.tools.registered.map(def => def.name)
  assert.ok(!names.includes('hindsight_capture_initiative'))
  assert.ok(names.includes('hindsight_read_page'), 'read tools stay available')
}))

test('expandCronHash expands H into a concrete five-field cron', () => {
  const cron = expandCronHash('H * * * *', 'test-bank\0Initiatives')
  assert.match(cron, /^\d+ \* \* \* \*$/)
  const minute = Number(cron.split(' ')[0])
  assert.ok(minute >= 0 && minute <= 59, 'the minute is inside the field range')
  assert.equal(expandCronHash('H * * * *', 'same seed'), expandCronHash('H * * * *', 'same seed'))
  assert.equal(expandCronHash('15 * * * *', 'seed'), '15 * * * *')
  const ranged = Number(expandCronHash('H(10-20) * * * *', 'seed').split(' ')[0])
  assert.ok(ranged >= 10 && ranged <= 20, 'H(lo-hi) stays inside the range')
  assert.throws(() => expandCronHash('H * * *', 'seed'), /invalid cron template/)
  assert.throws(() => expandCronHash('H * * * * *', 'seed'), /invalid cron template/)
})

test('resolveConfig exposes initiative capture defaults and aliases', withCleanEnv(async () => {
  const c = resolveConfig({}, {})
  assert.equal(c.knowledgePageCaptureEnabled, true)
  assert.equal(c.initiativeFolder, 'Initiatives')
  assert.equal(c.initiativeTierTag, 'knowledge:feature-work')
  assert.equal(c.initiativePageMaxTokens, 4096)
  assert.equal(c.initiativeRefreshCron, 'H * * * *')
  assert.deepEqual(c.initiativeTags, [])

  const aliased = resolveConfig({
    knowledge_page_capture_enabled: false,
    initiative_folder: 'Feature work',
    initiative_page_max_tokens: 2048,
    initiative_tags: ['team:agent'],
  }, {})
  assert.equal(aliased.knowledgePageCaptureEnabled, false)
  assert.equal(aliased.initiativeFolder, 'Feature work')
  assert.equal(aliased.initiativePageMaxTokens, 2048)
  assert.deepEqual(aliased.initiativeTags, ['team:agent'])

  process.env.HINDSIGHT_INITIATIVE_CAPTURE = 'off'
  process.env.HINDSIGHT_INITIATIVE_FOLDER = 'Work'
  const fromEnv = resolveConfig({}, process.env)
  assert.equal(fromEnv.knowledgePageCaptureEnabled, false)
  assert.equal(fromEnv.initiativeFolder, 'Work')
}))

test('hindsight_search_pages returns compact hits', withCleanEnv(async () => {
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
      autoRetain: false,
      autoMentalModel: false,
      pageSearchLimit: 2,
    })
    const tool = ctx.tools.registered.find(def => def.name === 'hindsight_search_pages')
    assert.ok(tool)

    const result = await tool!.execute({query: '模块'}, {} as unknown as ToolRunContext) as any
    assert.equal(result.count, 1)
    assert.equal(result.results[0].name, 'Component map')
    assert.ok(result.results[0].snippet.includes('kso'))

    const searchCall = calls.find(call => call.url.includes('/knowledge-base/search'))
    assert.ok(searchCall, 'expected a search request')
    assert.ok(searchCall!.url.includes('q='))
    assert.ok(searchCall!.url.includes('limit=2'))
  } finally {
    globalThis.fetch = originalFetch
  }
}))

test('pre-step injects the knowledge-page roster once per window', withCleanEnv(async () => {
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
      autoRetain: false,
      autoMentalModel: false,
      knowledgePagesEnabled: true,
      pageRosterRefreshEveryTurns: 10,
    })
    const preStepListener = ctx.listeners.get('agent/pre-step')?.[0] as PreStepListener | undefined
    assert.ok(preStepListener)

    const injected: any[] = []
    const agent = {
      id: 'agent-pages',
      session: { header: { cwd: 'D:\\repo\\app', origin: 'user' } },
      inject: (msg: any) => injected.push(msg),
    }
    const step = (turn: number) => preStepListener!(
      {agent, messages: [{source: {kind: 'user'}, content: [{type: 'text', text: 'hi'}]}], turn, step: 1},
      async () => ({kind: 'enter', messages: [{role: 'system', content: 'ctx'}]}),
    )
    const rosterTexts = () => injected
      .map((m: any) => String(m.content?.[0]?.text ?? ''))
      .filter(text => text.includes('hindsight-knowledge-pages'))

    await step(1)
    await waitFor(() => rosterTexts().length >= 1)
    assert.ok(rosterTexts()[0]!.includes('Component map'), 'roster lists page names')
    assert.ok(rosterTexts()[0]!.includes('wpsmain'), 'roster lists folders')
    assert.ok(rosterTexts()[0]!.includes('组件地图：'), 'roster uses the one-line summary')

    // Same window: no second roster.
    await step(5)
    await new Promise(resolve => setTimeout(resolve, 150))
    assert.equal(rosterTexts().length, 1, 'roster is not re-injected inside the window')

    // Next window: refreshed.
    await step(11)
    await waitFor(() => rosterTexts().length >= 2)
  } finally {
    globalThis.fetch = originalFetch
  }
}))

test('autoRecallOnce injects recall only once per agent', withCleanEnv(async () => {
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
      autoRecallOnce: true,
      autoRetain: false,
      autoMentalModel: false,
      knowledgePagesEnabled: false,
      recallTimeoutMs: 2000,
    })
    const preStepListener = ctx.listeners.get('agent/pre-step')?.[0] as PreStepListener | undefined
    assert.ok(preStepListener)

    const injected: any[] = []
    const agent = {id: 'agent-once', inject: (msg: any) => injected.push(msg)}
    const step = (turn: number) => preStepListener!(
      {agent, messages: [{source: {kind: 'user'}, content: [{type: 'text', text: 'q'}]}], turn, step: 1},
      async () => ({kind: 'enter', messages: [{role: 'system', content: 'ctx'}]}),
    )
    const recallTexts = () => injected
      .map((m: any) => String(m.content?.[0]?.text ?? ''))
      .filter(text => text.includes('hindsight-recall'))

    await step(1)
    await waitFor(() => recallTexts().length >= 1)
    await step(2)
    await new Promise(resolve => setTimeout(resolve, 200))

    assert.equal(calls.filter(call => call.url.endsWith('/memories/recall')).length, 1, 'recall is not re-run every turn')
    assert.equal(recallTexts().length, 1, 'recall is injected once per session')
  } finally {
    globalThis.fetch = originalFetch
  }
}))


