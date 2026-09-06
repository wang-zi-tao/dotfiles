import test from 'node:test';
import assert from 'node:assert/strict';
import { apply, inject, name } from '../src/index.js';
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
];
function withCleanEnv(run) {
    return async () => {
        const saved = new Map(envKeys.map(key => [key, process.env[key]]));
        for (const key of envKeys)
            delete process.env[key];
        try {
            await run();
        }
        finally {
            for (const [key, value] of saved) {
                if (value === undefined)
                    delete process.env[key];
                else
                    process.env[key] = value;
            }
        }
    };
}
function fakeCtx() {
    const ctx = {
        tools: {
            registered: [],
            register(definition) {
                this.registered.push(definition);
                return () => { };
            },
        },
        systemPrompt: {
            sections: [],
            contexts: [],
            section(section) { this.sections.push(section); return () => { }; },
            context(context) { this.contexts.push(context); return () => { }; },
        },
        listeners: new Map(),
        on(event, listener) {
            const list = this.listeners.get(event) ?? [];
            list.push(listener);
            this.listeners.set(event, list);
            return () => { };
        },
    };
    return ctx;
}
function turnEvent(turn, reason = 'completed') {
    return { type: 'turn/end', seq: 4, time: 4, data: { turn, reason: { kind: reason } } };
}
function textMessage(turn, text) {
    return {
        type: 'user/message', seq: 1, time: 1,
        data: { turn, source: { kind: 'user' }, content: [{ type: 'text', text }] },
    };
}
function assistantMessage(turn, text) {
    return {
        type: 'assistant/message', seq: 2, time: 2,
        data: { turn, message: { content: [{ type: 'text', text }] } },
    };
}
function makeSession(id, events, header = {}) {
    return { id, events, header };
}
function fakeFetch(calls) {
    return (async (url, init = {}) => {
        const target = String(url);
        calls.push({ url: target, init });
        const body = init.body ? JSON.parse(String(init.body)) : undefined;
        if (target.endsWith('/memories')) {
            return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({ success: true, items_count: body?.items?.length ?? 1 }) };
        }
        if (target.endsWith('/memories/recall')) {
            return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({ results: [{ id: 'm1', text: 'Memory 1', type: 'observation' }] }) };
        }
        if (target.endsWith('/reflect')) {
            return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({ text: 'Reflected answer' }) };
        }
        if (target.endsWith('/version')) {
            return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({ version: '0.6.1' }) };
        }
        return { ok: false, status: 404, statusText: 'not found', text: async () => JSON.stringify({ detail: 'not found' }) };
    });
}
async function waitFor(predicate, timeoutMs = 1000) {
    const deadline = Date.now() + timeoutMs;
    while (!predicate()) {
        if (Date.now() > deadline)
            throw new Error('timed out waiting for condition');
        await new Promise(resolve => setTimeout(resolve, 5));
    }
}
test('plugin registers hooks, tools, and prefetches recall on turn/end', withCleanEnv(async () => {
    const ctx = fakeCtx();
    const calls = [];
    const originalFetch = globalThis.fetch;
    globalThis.fetch = fakeFetch(calls);
    try {
        apply(ctx, {
            apiUrl: 'http://hindsight.test',
            bankId: 'test-bank',
            timeoutMs: 1000,
            autoRetain: true,
            retainEveryNTurns: 1,
            autoRecall: true,
            retainAsync: false,
        });
        assert.equal(name, 'dsh-hindsight');
        assert.deepEqual(inject, ['tools', 'systemPrompt']);
        assert.deepEqual(ctx.tools.registered.map(def => def.name), [
            'hindsight_retain',
            'hindsight_recall',
            'hindsight_reflect',
            'hindsight_status',
        ]);
        const session = makeSession('sess-1', [
            textMessage(1, 'Hello, remember my favorite color is blue'),
            assistantMessage(1, 'I will remember that.'),
            turnEvent(1),
        ]);
        const onSessionEvent = ctx.listeners.get('session/event')?.[0];
        assert.ok(onSessionEvent);
        onSessionEvent(session, session.events[2]);
        await waitFor(() => calls.length >= 2);
        const retainCall = calls.find(call => call.url.endsWith('/memories'));
        const recallCall = calls.find(call => call.url.endsWith('/memories/recall'));
        assert.ok(retainCall, 'expected a retain request');
        assert.ok(recallCall, 'expected a recall request');
        assert.equal(retainCall.url, 'http://hindsight.test/v1/default/banks/test-bank/memories');
        assert.equal(recallCall.url, 'http://hindsight.test/v1/default/banks/test-bank/memories/recall');
        const retainBody = JSON.parse(String(retainCall.init.body));
        assert.equal(retainBody.async, false);
        assert.match(retainBody.items[0]?.content ?? '', /User: Hello, remember my favorite color is blue/);
        assert.match(retainBody.items[0]?.content ?? '', /Assistant: I will remember that\./);
        const recallContext = ctx.systemPrompt.contexts.find(context => context.name === 'hindsight:recall');
        assert.ok(recallContext);
        const injected = typeof recallContext.text === 'function' ? recallContext.text({ agent: { session } }) : '';
        assert.match(injected, /Hindsight Memory/);
        assert.match(injected, /Memory 1/);
        const statusTool = ctx.tools.registered.find(def => def.name === 'hindsight_status');
        assert.ok(statusTool);
        const status = await statusTool.execute({}, {});
        assert.equal(status.ok, true);
        assert.equal(status.bankId, 'test-bank');
    }
    finally {
        globalThis.fetch = originalFetch;
    }
}));
test('memoryMode=context hides tools but still injects auto-recall', withCleanEnv(async () => {
    const ctx = fakeCtx();
    const calls = [];
    const originalFetch = globalThis.fetch;
    globalThis.fetch = fakeFetch(calls);
    try {
        apply(ctx, { apiUrl: 'http://hindsight.test', memoryMode: 'context', timeoutMs: 1000 });
        assert.deepEqual(ctx.tools.registered, []);
        assert.equal(ctx.systemPrompt.sections.length, 0);
        const session = makeSession('sess-2', [
            textMessage(1, 'context-mode turn'),
            assistantMessage(1, 'done'),
            turnEvent(1),
        ]);
        const onSessionEvent = ctx.listeners.get('session/event')?.[0];
        assert.ok(onSessionEvent);
        onSessionEvent(session, session.events[2]);
        await waitFor(() => calls.some(call => call.url.endsWith('/memories')));
        const recallContext = ctx.systemPrompt.contexts.find(context => context.name === 'hindsight:recall');
        assert.ok(recallContext);
        const injected = typeof recallContext.text === 'function' ? recallContext.text({ agent: { session } }) : '';
        assert.match(injected, /Memory 1/);
    }
    finally {
        globalThis.fetch = originalFetch;
    }
}));
