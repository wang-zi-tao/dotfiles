import test from 'node:test';
import assert from 'node:assert/strict';
import { HindsightClient, HindsightError } from '../src/client.js';
function jsonFetch(response) {
    return (async () => ({
        ok: response.ok,
        status: response.status,
        statusText: response.statusText,
        text: async () => JSON.stringify(response.data),
    }));
}
test('retain posts to the bank memories endpoint with bearer auth', async () => {
    const calls = [];
    const client = new HindsightClient({
        apiUrl: 'http://127.0.0.1:8888/',
        apiKey: 'secret',
        fetchImpl: async (url, init) => {
            calls.push({ url: String(url), init: init ?? {} });
            return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({ success: true, items_count: 1 }) };
        },
    });
    const response = await client.retain({
        bankId: 'bank/one',
        items: [{ content: 'hello' }],
        retainAsync: true,
    });
    assert.equal(calls.length, 1);
    assert.equal(calls[0]?.url, 'http://127.0.0.1:8888/v1/default/banks/bank%2Fone/memories');
    assert.equal((calls[0]?.init.headers).Authorization, 'Bearer secret');
    assert.deepEqual(JSON.parse(String(calls[0]?.init.body)), { items: [{ content: 'hello' }], async: true });
    assert.equal(response.items_count, 1);
});
test('recall maps server body', async () => {
    const client = new HindsightClient({
        apiUrl: 'http://hindsight.test',
        fetchImpl: jsonFetch({ ok: true, status: 200, statusText: '', data: { results: [{ id: '1', text: 'Memory 1', type: 'observation' }] } }),
    });
    const response = await client.recall({ bankId: 'b', query: 'q', budget: 'low', tags: ['x'] });
    assert.equal(response.results?.[0]?.text, 'Memory 1');
});
test('non-2xx responses throw HindsightError with status', async () => {
    const client = new HindsightClient({
        apiUrl: 'http://hindsight.test',
        fetchImpl: jsonFetch({ ok: false, status: 401, statusText: '', data: { detail: 'bad key' } }),
    });
    await assert.rejects(() => client.version(), (error) => error instanceof HindsightError && error.status === 401 && /bad key/.test(error.message));
});
test('operationStatus targets the operations endpoint', async () => {
    const calls = [];
    const client = new HindsightClient({
        apiUrl: 'http://hindsight.test',
        fetchImpl: async (url) => {
            calls.push(String(url));
            return { ok: true, status: 200, statusText: '', text: async () => JSON.stringify({ status: 'completed' }) };
        },
    });
    const response = await client.operationStatus('b', 'op-1');
    assert.equal(calls[0], 'http://hindsight.test/v1/default/banks/b/operations/op-1');
    assert.equal(response.status, 'completed');
});
