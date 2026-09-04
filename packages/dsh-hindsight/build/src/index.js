/**
 * dsh-hindsight — Hindsight long-term memory for DeepSeek Harness.
 *
 * Host-side Cordis plugin. The memory bank is shared across sessions, so the
 * row belongs in the host composition (the profile bundle), not in one agent
 * preset:
 *
 *   - `ctx.tools.register` exposes hindsight_retain / hindsight_recall /
 *     hindsight_reflect / hindsight_status.
 *   - `ctx.systemPrompt.context` injects the background recall result as
 *     ordered dynamic context (the next-turn prefetch model used by the
 *     Hermes Hindsight provider).
 *   - `session/event` (`turn/end`) drives automatic per-turn retention and
 *     primes the next-turn recall.
 *   - `session/disposed` flushes any buffered turns and drops session state.
 *
 * The package is dependency-free. It talks to the Hindsight HTTP API directly
 * (`/v1/default/banks/{bank}/memories`, `.../memories/recall`, `/reflect`).
 * @module dsh-hindsight
 */
import { DEFAULTS, resolveConfig } from './config.js';
import { HindsightClient } from './client.js';
import { buildTurnRecord } from './transcript.js';
export const name = 'dsh-hindsight';
export const inject = ['tools', 'systemPrompt'];
export { DEFAULTS, resolveConfig };
export { HindsightClient } from './client.js';
export { buildTurnRecord } from './transcript.js';
const TOOL_SECTION_ORDER = 135;
const RECALL_PREAMBLE = [
    '# Hindsight Memory (persistent cross-session context)',
    'Use this to answer questions about the user and prior sessions. Do not call tools to look up information that is already present here.',
].join('\n');
function makeLogger(ctx) {
    try {
        if (typeof ctx?.logger === 'function')
            return ctx.logger('hindsight');
        if (ctx?.logger)
            return ctx.logger;
    }
    catch {
        // fall through
    }
    return {
        name: 'hindsight',
        debug() { },
        info() { },
        warn() { },
        error() { },
    };
}
function logFailure(logger, action, error) {
    const message = error instanceof Error ? error.message : String(error);
    try {
        logger.warn(`${action} failed: ${message}`);
    }
    catch {
        // logging must never break a turn
    }
}
function mergeTags(...groups) {
    const seen = new Set();
    const out = [];
    for (const group of groups) {
        for (const raw of group ?? []) {
            const tag = String(raw ?? '').trim();
            if (tag && !seen.has(tag)) {
                seen.add(tag);
                out.push(tag);
            }
        }
    }
    return out;
}
function requireString(args, key) {
    const value = args?.[key];
    if (typeof value !== 'string' || value.trim() === '') {
        throw new Error(`Missing required string parameter: ${key}`);
    }
    return value;
}
function optionalString(args, key) {
    const value = args?.[key];
    if (value === undefined || value === null)
        return undefined;
    return String(value);
}
function optionalInteger(args, key, fallback, minimum = 1) {
    const value = args?.[key];
    if (value === undefined || value === null || value === '')
        return fallback;
    const parsed = Math.trunc(Number(value));
    if (!Number.isFinite(parsed) || parsed < minimum)
        return fallback;
    return parsed;
}
function optionalBudget(args, fallback) {
    const value = optionalString(args, 'budget') ?? fallback;
    if (!['low', 'mid', 'high'].includes(value)) {
        throw new Error(`Invalid budget ${JSON.stringify(value)}; expected low, mid or high`);
    }
    return value;
}
function optionalTags(args, fallback) {
    const value = args?.tags ?? fallback;
    if (value === undefined || value === null)
        return [];
    return mergeTags(Array.isArray(value) ? value : String(value).split(','));
}
function optionalTagsMatch(args, fallback) {
    const value = optionalString(args, 'tagsMatch') ?? fallback;
    if (!['any', 'all', 'any_strict', 'all_strict'].includes(value)) {
        throw new Error(`Invalid tagsMatch ${JSON.stringify(value)}`);
    }
    return value;
}
function optionalTypes(args, fallback) {
    const value = args?.types ?? fallback;
    if (value === undefined || value === null || value === '')
        return [];
    const list = Array.isArray(value) ? value : String(value).split(',');
    return list.map(item => String(item).trim()).filter(Boolean);
}
function agentMetadata(exec) {
    const metadata = { source: 'dsh-hindsight' };
    const session = exec?.agent?.session;
    if (session?.id)
        metadata.sessionId = String(session.id);
    if (session?.header?.cwd)
        metadata.cwd = String(session.header.cwd);
    return metadata;
}
function retainItem(record, session, config) {
    const item = {
        content: record.text,
        timestamp: record.startedAt,
        context: config.retainContext,
        metadata: {
            source: 'dsh-hindsight',
            sessionId: String(session.id),
            turn: String(record.turn),
            messageCount: String(record.messages.length),
            retainedAt: new Date().toISOString(),
            ...(session.header?.cwd ? { cwd: String(session.header.cwd) } : {}),
        },
        tags: mergeTags(config.retainTags, [`session:${session.id}`]),
    };
    if (config.retainDocumentId) {
        item.document_id = config.retainDocumentId;
        if (config.retainUpdateMode)
            item.update_mode = config.retainUpdateMode;
    }
    return item;
}
function recallResultsToValue(results) {
    const list = [];
    for (const result of results ?? []) {
        const item = {
            id: String(result.id ?? ''),
            text: String(result.text ?? ''),
        };
        if (typeof result.type === 'string')
            item.type = result.type;
        if (Array.isArray(result.tags))
            item.tags = result.tags.filter(tag => typeof tag === 'string');
        list.push(item);
    }
    return list;
}
function recallValueToText(value) {
    const record = (value ?? {});
    const lines = [String(record?.message ?? '')];
    for (const result of record?.results ?? [])
        lines.push(`- ${String(result?.text ?? '')}`);
    return lines.filter(Boolean).join('\n');
}
function operationIdsFrom(response) {
    const ids = [];
    if (response.operation_id)
        ids.push(String(response.operation_id));
    if (Array.isArray(response.operation_ids))
        ids.push(...response.operation_ids.map(String));
    return [...new Set(ids.filter(Boolean))];
}
function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}
async function waitForRetainOperations(client, config, response) {
    if (!config.retainAsync || !config.retainWaitForOperations)
        return;
    const operationIds = operationIdsFrom(response);
    if (operationIds.length === 0)
        return;
    const deadline = Date.now() + config.retainDrainTimeoutMs;
    const pending = new Set(operationIds);
    while (pending.size > 0 && Date.now() < deadline) {
        for (const operationId of [...pending]) {
            try {
                const status = await client.operationStatus(config.bankId, operationId, { timeoutMs: config.timeoutMs });
                if (status.status === 'completed')
                    pending.delete(operationId);
                else if (status.status === 'failed')
                    throw new Error(`Hindsight retain operation ${operationId} failed`);
            }
            catch (error) {
                // Older or minimal Hindsight deployments may not expose the
                // operations endpoint. The retain was already accepted; fail open.
                if (error instanceof Error && /404/.test(error.message)) {
                    pending.delete(operationId);
                    continue;
                }
                throw error;
            }
        }
        if (pending.size > 0 && Date.now() < deadline) {
            await sleep(config.retainOperationPollIntervalMs);
        }
    }
}
async function retainAndWait(client, config, options) {
    const response = await client.retain({
        ...options,
        retainAsync: config.retainAsync,
        timeoutMs: config.timeoutMs,
    });
    await waitForRetainOperations(client, config, response);
    return response;
}
function toolDefinitions(client, config) {
    const definitions = [];
    definitions.push({
        name: 'hindsight_retain',
        description: 'Store information to long-term memory. Hindsight automatically extracts structured facts, resolves entities, and indexes the content for later semantic retrieval.',
        parameters: {
            type: 'object',
            additionalProperties: false,
            properties: {
                content: { type: 'string', description: 'The information to store.' },
                context: { type: 'string', description: "Short label, e.g. 'user preference' or 'project decision'." },
                tags: { type: 'array', items: { type: 'string' }, description: 'Optional tags merged with configured default retain tags.' },
                documentId: { type: 'string', description: 'Optional document id; overrides the configured retainDocumentId for this call.' },
                updateMode: { type: 'string', enum: ['append', 'replace'], description: 'Optional update mode when documentId is used.' },
            },
            required: ['content'],
        },
        output: {
            schema: {
                type: 'object',
                additionalProperties: false,
                properties: {
                    ok: { type: 'boolean' },
                    message: { type: 'string' },
                    bankId: { type: 'string' },
                    itemsCount: { type: 'integer' },
                    operationIds: { type: 'array', items: { type: 'string' } },
                },
                required: ['ok', 'message', 'bankId', 'itemsCount'],
            },
            render: (_args, value) => [{ type: 'text', text: String(value?.message ?? '') }],
        },
        async execute(args, exec) {
            const content = requireString(args, 'content');
            const context = optionalString(args, 'context');
            const tags = mergeTags(config.retainTags, optionalTags(args, []));
            const documentId = optionalString(args, 'documentId') ?? config.retainDocumentId;
            const updateMode = optionalString(args, 'updateMode') ?? (config.retainUpdateMode ?? undefined);
            const item = { content, context, metadata: agentMetadata(exec), tags };
            if (documentId) {
                item.document_id = documentId;
                if (updateMode)
                    item.update_mode = updateMode;
            }
            try {
                const response = await retainAndWait(client, config, {
                    bankId: config.bankId,
                    items: [item],
                    signal: exec?.signal,
                });
                const value = {
                    ok: response.success !== false,
                    message: `Stored 1 memory in Hindsight bank '${config.bankId}'.`,
                    bankId: config.bankId,
                    itemsCount: Number(response.items_count ?? 1),
                };
                const operationIds = operationIdsFrom(response);
                if (operationIds.length)
                    value.operationIds = operationIds;
                return value;
            }
            catch (error) {
                throw new Error(`hindsight_retain failed: ${error instanceof Error ? error.message : error}`, { cause: error });
            }
        },
    });
    definitions.push({
        name: 'hindsight_recall',
        description: 'Search long-term memory. Returns memories ranked by relevance using semantic search, keyword matching, entity-graph traversal, and reranking.',
        parameters: {
            type: 'object',
            additionalProperties: false,
            properties: {
                query: { type: 'string', description: 'What to search for.' },
                budget: { type: 'string', enum: ['low', 'mid', 'high'], description: 'Recall thoroughness override.' },
                maxTokens: { type: 'integer', description: 'Maximum tokens in returned results.' },
                types: { type: 'array', items: { type: 'string' }, description: 'Fact types: world, experience, opinion, observation.' },
                tags: { type: 'array', items: { type: 'string' }, description: 'Tags to filter by.' },
                tagsMatch: { type: 'string', enum: ['any', 'all', 'any_strict', 'all_strict'], description: 'Tag matching mode.' },
            },
            required: ['query'],
        },
        output: {
            schema: {
                type: 'object',
                additionalProperties: false,
                properties: {
                    ok: { type: 'boolean' },
                    message: { type: 'string' },
                    bankId: { type: 'string' },
                    count: { type: 'integer' },
                    results: {
                        type: 'array',
                        items: {
                            type: 'object',
                            additionalProperties: false,
                            properties: {
                                id: { type: 'string' },
                                text: { type: 'string' },
                                type: { type: 'string' },
                                tags: { type: 'array', items: { type: 'string' } },
                            },
                            required: ['id', 'text'],
                        },
                    },
                },
                required: ['ok', 'message', 'bankId', 'count', 'results'],
            },
            render: (_args, value) => [{ type: 'text', text: recallValueToText(value) }],
        },
        async execute(args, exec) {
            const query = requireString(args, 'query');
            const budget = optionalBudget(args, config.budget);
            const maxTokens = optionalInteger(args, 'maxTokens', config.recallMaxTokens);
            const types = optionalTypes(args, config.recallTypes);
            const tags = optionalTags(args, config.recallTags);
            const tagsMatch = optionalTagsMatch(args, config.recallTagsMatch);
            try {
                const response = await client.recall({
                    bankId: config.bankId,
                    query,
                    budget,
                    maxTokens,
                    types,
                    tags,
                    tagsMatch,
                    signal: exec?.signal,
                    timeoutMs: config.timeoutMs,
                });
                const results = recallResultsToValue(response.results);
                return {
                    ok: true,
                    message: results.length
                        ? `Recalled ${results.length} memories from Hindsight bank '${config.bankId}'.`
                        : `No relevant memories found in Hindsight bank '${config.bankId}'.`,
                    bankId: config.bankId,
                    count: results.length,
                    results,
                };
            }
            catch (error) {
                throw new Error(`hindsight_recall failed: ${error instanceof Error ? error.message : error}`, { cause: error });
            }
        },
    });
    definitions.push({
        name: 'hindsight_reflect',
        description: 'Synthesize a reasoned answer from long-term memories. Unlike recall, this reasons across all stored memories to produce a coherent response.',
        parameters: {
            type: 'object',
            additionalProperties: false,
            properties: {
                query: { type: 'string', description: 'The question to reflect on.' },
                budget: { type: 'string', enum: ['low', 'mid', 'high'], description: 'Reflection thoroughness override.' },
                context: { type: 'string', description: 'Optional extra context for the reflection.' },
                maxTokens: { type: 'integer', description: 'Maximum tokens for the answer.' },
                tags: { type: 'array', items: { type: 'string' }, description: 'Tags to filter memories by.' },
                tagsMatch: { type: 'string', enum: ['any', 'all', 'any_strict', 'all_strict'], description: 'Tag matching mode.' },
            },
            required: ['query'],
        },
        output: {
            schema: {
                type: 'object',
                additionalProperties: false,
                properties: {
                    ok: { type: 'boolean' },
                    message: { type: 'string' },
                    text: { type: 'string' },
                },
                required: ['ok', 'message', 'text'],
            },
            render: (_args, value) => [{ type: 'text', text: String(value?.text ?? '') }],
        },
        async execute(args, exec) {
            const query = requireString(args, 'query');
            const budget = optionalBudget(args, config.budget);
            const context = optionalString(args, 'context');
            const maxTokens = optionalInteger(args, 'maxTokens', undefined);
            const tags = optionalTags(args, config.recallTags);
            const tagsMatch = optionalTagsMatch(args, config.recallTagsMatch);
            try {
                const response = await client.reflect({
                    bankId: config.bankId,
                    query,
                    budget,
                    context,
                    maxTokens,
                    tags,
                    tagsMatch,
                    signal: exec?.signal,
                    timeoutMs: config.timeoutMs,
                });
                const text = String(response.text ?? 'No relevant memories found.');
                return { ok: true, message: `Hindsight reflection completed for bank '${config.bankId}'.`, text };
            }
            catch (error) {
                throw new Error(`hindsight_reflect failed: ${error instanceof Error ? error.message : error}`, { cause: error });
            }
        },
    });
    if (config.statusToolEnabled) {
        definitions.push({
            name: 'hindsight_status',
            description: 'Check the Hindsight server connection, API version, and the memory bank currently in use.',
            parameters: {
                type: 'object',
                additionalProperties: false,
                properties: {},
            },
            output: {
                schema: {
                    type: 'object',
                    additionalProperties: false,
                    properties: {
                        ok: { type: 'boolean' },
                        message: { type: 'string' },
                        apiUrl: { type: 'string' },
                        bankId: { type: 'string' },
                        version: { type: 'string' },
                    },
                    required: ['ok', 'message', 'apiUrl', 'bankId', 'version'],
                },
                render: (_args, value) => [{ type: 'text', text: String(value?.message ?? '') }],
            },
            async execute(_args, exec) {
                try {
                    const response = await client.version({ signal: exec?.signal });
                    const version = String(response.version ?? 'unknown');
                    return {
                        ok: true,
                        message: `Hindsight reachable at ${config.apiUrl}; bank '${config.bankId}'; API version ${version}.`,
                        apiUrl: config.apiUrl,
                        bankId: config.bankId,
                        version,
                    };
                }
                catch (error) {
                    throw new Error(`hindsight_status failed: ${error instanceof Error ? error.message : error}`, { cause: error });
                }
            },
        });
    }
    return definitions;
}
/**
 * The plugin entrypoint.
 */
export function apply(ctx, rawConfig = {}) {
    const config = resolveConfig(rawConfig);
    const logger = makeLogger(ctx);
    const client = new HindsightClient({
        apiUrl: config.apiUrl,
        apiKey: config.apiKey,
        timeoutMs: config.timeoutMs,
    });
    // sessionId -> { buffer: turn records, chain: Promise }
    const states = new Map();
    const recallCache = new Map();
    const disposedSessions = new Set();
    function stateFor(sessionId) {
        let state = states.get(sessionId);
        if (!state) {
            state = { buffer: [], chain: Promise.resolve() };
            states.set(sessionId, state);
        }
        return state;
    }
    async function flushRetain(session, state) {
        const records = state.buffer.slice();
        if (records.length === 0)
            return;
        const items = records.map(record => retainItem(record, session, config));
        await retainAndWait(client, config, { bankId: config.bankId, items });
        state.buffer.splice(0, records.length);
        logger.debug(`hindsight retained ${items.length} turn(s) for session ${session.id}`);
    }
    async function refreshRecall(session, query) {
        if (!config.autoRecall || config.memoryMode === 'tools') {
            recallCache.delete(String(session.id));
            return;
        }
        if (!query) {
            recallCache.delete(String(session.id));
            return;
        }
        try {
            if (config.recallPrefetch === 'reflect') {
                const response = await client.reflect({
                    bankId: config.bankId,
                    query,
                    budget: config.budget,
                    maxTokens: config.recallMaxTokens,
                    tags: config.recallTags,
                    tagsMatch: config.recallTagsMatch,
                    timeoutMs: config.timeoutMs,
                });
                recallCache.set(String(session.id), { text: String(response.text ?? '').trim(), count: 0 });
            }
            else {
                const response = await client.recall({
                    bankId: config.bankId,
                    query,
                    budget: config.budget,
                    maxTokens: config.recallMaxTokens,
                    types: config.recallTypes,
                    tags: config.recallTags,
                    tagsMatch: config.recallTagsMatch,
                    timeoutMs: config.timeoutMs,
                });
                const results = Array.isArray(response.results) ? response.results : [];
                const text = results
                    .map(result => (result && typeof result.text === 'string' ? result.text.trim() : ''))
                    .filter(Boolean)
                    .map(result => `- ${result}`)
                    .join('\n');
                recallCache.set(String(session.id), { text, count: results.length });
            }
        }
        catch (error) {
            recallCache.delete(String(session.id));
            logFailure(logger, 'hindsight recall prefetch', error);
        }
    }
    function handleCompletedTurn(session, event) {
        if (event?.type !== 'turn/end')
            return;
        const turn = event.data.turn;
        const reasonKind = event.data.reason.kind;
        if (!config.retainTurnKinds.includes(reasonKind))
            return;
        if (config.skipSubagents && session?.header?.origin === 'subagent')
            return;
        if (disposedSessions.has(String(session.id)))
            return;
        const record = buildTurnRecord(session, turn, config);
        if (!record)
            return;
        const state = stateFor(String(session.id));
        state.chain = state.chain
            .then(async () => {
            if (config.autoRetain) {
                state.buffer.push(record);
                if (state.buffer.length >= config.retainEveryNTurns) {
                    try {
                        await flushRetain(session, state);
                    }
                    catch (error) {
                        // Keep the buffer; the next completed turn retries the batch.
                        logFailure(logger, `hindsight retain for session ${session.id}`, error);
                    }
                }
            }
            if (record.query)
                await refreshRecall(session, record.query);
        })
            .catch(error => logFailure(logger, `hindsight turn hook for session ${session.id}`, error));
    }
    function disposeSession(session) {
        disposedSessions.add(String(session.id));
        recallCache.delete(String(session.id));
        const state = states.get(String(session.id));
        states.delete(String(session.id));
        if (!state || state.buffer.length === 0 || !config.autoRetain)
            return;
        // Best-effort final flush. It runs off the reply/teardown path.
        state.chain = state.chain
            .then(() => flushRetain(session, state))
            .catch(error => logFailure(logger, `hindsight final flush for session ${session.id}`, error));
    }
    ctx.on('session/event', (session, event) => {
        if (event?.type === 'turn/end') {
            try {
                handleCompletedTurn(session, event);
            }
            catch (error) {
                logFailure(logger, `hindsight session event for ${session?.id}`, error);
            }
        }
    }, { global: true });
    ctx.on('session/disposed', (session) => {
        try {
            disposeSession(session);
        }
        catch (error) {
            logFailure(logger, 'hindsight session disposal', error);
        }
    }, { global: true });
    // Static prompt section. Order 135 sits inside the 100–199 per-tool
    // guidance band, after the tool-catalog guidance and before later tools.
    if (config.memoryMode !== 'context') {
        ctx.systemPrompt.section({
            name: 'hindsight:memory',
            order: TOOL_SECTION_ORDER,
            text: [
                '# Hindsight Memory',
                `Active. Bank: ${config.bankId}; recall budget: ${config.budget}.`,
                ...(config.autoRecall ? ['Relevant memories are automatically injected as runtime context before a turn.'] : []),
                'Use hindsight_recall to search memories, hindsight_reflect to synthesize an answer across memories, and hindsight_retain to store information.',
                ...(config.statusToolEnabled ? ['Use hindsight_status to verify the server connection and active bank.'] : []),
            ].join('\n'),
        });
    }
    // Ordered dynamic context. The provider must be synchronous; background
    // recall primed at `turn/end` is the cache it reads.
    ctx.systemPrompt.context({
        name: 'hindsight:recall',
        order: config.recallOrder,
        text: (assemblyContext) => {
            if (config.memoryMode === 'tools')
                return '';
            if (!config.autoRecall)
                return '';
            const session = assemblyContext?.agent?.session;
            if (!session)
                return '';
            if (config.skipSubagents && session.header?.origin === 'subagent')
                return '';
            const cached = recallCache.get(String(session.id));
            if (!cached?.text)
                return '';
            const preamble = config.recallPromptPreamble || RECALL_PREAMBLE;
            return `${preamble}\n\n${cached.text}`;
        },
    });
    // Model-facing tools are intentionally global (shared bank). `memoryMode:
    // context` hides them entirely; `tools` keeps tools but disables auto-recall.
    if (config.memoryMode !== 'context') {
        for (const definition of toolDefinitions(client, config)) {
            ctx.tools.register(definition);
        }
    }
}
