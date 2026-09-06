/**
 * Configuration surface for dsh-hindsight.
 *
 * Precedence (highest wins):
 *   1. HINDSIGHT_* environment variables
 *   2. the Cordis row `config` (what `cordis.patch.yml` supplied)
 *   3. an optional JSON config file (`configFile` or `HINDSIGHT_CONFIG`)
 *   4. built-in defaults
 *
 * The plugin deliberately accepts both camelCase keys (used by the dsh row
 * config) and snake_case keys (used by the Hermes Hindsight config.json) so an
 * existing Hindsight config can be pointed at with `configFile`.
 */
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
export const DEFAULT_API_URL = 'https://api.hindsight.vectorize.io';
export const DEFAULTS = Object.freeze({
    apiUrl: DEFAULT_API_URL,
    apiKey: null,
    bankId: 'dsh',
    budget: 'mid',
    timeoutMs: 120000,
    memoryMode: 'hybrid',
    statusToolEnabled: true,
    autoRecall: true,
    recallPrefetch: 'recall',
    recallOrder: 500,
    recallMaxInputChars: 800,
    recallMaxTokens: 4096,
    recallTypes: ['observation'],
    recallTags: [],
    recallTagsMatch: 'any',
    recallPromptPreamble: '',
    autoRetain: true,
    retainEveryNTurns: 1,
    retainAsync: true,
    retainWaitForOperations: true,
    retainDrainTimeoutMs: 10000,
    retainOperationPollIntervalMs: 500,
    retainContext: 'conversation between a DeepSeek Harness agent and the user',
    retainTags: [],
    retainUserPrefix: 'User',
    retainAssistantPrefix: 'Assistant',
    retainTurnKinds: ['completed'],
    retainMaxChars: 200000,
    retainDocumentId: null,
    retainUpdateMode: null,
    includeToolResults: false,
    skipSubagents: true,
});
const BUDGETS = new Set(['low', 'mid', 'high']);
const MEMORY_MODES = new Set(['hybrid', 'context', 'tools']);
const PREFETCH_MODES = new Set(['recall', 'reflect']);
const TAG_MATCHES = new Set(['any', 'all', 'any_strict', 'all_strict']);
const UPDATE_MODES = new Set([null, 'append', 'replace']);
const ALIASES = {
    api_url: 'apiUrl',
    api_key: 'apiKey',
    bank_id: 'bankId',
    bank: 'bankId',
    recall_budget: 'budget',
    timeout: 'timeoutMs',
    memory_mode: 'memoryMode',
    auto_recall: 'autoRecall',
    recall_prefetch_method: 'recallPrefetch',
    recall_order: 'recallOrder',
    recall_max_input_chars: 'recallMaxInputChars',
    recall_max_tokens: 'recallMaxTokens',
    recall_types: 'recallTypes',
    recall_tags: 'recallTags',
    recall_tags_match: 'recallTagsMatch',
    recall_prompt_preamble: 'recallPromptPreamble',
    auto_retain: 'autoRetain',
    retain_every_n_turns: 'retainEveryNTurns',
    retain_async: 'retainAsync',
    retain_wait_for_operations: 'retainWaitForOperations',
    retain_drain_timeout_ms: 'retainDrainTimeoutMs',
    retain_operation_poll_interval_ms: 'retainOperationPollIntervalMs',
    retain_context: 'retainContext',
    retain_tags: 'retainTags',
    retain_user_prefix: 'retainUserPrefix',
    retain_assistant_prefix: 'retainAssistantPrefix',
    retain_turn_kinds: 'retainTurnKinds',
    retain_max_chars: 'retainMaxChars',
    retain_document_id: 'retainDocumentId',
    retain_update_mode: 'retainUpdateMode',
    include_tool_results: 'includeToolResults',
    skip_subagents: 'skipSubagents',
};
const BOOLEAN_KEYS = new Set([
    'autoRecall',
    'autoRetain',
    'retainAsync',
    'retainWaitForOperations',
    'includeToolResults',
    'skipSubagents',
    'statusToolEnabled',
]);
const INTEGER_KEYS = {
    timeoutMs: 1,
    recallOrder: Number.NEGATIVE_INFINITY,
    recallMaxInputChars: 0,
    recallMaxTokens: 1,
    retainEveryNTurns: 1,
    retainMaxChars: 1,
    retainDrainTimeoutMs: 1,
    retainOperationPollIntervalMs: 100,
};
function fail(message) {
    throw new Error(`dsh-hindsight: ${message}`);
}
export function normalizeTags(value) {
    if (value === undefined || value === null)
        return [];
    const list = Array.isArray(value) ? value : String(value).split(',');
    const seen = new Set();
    const tags = [];
    for (const item of list) {
        const tag = String(item ?? '').trim();
        if (tag && !seen.has(tag)) {
            seen.add(tag);
            tags.push(tag);
        }
    }
    return tags;
}
function normalizeStringList(value) {
    if (value === undefined || value === null)
        return [];
    const list = Array.isArray(value) ? value : String(value).split(',');
    return list.map(item => String(item).trim()).filter(Boolean);
}
function normalizeRaw(raw) {
    const out = {};
    for (const [key, value] of Object.entries(raw)) {
        out[ALIASES[key] ?? key] = value;
    }
    return out;
}
function readConfigFile(path, env) {
    const configured = path ?? env.HINDSIGHT_CONFIG;
    if (!configured)
        return {};
    const filename = resolve(String(configured));
    let parsed;
    try {
        parsed = JSON.parse(readFileSync(filename, 'utf8'));
    }
    catch (error) {
        fail(`cannot read config file ${JSON.stringify(filename)}: ${error.message}`);
    }
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) {
        fail(`config file ${JSON.stringify(filename)} must contain a JSON object`);
    }
    return normalizeRaw(parsed);
}
function parseInteger(key, value, fallback, minimum) {
    if (value === undefined || value === null || value === '')
        return fallback;
    const parsed = Number(value);
    if (!Number.isFinite(parsed))
        fail(`invalid ${key} ${JSON.stringify(value)}; expected a finite number`);
    const integer = Math.trunc(parsed);
    if (integer < minimum)
        fail(`invalid ${key} ${JSON.stringify(integer)}; expected >= ${minimum}`);
    return integer;
}
function parseBoolean(value, fallback) {
    if (value === undefined || value === null || value === '')
        return fallback;
    if (typeof value === 'boolean')
        return value;
    if (typeof value === 'string') {
        if (/^(1|true|yes|on)$/i.test(value))
            return true;
        if (/^(0|false|no|off)$/i.test(value))
            return false;
    }
    return fallback;
}
/**
 * Resolve and validate the plugin configuration.
 * @param raw - raw Cordis row config
 * @param env - environment (test seam)
 */
export function resolveConfig(raw = {}, env = process.env) {
    const file = readConfigFile(raw.configFile, env);
    const config = {
        ...DEFAULTS,
        ...file,
        ...normalizeRaw(raw),
    };
    // Known environment overrides. Empty env vars are ignored so `?? null`
    // expressions in a cordis patch cannot accidentally erase a real value.
    const envOverrides = [
        ['apiUrl', env.HINDSIGHT_API_URL],
        ['apiKey', env.HINDSIGHT_API_KEY],
        ['bankId', env.HINDSIGHT_BANK_ID],
        ['budget', env.HINDSIGHT_BUDGET],
        ['timeoutMs', env.HINDSIGHT_TIMEOUT],
        ['memoryMode', env.HINDSIGHT_MEMORY_MODE],
        ['retainTags', env.HINDSIGHT_RETAIN_TAGS],
        ['recallTags', env.HINDSIGHT_RECALL_TAGS],
        ['recallTypes', env.HINDSIGHT_RECALL_TYPES],
    ];
    for (const [key, value] of envOverrides) {
        if (value)
            config[key] = value;
    }
    const draft = config;
    draft.apiUrl = String(draft.apiUrl ?? '').trim().replace(/\/+$/, '');
    if (!/^https?:\/\//.test(String(draft.apiUrl))) {
        fail(`invalid apiUrl ${JSON.stringify(draft.apiUrl)}; expected http(s)://...`);
    }
    draft.bankId = String(draft.bankId ?? '').trim();
    if (!draft.bankId)
        fail('bankId must not be empty');
    if (String(draft.bankId).includes('/'))
        fail('bankId must not contain "/"');
    draft.apiKey = draft.apiKey === '' || draft.apiKey === undefined ? null : String(draft.apiKey);
    if (!BUDGETS.has(String(draft.budget)))
        fail(`invalid budget ${JSON.stringify(draft.budget)}; expected low, mid or high`);
    if (!MEMORY_MODES.has(String(draft.memoryMode)))
        fail(`invalid memoryMode ${JSON.stringify(draft.memoryMode)}; expected hybrid, context or tools`);
    if (!PREFETCH_MODES.has(String(draft.recallPrefetch)))
        fail(`invalid recallPrefetch ${JSON.stringify(draft.recallPrefetch)}; expected recall or reflect`);
    if (!TAG_MATCHES.has(String(draft.recallTagsMatch)))
        fail(`invalid recallTagsMatch ${JSON.stringify(draft.recallTagsMatch)}`);
    if (!UPDATE_MODES.has(draft.retainUpdateMode)) {
        fail(`invalid retainUpdateMode ${JSON.stringify(draft.retainUpdateMode)}; expected null, append or replace`);
    }
    for (const key of BOOLEAN_KEYS) {
        draft[key] = parseBoolean(draft[key], DEFAULTS[key]);
    }
    for (const [key, minimum] of Object.entries(INTEGER_KEYS)) {
        draft[key] = parseInteger(String(key), draft[key], DEFAULTS[key], minimum);
    }
    if (!Number.isFinite(Number(draft.recallOrder)))
        fail('recallOrder must be a finite number');
    const result = draft;
    result.recallTypes = normalizeStringList(result.recallTypes);
    result.recallTags = normalizeTags(result.recallTags);
    result.retainTags = normalizeTags(result.retainTags);
    result.retainContext = String(result.retainContext ?? '');
    result.retainUserPrefix = String(result.retainUserPrefix ?? 'User');
    result.retainAssistantPrefix = String(result.retainAssistantPrefix ?? 'Assistant');
    result.recallPromptPreamble = String(result.recallPromptPreamble ?? '');
    result.retainDocumentId = draft.retainDocumentId === '' || draft.retainDocumentId === undefined
        ? null
        : String(draft.retainDocumentId);
    result.retainUpdateMode = draft.retainUpdateMode === '' ? null : draft.retainUpdateMode;
    result.retainTurnKinds = normalizeStringList(result.retainTurnKinds);
    if (result.retainTurnKinds.length === 0)
        fail('retainTurnKinds must not be empty');
    return result;
}
