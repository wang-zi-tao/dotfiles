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

import { readFileSync } from 'node:fs'
import { resolve } from 'node:path'

export type MemoryMode = 'hybrid' | 'context' | 'tools'
export type RecallBudget = 'low' | 'mid' | 'high'
export type RecallPrefetch = 'recall' | 'reflect'
export type TagsMatch = 'any' | 'all' | 'any_strict' | 'all_strict'
export type RetainUpdateMode = null | 'append' | 'replace'

export interface HindsightConfig {
  apiUrl: string
  apiKey: string | null
  bankId: string
  budget: RecallBudget
  timeoutMs: number
  memoryMode: MemoryMode
  statusToolEnabled: boolean

  autoRecall: boolean
  recallPrefetch: RecallPrefetch
  recallOrder: number
  recallMaxInputChars: number
  recallMaxTokens: number
  recallTypes: string[]
  recallTags: string[]
  recallTagsMatch: TagsMatch
  recallPromptPreamble: string

  autoRetain: boolean
  retainEveryNTurns: number
  retainAsync: boolean
  retainWaitForOperations: boolean
  retainDrainTimeoutMs: number
  retainOperationPollIntervalMs: number
  retainContext: string
  retainTags: string[]
  retainUserPrefix: string
  retainAssistantPrefix: string
  retainTurnKinds: string[]
  retainMaxChars: number
  retainDocumentId: string | null
  retainUpdateMode: RetainUpdateMode
  includeToolResults: boolean
  skipSubagents: boolean
}

export const DEFAULT_API_URL = 'https://api.hindsight.vectorize.io'

export const DEFAULTS: Readonly<HindsightConfig> = Object.freeze({
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
})

const BUDGETS = new Set<string>(['low', 'mid', 'high'])
const MEMORY_MODES = new Set<string>(['hybrid', 'context', 'tools'])
const PREFETCH_MODES = new Set<string>(['recall', 'reflect'])
const TAG_MATCHES = new Set<string>(['any', 'all', 'any_strict', 'all_strict'])
const UPDATE_MODES = new Set<RetainUpdateMode>([null, 'append', 'replace'])

const ALIASES: Record<string, string> = {
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
}

const BOOLEAN_KEYS = new Set<keyof HindsightConfig>([
  'autoRecall',
  'autoRetain',
  'retainAsync',
  'retainWaitForOperations',
  'includeToolResults',
  'skipSubagents',
  'statusToolEnabled',
])

const INTEGER_KEYS: Partial<Record<keyof HindsightConfig, number>> = {
  timeoutMs: 1,
  recallOrder: Number.NEGATIVE_INFINITY,
  recallMaxInputChars: 0,
  recallMaxTokens: 1,
  retainEveryNTurns: 1,
  retainMaxChars: 1,
  retainDrainTimeoutMs: 1,
  retainOperationPollIntervalMs: 100,
}

function fail(message: string): never {
  throw new Error(`dsh-hindsight: ${message}`)
}

export function normalizeTags(value: unknown): string[] {
  if (value === undefined || value === null) return []
  const list: unknown[] = Array.isArray(value) ? value : String(value).split(',')
  const seen = new Set<string>()
  const tags: string[] = []
  for (const item of list) {
    const tag = String(item ?? '').trim()
    if (tag && !seen.has(tag)) {
      seen.add(tag)
      tags.push(tag)
    }
  }
  return tags
}

function normalizeStringList(value: unknown): string[] {
  if (value === undefined || value === null) return []
  const list: unknown[] = Array.isArray(value) ? value : String(value).split(',')
  return list.map(item => String(item).trim()).filter(Boolean)
}

type RawConfig = Record<string, unknown>

function normalizeRaw(raw: RawConfig): RawConfig {
  const out: RawConfig = {}
  for (const [key, value] of Object.entries(raw)) {
    out[ALIASES[key] ?? key] = value
  }
  return out
}

function readConfigFile(path: unknown, env: ProcessEnv): RawConfig {
  const configured = path ?? env.HINDSIGHT_CONFIG
  if (!configured) return {}
  const filename = resolve(String(configured))
  let parsed: unknown
  try {
    parsed = JSON.parse(readFileSync(filename, 'utf8'))
  } catch (error) {
    fail(`cannot read config file ${JSON.stringify(filename)}: ${(error as Error).message}`)
  }
  if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) {
    fail(`config file ${JSON.stringify(filename)} must contain a JSON object`)
  }
  return normalizeRaw(parsed as RawConfig)
}

function parseInteger(key: string, value: unknown, fallback: number, minimum: number): number {
  if (value === undefined || value === null || value === '') return fallback
  const parsed = Number(value)
  if (!Number.isFinite(parsed)) fail(`invalid ${key} ${JSON.stringify(value)}; expected a finite number`)
  const integer = Math.trunc(parsed)
  if (integer < minimum) fail(`invalid ${key} ${JSON.stringify(integer)}; expected >= ${minimum}`)
  return integer
}

function parseBoolean(value: unknown, fallback: boolean): boolean {
  if (value === undefined || value === null || value === '') return fallback
  if (typeof value === 'boolean') return value
  if (typeof value === 'string') {
    if (/^(1|true|yes|on)$/i.test(value)) return true
    if (/^(0|false|no|off)$/i.test(value)) return false
  }
  return fallback
}

/**
 * Resolve and validate the plugin configuration.
 * @param raw - raw Cordis row config
 * @param env - environment (test seam)
 */
export function resolveConfig(raw: RawConfig = {}, env: ProcessEnv = process.env): HindsightConfig {
  const file = readConfigFile(raw.configFile, env)
  const config = {
    ...DEFAULTS,
    ...file,
    ...normalizeRaw(raw),
  } as Record<string, unknown>

  // Known environment overrides. Empty env vars are ignored so `?? null`
  // expressions in a cordis patch cannot accidentally erase a real value.
  const envOverrides: Array<[keyof HindsightConfig, string | undefined]> = [
    ['apiUrl', env.HINDSIGHT_API_URL],
    ['apiKey', env.HINDSIGHT_API_KEY],
    ['bankId', env.HINDSIGHT_BANK_ID],
    ['budget', env.HINDSIGHT_BUDGET],
    ['timeoutMs', env.HINDSIGHT_TIMEOUT],
    ['memoryMode', env.HINDSIGHT_MEMORY_MODE],
    ['retainTags', env.HINDSIGHT_RETAIN_TAGS],
    ['recallTags', env.HINDSIGHT_RECALL_TAGS],
    ['recallTypes', env.HINDSIGHT_RECALL_TYPES],
  ]
  for (const [key, value] of envOverrides) {
    if (value) config[key] = value
  }

  const draft = config

  draft.apiUrl = String(draft.apiUrl ?? '').trim().replace(/\/+$/, '')
  if (!/^https?:\/\//.test(String(draft.apiUrl))) {
    fail(`invalid apiUrl ${JSON.stringify(draft.apiUrl)}; expected http(s)://...`)
  }
  draft.bankId = String(draft.bankId ?? '').trim()
  if (!draft.bankId) fail('bankId must not be empty')
  if (String(draft.bankId).includes('/')) fail('bankId must not contain "/"')

  draft.apiKey = draft.apiKey === '' || draft.apiKey === undefined ? null : String(draft.apiKey)

  if (!BUDGETS.has(String(draft.budget))) fail(`invalid budget ${JSON.stringify(draft.budget)}; expected low, mid or high`)
  if (!MEMORY_MODES.has(String(draft.memoryMode))) fail(`invalid memoryMode ${JSON.stringify(draft.memoryMode)}; expected hybrid, context or tools`)
  if (!PREFETCH_MODES.has(String(draft.recallPrefetch))) fail(`invalid recallPrefetch ${JSON.stringify(draft.recallPrefetch)}; expected recall or reflect`)
  if (!TAG_MATCHES.has(String(draft.recallTagsMatch))) fail(`invalid recallTagsMatch ${JSON.stringify(draft.recallTagsMatch)}`)
  if (!UPDATE_MODES.has(draft.retainUpdateMode as RetainUpdateMode)) {
    fail(`invalid retainUpdateMode ${JSON.stringify(draft.retainUpdateMode)}; expected null, append or replace`)
  }

  for (const key of BOOLEAN_KEYS) {
    draft[key] = parseBoolean(draft[key], DEFAULTS[key] as boolean)
  }
  for (const [key, minimum] of Object.entries(INTEGER_KEYS) as Array<[keyof HindsightConfig, number]>) {
    draft[key] = parseInteger(String(key), draft[key], DEFAULTS[key] as number, minimum)
  }
  if (!Number.isFinite(Number(draft.recallOrder))) fail('recallOrder must be a finite number')

  const result = draft as unknown as HindsightConfig
  result.recallTypes = normalizeStringList(result.recallTypes)
  result.recallTags = normalizeTags(result.recallTags)
  result.retainTags = normalizeTags(result.retainTags)
  result.retainContext = String(result.retainContext ?? '')
  result.retainUserPrefix = String(result.retainUserPrefix ?? 'User')
  result.retainAssistantPrefix = String(result.retainAssistantPrefix ?? 'Assistant')
  result.recallPromptPreamble = String(result.recallPromptPreamble ?? '')
  result.retainDocumentId = draft.retainDocumentId === '' || draft.retainDocumentId === undefined
    ? null
    : String(draft.retainDocumentId)
  result.retainUpdateMode = draft.retainUpdateMode === '' ? null : draft.retainUpdateMode as RetainUpdateMode
  result.retainTurnKinds = normalizeStringList(result.retainTurnKinds)
  if (result.retainTurnKinds.length === 0) fail('retainTurnKinds must not be empty')

  return result
}
