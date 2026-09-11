/**
 * dsh-hindsight — Hindsight long-term memory for DeepSeek Harness.
 *
 * Host-side Cordis plugin. The memory bank is shared across sessions, so the
 * row belongs in the host composition (the profile bundle), not in one agent
 * preset:
 *
 *   - `ctx.tools.register` exposes hindsight_retain / hindsight_recall /
 *     hindsight_reflect / hindsight_status; `ctx.commands.register` exposes
 *     the `/hindsight-import` slash command for importing historical sessions.
 *   - `ctx.systemPrompt.section` adds static Hindsight guidance; dynamic
 *     recall is injected per turn by the `agent/pre-step` hook (recall API).
 *   - `agent/pre-step` (`{ global: true }`) calls the Hindsight recall API
 *     and appends the recalled memories as a user-role message.
 *   - `session/event` (`turn/end`) drives automatic per-turn retention.
 *   - `session/disposed` flushes any buffered turns and drops session state.
 *
 * The package is dependency-free. It talks to the Hindsight HTTP API directly
 * (`/v1/default/banks/{bank}/memories`, `.../memories/recall`, `/reflect`).
 * @module dsh-hindsight
 */

import {DEFAULTS, resolveConfig} from './config.js';
import type {HindsightConfig} from './config.js';
import {HindsightClient} from './client.js';
import type {MentalModel, RecallResult, RetainItem, RetainResponse} from './client.js';
import {buildTurnRecord, buildTurnRecordsFromEvents} from './transcript.js';
import type {TurnRecord} from './transcript.js';
import {installFileLogger} from './logger.js';
import type {ToolDefinition, ToolRunContext} from '@deepseek-ai/dsh-tools';
import type {CommandInvocation, CommandResult} from '@deepseek-ai/dsh-commands';
import type {PromptSection, PromptContext} from '@deepseek-ai/dsh-system-prompt';
import type {Agent, PreStepDecision} from '@deepseek-ai/dsh-agent';
import type {Logger, Context} from '@deepseek-ai/cordis';
import type {Session, SessionEvent, SessionId, UserMessage} from '@deepseek-ai/dsh-session';
import type {SessionPersistence} from '@deepseek-ai/dsh-session-persistence';

export const name = 'dsh-hindsight';
export const inject = ['tools', 'systemPrompt', 'commands','sessionPersistence'];
export {DEFAULTS, resolveConfig};
export type {HindsightConfig};
export {HindsightClient} from './client.js';
export type {
  HindsightClientOptions,
  RecallOptions,
  RecallResponse,
  ReflectOptions,
  RetainItem,
  RetainOptions,
  RetainResponse,
} from './client.js';
export {buildTurnRecord} from './transcript.js';
export type {Session, SessionEvent} from '@deepseek-ai/dsh-session';
export type {ToolDefinition} from '@deepseek-ai/dsh-tools';
export type {TurnMessage, TurnRecord} from './transcript.js';

/**
 * Lossless JSON value.  Matches the shape of `@deepseek-ai/dsh-util-values`'s
 * JsonValue; kept local so this plugin stays zero-runtime-dependency
 * (dsh-session >= 0.1.2-rc.1 no longer re-exports it).
 */
type JsonValue = null | boolean | number | string | JsonValue[] | {[key: string]: JsonValue;};

interface SessionState {
  buffer: TurnRecord[];
  chain: Promise<void>;
}

const TOOL_SECTION_ORDER = 135;

function makeLogger(ctx: Context): Logger {
  try {
    if (typeof ctx.logger === 'function') return ctx.logger('hindsight');
    if (ctx.logger) return ctx.logger as unknown as Logger;
  } catch {
    // fall through
  }
  return {
    name: 'hindsight',
    debug() {},
    info() {},
    warn() {},
    error() {},
  } as unknown as Logger;
}

function logFailure(logger: Logger, action: string, error: unknown): void {
  const message = error instanceof Error ? error.message : String(error);
  try {
    logger.warn(`${action} failed: ${message}`);
  } catch {
    // logging must never break a turn
  }
}

function mergeTags(...groups: Array<unknown[] | undefined>): string[] {
  const seen = new Set<string>();
  const out: string[] = [];
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

/** Narrow unknown tool-run args to a record (non-object input becomes {}). */
function toArgs(args: unknown): Record<string, unknown> {
  return typeof args === 'object' && args !== null ? args as Record<string, unknown> : {};
}

function requireString(args: unknown, key: string): string {
  const value = toArgs(args)[key];
  if (typeof value !== 'string' || value.trim() === '') {
    throw new Error(`Missing required string parameter: ${key}`);
  }
  return value;
}

function optionalString(args: unknown, key: string): string | undefined {
  const value = toArgs(args)[key];
  if (value === undefined || value === null) return undefined;
  return String(value);
}

function optionalInteger(args: unknown, key: string, fallback: number | undefined, minimum = 1): number | undefined {
  const value = toArgs(args)[key];
  if (value === undefined || value === null || value === '') return fallback;
  const parsed = Math.trunc(Number(value));
  if (!Number.isFinite(parsed) || parsed < minimum) return fallback;
  return parsed;
}

function optionalBudget(args: unknown, fallback: string): string {
  const value = optionalString(args, 'budget') ?? fallback;
  if (!['low', 'mid', 'high'].includes(value)) {
    throw new Error(`Invalid budget ${JSON.stringify(value)}; expected low, mid or high`);
  }
  return value;
}

function optionalTags(args: unknown, fallback: string[]): string[] {
  const value = toArgs(args).tags ?? fallback;
  if (value === undefined || value === null) return [];
  return mergeTags(Array.isArray(value) ? value : String(value).split(','));
}

function optionalTagsMatch(args: unknown, fallback: string): string {
  const value = optionalString(args, 'tagsMatch') ?? fallback;
  if (!['any', 'all', 'any_strict', 'all_strict'].includes(value)) {
    throw new Error(`Invalid tagsMatch ${JSON.stringify(value)}`);
  }
  return value;
}

function optionalTypes(args: unknown, fallback: string[]): string[] {
  const value = toArgs(args).types ?? fallback;
  if (value === undefined || value === null || value === '') return [];
  const list: unknown[] = Array.isArray(value) ? value : String(value).split(',');
  return list.map(item => String(item).trim()).filter(Boolean);
}

function agentMetadata(exec: ToolRunContext | undefined): Record<string, string> {
  const metadata: Record<string, string> = {source: 'dsh-hindsight'};
  const session = exec?.agent?.session;
  if (session?.id) metadata.sessionId = String(session.id);
  if (session?.header?.cwd) metadata.cwd = String(session.header.cwd);
  return metadata;
}

function retainItem(record: TurnRecord, session: Session, config: HindsightConfig): RetainItem {
  const item: RetainItem = {
    content: record.text,
    timestamp: record.startedAt,
    context: config.retainContext,
    update_mode: "append",
    document_id: `${session.id}-turn-${record.turn}`,
    metadata: {
      source: 'dsh-hindsight',
      sessionId: String(session.id),
      turn: String(record.turn),
      messageCount: String(record.messages.length),
      retainedAt: new Date().toISOString(),
      ...(session.header.cwd ? {cwd: String(session.header.cwd)} : {}),
    },
    tags: mergeTags(config.retainTags, [`session:${session.id}`]),
  };
  if (config.retainDocumentId) {
    item.document_id = config.retainDocumentId;
    if (config.retainUpdateMode) item.update_mode = config.retainUpdateMode;
  }
  return item;
}

function recallResultsToValue(results: RecallResult[] | undefined): Array<Record<string, unknown>> {
  const list: Array<Record<string, unknown>> = [];
  for (const result of results ?? []) {
    const item: Record<string, unknown> = {
      id: String(result.id ?? ''),
      text: String(result.text ?? ''),
    };
    if (typeof result.type === 'string') item.type = result.type;
    if (Array.isArray(result.tags)) item.tags = result.tags.filter(tag => typeof tag === 'string');
    list.push(item);
  }
  return list;
}

function recallValueToText(value: JsonValue): string {
  const record = (value ?? {}) as {message?: JsonValue; results?: Array<{text?: JsonValue;}>;} | null;
  const lines = [String(record?.message ?? '')];
  for (const result of record?.results ?? []) lines.push(`- ${String(result?.text ?? '')}`);
  return lines.filter(Boolean).join('\n');
}

function operationIdsFrom(response: RetainResponse): string[] {
  const ids: string[] = [];
  if (response.operation_id) ids.push(String(response.operation_id));
  if (Array.isArray(response.operation_ids)) ids.push(...response.operation_ids.map(String));
  return [...new Set(ids.filter(Boolean))];
}

function sleep(ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms));
}

async function waitForRetainOperations(
  client: HindsightClient,
  config: HindsightConfig,
  response: RetainResponse,
): Promise<void> {
  if (!config.retainAsync || !config.retainWaitForOperations) return;
  const operationIds = operationIdsFrom(response);
  if (operationIds.length === 0) return;
  const deadline = Date.now() + config.retainDrainTimeoutMs;
  const pending = new Set(operationIds);
  while (pending.size > 0 && Date.now() < deadline) {
    for (const operationId of [...pending]) {
      try {
        const status = await client.operationStatus(config.bankId, operationId, {timeoutMs: config.timeoutMs});
        if (status.status === 'completed') pending.delete(operationId);
        else if (status.status === 'failed') throw new Error(`Hindsight retain operation ${operationId} failed`);
      } catch (error) {
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

async function retainAndWait(
  client: HindsightClient,
  config: HindsightConfig,
  options: {bankId: string; items: RetainItem[]; signal?: AbortSignal;},
): Promise<RetainResponse> {
  const response = await client.retain({
    ...options,
    retainAsync: config.retainAsync,
    timeoutMs: config.timeoutMs,
  });
  await waitForRetainOperations(client, config, response);
  return response;
}

function toolDefinitions(client: HindsightClient, config: HindsightConfig): ToolDefinition[] {
  const definitions: ToolDefinition[] = [];

  definitions.push({
    name: 'hindsight_retain',
    description:
      'Store information to long-term memory. Hindsight automatically extracts structured facts, resolves entities, and indexes the content for later semantic retrieval.',
    parameters: {
      type: 'object',
      additionalProperties: false,
      properties: {
        content: {type: 'string', description: 'The information to store.'},
        context: {type: 'string', description: "Short label, e.g. 'user preference' or 'project decision'."},
        tags: {type: 'array', items: {type: 'string'}, description: 'Optional tags merged with configured default retain tags.'},
        documentId: {type: 'string', description: 'Optional document id; overrides the configured retainDocumentId for this call.'},
        updateMode: {type: 'string', enum: ['append', 'replace'], description: 'Optional update mode when documentId is used.'},
      },
      required: ['content'],
    },
    output: {
      schema: {
        type: 'object',
        additionalProperties: false,
        properties: {
          ok: {type: 'boolean'},
          message: {type: 'string'},
          bankId: {type: 'string'},
          itemsCount: {type: 'integer'},
          operationIds: {type: 'array', items: {type: 'string'}},
        },
        required: ['ok', 'message', 'bankId', 'itemsCount'],
      },
      render: (_args, value) => [{type: 'text', text: String((value as {message?: string;} | null)?.message ?? '')}],
    },
    async execute(args, exec) {
      const content = requireString(args, 'content');
      const context = optionalString(args, 'context');
      const tags = mergeTags(config.retainTags, optionalTags(args, []));
      const documentId = optionalString(args, 'documentId') ?? config.retainDocumentId;
      const updateMode = optionalString(args, 'updateMode') ?? (config.retainUpdateMode ?? undefined);
      const item: RetainItem = {content, context, metadata: agentMetadata(exec), tags};
      if (documentId) {
        item.document_id = documentId;
        if (updateMode) item.update_mode = updateMode;
      }
      try {
        const response = await retainAndWait(client, config, {
          bankId: config.bankId,
          items: [item],
          signal: exec?.signal,
        });
        const value: Record<string, unknown> = {
          ok: response.success !== false,
          message: `Stored 1 memory in Hindsight bank '${config.bankId}'.`,
          bankId: config.bankId,
          itemsCount: Number(response.items_count ?? 1),
        };
        const operationIds = operationIdsFrom(response);
        if (operationIds.length) value.operationIds = operationIds;
        return value;
      } catch (error) {
        throw new Error(`hindsight_retain failed: ${error instanceof Error ? error.message : error}`, {cause: error});
      }
    },
  });

  definitions.push({
    name: 'hindsight_recall',
    description:
      'Search long-term memory. Returns memories ranked by relevance using semantic search, keyword matching, entity-graph traversal, and reranking.',
    parameters: {
      type: 'object',
      additionalProperties: false,
      properties: {
        query: {type: 'string', description: 'What to search for.'},
        budget: {type: 'string', enum: ['low', 'mid', 'high'], description: 'Recall thoroughness override.'},
        maxTokens: {type: 'integer', description: 'Maximum tokens in returned results.'},
        types: {type: 'array', items: {type: 'string'}, description: 'Fact types: world, experience, opinion, observation.'},
        tags: {type: 'array', items: {type: 'string'}, description: 'Tags to filter by.'},
        tagsMatch: {type: 'string', enum: ['any', 'all', 'any_strict', 'all_strict'], description: 'Tag matching mode.'},
      },
      required: ['query'],
    },
    output: {
      schema: {
        type: 'object',
        additionalProperties: false,
        properties: {
          ok: {type: 'boolean'},
          message: {type: 'string'},
          bankId: {type: 'string'},
          count: {type: 'integer'},
          results: {
            type: 'array',
            items: {
              type: 'object',
              additionalProperties: false,
              properties: {
                id: {type: 'string'},
                text: {type: 'string'},
                type: {type: 'string'},
                tags: {type: 'array', items: {type: 'string'}},
              },
              required: ['id', 'text'],
            },
          },
        },
        required: ['ok', 'message', 'bankId', 'count', 'results'],
      },
      render: (_args, value) => [{type: 'text', text: recallValueToText(value)}],
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
            ? `Recalled ${results.length} memories from Hindsight.`
            : `No relevant memories found in Hindsight bank '${config.bankId}'.`,
          bankId: config.bankId,
          count: results.length,
          results,
        };
      } catch (error) {
        throw new Error(`hindsight_recall failed: ${error instanceof Error ? error.message : error}`, {cause: error});
      }
    },
  });

  definitions.push({
    name: 'hindsight_reflect',
    description:
      'Synthesize a reasoned answer from long-term memories. Unlike recall, this reasons across all stored memories to produce a coherent response.',
    parameters: {
      type: 'object',
      additionalProperties: false,
      properties: {
        query: {type: 'string', description: 'The question to reflect on.'},
        budget: {type: 'string', enum: ['low', 'mid', 'high'], description: 'Reflection thoroughness override.'},
        context: {type: 'string', description: 'Optional extra context for the reflection.'},
        maxTokens: {type: 'integer', description: 'Maximum tokens for the answer.'},
        tags: {type: 'array', items: {type: 'string'}, description: 'Tags to filter memories by.'},
        tagsMatch: {type: 'string', enum: ['any', 'all', 'any_strict', 'all_strict'], description: 'Tag matching mode.'},
      },
      required: ['query'],
    },
    output: {
      schema: {
        type: 'object',
        additionalProperties: false,
        properties: {
          ok: {type: 'boolean'},
          message: {type: 'string'},
          text: {type: 'string'},
        },
        required: ['ok', 'message', 'text'],
      },
      render: (_args, value) => [{type: 'text', text: String((value as {text?: string;} | null)?.text ?? '')}],
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
        return {ok: true, message: `Hindsight reflection completed for bank '${config.bankId}'.`, text};
      } catch (error) {
        throw new Error(`hindsight_reflect failed: ${error instanceof Error ? error.message : error}`, {cause: error});
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
            ok: {type: 'boolean'},
            message: {type: 'string'},
            apiUrl: {type: 'string'},
            bankId: {type: 'string'},
            version: {type: 'string'},
          },
          required: ['ok', 'message', 'apiUrl', 'bankId', 'version'],
        },
        render: (_args, value) => [{type: 'text', text: String((value as {message?: string;} | null)?.message ?? '')}],
      },
      async execute(_args, exec) {
        try {
          const response = await client.version({signal: exec?.signal});
          const version = String(response.version ?? 'unknown');
          return {
            ok: true,
            message: `Hindsight reachable at ${config.apiUrl}; bank '${config.bankId}'; API version ${version}.`,
            apiUrl: config.apiUrl,
            bankId: config.bankId,
            version,
          };
        } catch (error) {
          throw new Error(`hindsight_status failed: ${error instanceof Error ? error.message : error}`, {cause: error});
        }
      },
    });
  }

  return definitions;
}

/** Parse `/hindsight-import` raw input into structured options. */
function parseImportArgs(raw: string): {sessionId?: string; bankId?: string; maxTurns?: number; turnKinds?: string[];} {
  const tokens = raw.trim().split(/\s+/).filter(Boolean);
  const args: {sessionId?: string; bankId?: string; maxTurns?: number; turnKinds?: string[];} = {};
  const positionals: string[] = [];
  for (let i = 0; i < tokens.length; i++) {
    const token = tokens[i]!;
    if (token.startsWith('--')) {
      const eq = token.indexOf('=');
      const key = eq >= 0 ? token.slice(2, eq) : token.slice(2);
      let value = eq >= 0 ? token.slice(eq + 1) : undefined;
      if (value === undefined) {
        const next = tokens[i + 1];
        if (next !== undefined && !next.startsWith('--')) {
          value = next;
          i++;
        }
      }
      switch (key) {
        case 'bank':
          if (value) args.bankId = value;
          break;
        case 'max-turns': {
          const n = Number(value);
          if (Number.isInteger(n) && n >= 1) args.maxTurns = n;
          break;
        }
        case 'turn-kinds':
          if (value) args.turnKinds = value.split(',').map(part => part.trim()).filter(Boolean);
          break;
      }
    } else {
      positionals.push(token);
    }
  }
  if (positionals.length > 0) args.sessionId = positionals[0];
  return args;
}

function clip(text: string, maxChars: number): string {
  if (maxChars <= 0 || text.length <= maxChars) return text;
  return `${text.slice(0, maxChars - 20)}\n…[truncated]`;
}

/** Extract the first user text from pre-step messages. */
function extractUserQuery(messages: readonly UserMessage[]): string {
  for (const msg of messages) {
    if (msg.source?.kind === 'user') {
      for (const block of msg.content) {
        if (block.type === 'text' && block.text.trim()) {
          return block.text.trim();
        }
      }
    }
  }
  return '';
}

/** Call recall with a timeout; returns results on success, undefined on failure or timeout (fail-open). */
async function recallWithTimeout(
  client: HindsightClient,
  config: HindsightConfig,
  query: string,
  signal?: AbortSignal,
): Promise<RecallResult[] | undefined> {
  try {
    const result = await Promise.race([
      client.recall({
        bankId: config.bankId,
        query: clip(query, config.recallMaxInputChars),
        budget: config.budget,
        maxTokens: config.recallMaxTokens,
        types: config.recallTypes,
        tags: config.recallTags,
        tagsMatch: config.recallTagsMatch,
        signal,
        timeoutMs: config.timeoutMs,
      }),
      sleep(config.recallTimeoutMs).then(() => undefined),
    ]);
    const results = result?.results?.filter(r => typeof r.text === 'string' && r.text.trim());
    return results && results.length > 0 ? results : undefined;
  } catch {
    return undefined;
  }
}

let memoryMessageSeq = 0;

/** Build a user-role memory message for injection into the pre-step decision. */
function memoryMessage(text: string): UserMessage {
  const preamble = '<hindsight-recall>\n# Hindsight Memory (recalled for this turn)\n\n';
  const footer = '\n</hindsight-recall>';
  return {
    id: `hindsight-${Date.now()}-${++memoryMessageSeq}` as UserMessage['id'],
    role: 'user',
    content: [{type: 'text', text: `${preamble}${text}${footer}`}],
    source: {kind: 'plugin', plugin: 'dsh-hindsight', form: 'recall'},
  };
}

/** Build a user-role mental-model message for injection into the pre-step decision. */
function mentalModelMessage(name: string, content: string): UserMessage {
  const preamble = '<hindsight-mental-model name="' + name + '">\n# Hindsight Mental Model: ' + name + '\n\n';
  const footer = '\n</hindsight-mental-model>';
  return {
    id: ('hindsight-mm-' + Date.now() + '-' + (++memoryMessageSeq)) as UserMessage['id'],
    role: 'user',
    content: [{type: 'text', text: preamble + content + footer}],
    source: {kind: 'plugin', plugin: 'dsh-hindsight', form: 'recall'},
  };
}

/** Format recall results as a compact bulleted text block for injection. */
function recallResultsToText(results: RecallResult[]): string {
  const lines: string[] = [];
  for (const result of results) {
    const text = String(result.text ?? '').trim();
    if (!text) continue;
    const meta: string[] = [];
    if (typeof result.type === 'string' && result.type) meta.push(result.type);
    if (Array.isArray(result.tags) && result.tags.length > 0) meta.push(result.tags.join(', '));
    lines.push(meta.length > 0 ? `- ${text}  —  ${meta.join(' · ')}` : `- ${text}`);
  }
  return lines.join('\n');
}

/**
 * Recall in the background and queue the matched memories into the agent's
 * next pre-step context via {@link Agent.inject}. Never blocks the agent and
 * never wakes an idle driver: the injected message is claimed at the nearest
 * later step boundary (or stays pending for the next follow-up). Fail-open:
 * timeouts, recall errors, or an unavailable/invalid agent never throw.
 */
async function recallAndInject(
  agent: Agent,
  query: string,
  signal: AbortSignal | undefined,
  deps: {client: HindsightClient; config: HindsightConfig; logger: Logger;},
): Promise<void> {
  const results = await recallWithTimeout(deps.client, deps.config, query, signal);
  if (!results) return;
  try {
    if (typeof agent.inject !== 'function') {
      deps.logger.warn('hindsight: agent.inject is unavailable, recall not injected');
      return;
    }
    agent.inject(memoryMessage(recallResultsToText(results)));
    deps.logger.info(`hindsight recall injected for agent ${agent.id}`);
  } catch (error) {
    deps.logger.warn(`hindsight recall injection failed: ${String(error)}`);
  }
}


/**
 * A mental model the plugin wants to keep available for the agent: the user
 * preference model plus one project model derived from the session cwd.
 */
interface MentalModelWanted {
  /** Stable identity for dedup and in-flight tracking. */
  key: string;
  name: string;
  sourceQuery: string;
  tags: string[];
}

function wantedMentalModels(config: HindsightConfig, cwd: string | undefined): MentalModelWanted[] {
  const wants: MentalModelWanted[] = [];
  if (config.mentalModelUserQuery.trim()) {
    wants.push({
      key: 'user',
      name: config.mentalModelUserQuery.trim(),
      sourceQuery: config.mentalModelUserQuery.trim(),
      tags: config.mentalModelUserTags,
    });
  }
  if (cwd && config.mentalModelProjectQueryTemplate.trim()) {
    const query = config.mentalModelProjectQueryTemplate.replace(/{cwd}/g, cwd);
    wants.push({
      key: 'project:' + cwd,
      name: '项目 ' + cwd,
      sourceQuery: query,
      tags: config.mentalModelProjectTags,
    });
  }
  return wants;
}

/**
 * Ensure a mental model exists for the wanted query: look it up by
 * source_query (falling back to name/id), auto-create it when missing, and
 * wait for the background reflect to finish. Returns the fresh model or
 * undefined when it is unavailable (fail-open).
 */
async function ensureMentalModel(
  client: HindsightClient,
  config: HindsightConfig,
  want: MentalModelWanted,
  creating: Map<string, Promise<MentalModel | undefined>>,
  signal?: AbortSignal,
): Promise<MentalModel | undefined> {
  const inflight = creating.get(want.key);
  if (inflight) return inflight;
  const task = (async () => {
    try {
      const list = await client.listMentalModels(config.bankId, {signal, timeoutMs: config.mentalModelRequestTimeoutMs});
      const items = Array.isArray(list.items) ? list.items : [];
      const found = items.find(mm => mm.source_query === want.sourceQuery)
        ?? items.find(mm => mm.name === want.name)
        ?? (want.key === 'user' ? items.find(mm => mm.id === 'user_advise') : undefined);
      if (found) {
        return await client.getMentalModel(config.bankId, found.id, {signal, timeoutMs: config.mentalModelRequestTimeoutMs});
      }
      if (!config.mentalModelAutoCreate) return undefined;
      const trigger: Record<string, unknown> = {
        mode: config.mentalModelRefreshMode,
        refresh_after_consolidation: true,
      };
      if (config.mentalModelFactTypes.length > 0) trigger.fact_types = config.mentalModelFactTypes;
      const created = await client.createMentalModel({
        bankId: config.bankId,
        id: want.key === 'user' ? 'user_advise' : undefined,
        name: want.name,
        sourceQuery: want.sourceQuery,
        tags: want.tags,
        maxTokens: config.mentalModelMaxTokens,
        trigger,
        signal,
        timeoutMs: config.mentalModelRequestTimeoutMs,
      });
      const operationId = created.operation_id;
      if (operationId) {
        const deadline = Date.now() + config.mentalModelTimeoutMs;
        while (Date.now() < deadline) {
          const status = await client.operationStatus(config.bankId, operationId, {signal, timeoutMs: config.mentalModelRequestTimeoutMs});
          if (status.status === 'completed') break;
          if (status.status === 'failed') return undefined;
          await sleep(config.mentalModelPollIntervalMs);
        }
      }
      const id = created.mental_model_id;
      if (!id) return undefined;
      return await client.getMentalModel(config.bankId, id, {signal, timeoutMs: config.mentalModelRequestTimeoutMs});
    } catch (error) {
      return undefined;
    }
  })();
  creating.set(want.key, task);
  task.finally(() => {
    if (creating.get(want.key) === task) creating.delete(want.key);
  }).catch(() => {});
  return task;
}

/**
 * Background: ensure user + project mental models exist and inject them into
 * the agent's context — at most once per session (agent). The injection is
 * keyed by agent id + wanted model, so every new session gets the current
 * settled knowledge exactly once, and later turns of the same session skip
 * the query entirely. When a model is not ready yet (auto-create still
 * reflecting) it is not marked, so a later turn retries. Fail-open: any
 * error only logs; the agent never waits on the memory fetch.
 */
async function mentalModelsAndInject(
  agent: Agent,
  signal: AbortSignal | undefined,
  deps: {client: HindsightClient; config: HindsightConfig; creatingMentalModels: Map<string, Promise<MentalModel | undefined>>; injectedMentalModels: Set<string>; logger: Logger;},
): Promise<void> {
  if (!deps.config.autoMentalModel) return;
  const cwd = typeof agent.session?.header?.cwd === 'string' ? agent.session.header.cwd : undefined;
  const wants = wantedMentalModels(deps.config, cwd);
  for (const want of wants) {
    const sessionKey = agent.id + ':' + want.key;
    if (deps.injectedMentalModels.has(sessionKey)) continue;
    const mm = await ensureMentalModel(deps.client, deps.config, want, deps.creatingMentalModels, signal);
    const content = typeof mm?.content === 'string' && mm.content.trim() ? mm.content : '';
    if (!content) continue;
    try {
      if (typeof agent.inject !== 'function') {
        deps.logger.warn('hindsight: agent.inject is unavailable, mental model not injected');
        continue;
      }
      agent.inject(mentalModelMessage(want.name, content));
      deps.injectedMentalModels.add(sessionKey);
      deps.logger.info('hindsight mental model injected: ' + want.key + ' for agent ' + agent.id);
    } catch (error) {
      deps.logger.warn('hindsight mental model injection failed: ' + String(error));
    }
  }
}

/**
 * Compose the pre-step decision without blocking the agent on recall: the
 * decision is returned unchanged, and {@link recallAndInject} is scheduled
 * fire-and-forget so the memory arrives at the next step boundary.
 * Deduplication keyed by agent id + turn prevents duplicate scheduling.
 */
async function preStepDecision(
  agent: Agent,
  messages: readonly UserMessage[],
  turn: number,
  signal: AbortSignal | undefined,
  next: () => Promise<PreStepDecision>,
  deps: {client: HindsightClient; config: HindsightConfig; injectedTurns: Set<string>; creatingMentalModels: Map<string, Promise<MentalModel | undefined>>; injectedMentalModels: Set<string>; logger: Logger;},
): Promise<PreStepDecision> {
  if ((!deps.config.autoRecall && !deps.config.autoMentalModel) || deps.config.memoryMode === 'tools') return next();
  const decision = await next();
  if (decision.kind !== 'enter') return decision;
  if (decision.messages.length === 0) return decision;
  const key = agent.id + ':' + turn;
  if (deps.injectedTurns.has(key)) return decision;
  if (deps.config.skipSubagents && agent.session.header.origin === 'subagent') return decision;
  const query = extractUserQuery(messages);
  deps.injectedTurns.add(key);
  if (deps.config.autoRecall && query) void recallAndInject(agent, query, signal, deps);
  if (deps.config.autoMentalModel) void mentalModelsAndInject(agent, signal, deps);
  return decision;
}

/**
 * The plugin entrypoint.
 */
export function apply(ctx: Context, rawConfig: HindsightConfig | Record<string, unknown> = {}): void {
  const config = resolveConfig(rawConfig as Record<string, unknown>);
  const logger = makeLogger(ctx);
  const client = new HindsightClient({
    apiUrl: config.apiUrl,
    apiKey: config.apiKey,
    timeoutMs: config.timeoutMs,
  });

  // Persist hindsight logs to a file: dsh hosts register no cordis logger
  // sink, so without this every line lands only in the in-memory buffer.
  try {
    installFileLogger(ctx, config.logDir, logger);
  } catch (error) {
    logFailure(logger, 'hindsight file logger install', error);
  }

  // sessionId -> { buffer: turn records, chain: Promise }
  const states = new Map<string, SessionState>();
  const injectedTurns = new Set<string>();
  const disposedSessions = new Set<string>();
  const creatingMentalModels = new Map<string, Promise<MentalModel | undefined>>();
  const injectedMentalModels = new Set<string>();

  function stateFor(sessionId: string): SessionState {
    let state = states.get(sessionId);
    if (!state) {
      state = {buffer: [], chain: Promise.resolve()};
      states.set(sessionId, state);
    }
    return state;
  }

  async function flushRetain(session: Session, state: SessionState): Promise<void> {
    const records = state.buffer.slice();
    if (records.length === 0) return;
    const items = records.map(record => retainItem(record, session, config));
    // Detect duplicate document_ids (possible when retainDocumentId is explicitly
    // set, forcing every item to share one id).  The server rejects duplicates in
    // one batch — retain sequentially instead.
    const docIds = items.map(item => item.document_id);
    const hasDuplicate = new Set(docIds).size !== docIds.length;
    if (hasDuplicate) {
      for (const item of items) {
        await retainAndWait(client, config, {bankId: config.bankId, items: [item]});
      }
    } else {
      await retainAndWait(client, config, {bankId: config.bankId, items});
    }
    state.buffer.splice(0, records.length);
    logger.info(`hindsight retained ${items.length} turn(s) for session ${session.id}`);
  }

  function handleCompletedTurn(session: Session, event: SessionEvent): void {
    if (event.type !== 'turn/end') return;
    const turn = event.data.turn;
    const reasonKind = event.data.reason.kind;
    if (!config.retainTurnKinds.includes(reasonKind)) return;
    if (config.skipSubagents && session.header.origin === 'subagent') return;
    if (disposedSessions.has(String(session.id))) return;

    const record = buildTurnRecord(session, turn, config);
    if (!record) return;

    const state = stateFor(String(session.id));
    state.chain = state.chain
      .then(async () => {
        if (config.autoRetain) {
          state.buffer.push(record);
          if (state.buffer.length >= config.retainEveryNTurns) {
            try {
              await flushRetain(session, state);
            } catch (error) {
              // Keep the buffer; the next completed turn retries the batch.
              logFailure(logger, `hindsight retain for session ${session.id}`, error);
            }
          }
        }
      })
      .catch(error => logFailure(logger, `hindsight turn hook for session ${session.id}`, error));
  }

  function disposeSession(session: Session): void {
    disposedSessions.add(String(session.id));
    // Clean up injected-turn bookkeeping for this session's agent.
    for (const key of injectedTurns) {
      if (key.startsWith(String(session.id) + ':')) injectedTurns.delete(key);
    }
    const state = states.get(String(session.id));
    states.delete(String(session.id));
    if (!state || state.buffer.length === 0 || !config.autoRetain) return;
    // Best-effort final flush. It runs off the reply/teardown path.
    state.chain = state.chain
      .then(() => flushRetain(session, state))
      .catch(error => logFailure(logger, `hindsight final flush for session ${session.id}`, error));
  }

  ctx.on('session/event', (session: Session, event: SessionEvent) => {
    if (event.type === 'turn/end') {
      try {
        handleCompletedTurn(session, event);
      } catch (error) {
        logFailure(logger, `hindsight session event for ${session.id}`, error);
      }
    }
  }, {global: true});

  ctx.on('session/disposed', (session: Session) => {
    try {
      disposeSession(session);
    } catch (error) {
      logFailure(logger, 'hindsight session disposal', error);
    }
  }, {global: true});

  // Static prompt section. Order 135 sits inside the 100–199 per-tool
  // guidance band, after the tool-catalog guidance and before later tools.
  if (config.memoryMode !== 'context') {
    const statusLine = config.statusToolEnabled
      ? 'Use hindsight_status to verify the server connection and active bank.'
      : '';
    ctx.systemPrompt.section({
      name: 'hindsight:memory',
      order: TOOL_SECTION_ORDER,
      text: [
        '# Hindsight Memory',
        `Active. Bank: ${config.bankId}; recall budget: ${config.budget}.`,
        'Relevant long-term memories are synthesized by the Hindsight reflect API and injected into your context at the start of relevant turns.',
        'Use hindsight_recall to search memories, hindsight_reflect to synthesize an answer across memories, and hindsight_retain to store information.',
        ...(statusLine ? [statusLine] : []),
      ].join('\n'),
    });
  }

  // Agent pre-step hook: schedule a background reflect and queue the
  // synthesized memory via agent.inject() — the agent never waits on the
  // memory fetch. Fail-open: timeouts or reflect errors never block the turn.
  ctx.on('agent/pre-step', (payload, next) =>
    preStepDecision(payload.agent, payload.messages, payload.turn, payload.signal, next, {
      client,
      config,
      injectedTurns,
      creatingMentalModels,
      injectedMentalModels,
      logger,
    }),
    {global: true});

  // /hindsight-import — import historical session turns into memory.
  // Slash commands are user-driven (not model tools), so this is registered
  // unconditionally: importing is available even in `memoryMode: context`.
  ctx.commands.register({
    name: 'hindsight-import',
    description:
      'Import a historical dsh session into the Hindsight memory bank. ' +
      'No arguments lists importable sessions; pass a session id to import.',
    input: {hint: '[sessionId] [--bank <id>] [--max-turns <n>] [--turn-kinds <a,b>]'},
    async handler(invocation: CommandInvocation): Promise<CommandResult> {
      const sp: SessionPersistence | undefined = ctx.sessionPersistence;
      if (!sp) {
        return {
          kind: 'error',
          text: 'hindsight-import: session persistence is not available. ' +
            'This command requires a dsh runtime that exposes ctx.sessionPersistence.',
        };
      }
      const args = parseImportArgs(invocation.rawInput);
      const targetBank = String(args.bankId ?? config.bankId);
      const maxTurns = args.maxTurns;
      const turnKinds = (Array.isArray(args.turnKinds) && args.turnKinds.length > 0)
        ? args.turnKinds
        : config.retainTurnKinds;

      try {
        // List mode: list importable sessions.
        if (!args.sessionId) {
          const headers = await sp.list();
          const sessions = headers.map(s => ({
            id: String(s.id),
            cwd: String(s.cwd ?? ''),
            createdAt: String(s.createdAt),
          }));
          if (sessions.length === 0) return {kind: 'success', text: 'No importable sessions found.'};
          const lines = sessions.map(s => `  ${s.id}  (cwd: ${s.cwd}, created: ${s.createdAt})`);
          return {kind: 'success', text: `Found ${sessions.length} session(s):\n${lines.join('\n')}`};
        }

        // Import mode.
        const sessionId = args.sessionId as SessionId;
        const inspected = await sp.inspect(sessionId);
        const events = Array.isArray(inspected.events) ? [...inspected.events] : ([] as SessionEvent[]);
        if (events.length === 0) {
          return {kind: 'success', text: `Session ${sessionId} has no events to import.`};
        }

        // Use the shared turn-record builder; it filters by turnKinds internally.
        const importConfig = {...config, bankId: targetBank, retainTurnKinds: turnKinds};
        const records = buildTurnRecordsFromEvents(events, importConfig);
        const limited = maxTurns != null ? records.slice(0, maxTurns) : records;
        if (limited.length === 0) {
          return {kind: 'success', text: `No importable turns found in session ${sessionId}.`};
        }

        let retained = 0;
        for (const record of limited) {
          const item: RetainItem = {
            content: record.text,
            timestamp: record.startedAt,
            context: `imported session ${sessionId} turn ${record.turn}`,
            document_id: `${sessionId}-turn-${record.turn}`,
            update_mode: 'replace',
            metadata: {
              source: 'dsh-hindsight',
              sessionId,
              turn: String(record.turn),
              messageCount: String(record.messages.length),
              importedAt: new Date().toISOString(),
            },
            tags: mergeTags(config.retainTags, [`session:${sessionId}`]),
          };
          try {
            await retainAndWait(client, config, {
              bankId: targetBank,
              items: [item],
            });
            retained++;
          } catch (error) {
            logFailure(logger, `hindsight-import retain turn ${record.turn}`, error);
          }
        }
        const skipped = limited.length - retained;
        return {
          kind: 'success',
          text: `Imported ${retained} of ${limited.length} turn(s) from session ${sessionId} to bank '${targetBank}'.` +
            (skipped > 0 ? ` (${skipped} failed)` : ''),
        };
      } catch (error) {
        return {kind: 'error', text: `hindsight-import failed: ${error instanceof Error ? error.message : String(error)}`};
      }
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
