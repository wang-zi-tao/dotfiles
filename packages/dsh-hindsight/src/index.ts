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

import {DEFAULTS, cwdTag, resolveConfig} from './config.js';
import type {HindsightConfig} from './config.js';
import {HindsightClient} from './client.js';
import type {
  KnowledgePage,
  KnowledgePageNode,
  MentalModel,
  RecallResult,
  RetainItem,
  RetainResponse,
} from './client.js';
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
import type {ContextFormed} from '@deepseek-ai/dsh-llm';

/**
 * This plugin's own injected-context kind. DSH 0.1.7 dropped the shared
 * catch-all `plugin` source: every producer declares its kind by merging into
 * the harness's `MessageSourceMap`.
 */
declare module '@deepseek-ai/dsh-llm' {
  interface MessageSourceMap {
    'dsh-hindsight': {kind: 'dsh-hindsight';} & ContextFormed;
  }
}

export const name = 'dsh-hindsight';
export const inject = ['tools', 'systemPrompt', 'commands', 'sessionPersistence'];
export {DEFAULTS, cwdTag, resolveConfig};
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

/** Flatten an error and its `cause` chain (fetch failures hide the reason one level down). */
function errorChain(error: unknown): string {
  const parts: string[] = [];
  let current: unknown = error;
  for (let depth = 0; current !== undefined && current !== null && depth < 4; depth += 1) {
    if (current instanceof Error) {
      parts.push(current.message || current.name);
      current = (current as {cause?: unknown}).cause;
    } else {
      parts.push(String(current));
      break;
    }
  }
  return parts.filter(part => part && part.trim()).join(' <- ') || 'unknown error';
}

function logFailure(logger: Logger, action: string, error: unknown): void {
  try {
    logger.warn(`${action} failed: ${errorChain(error)}`);
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

/** The tool-run agent's session working directory, when it has one. */
function agentCwd(exec: ToolRunContext | undefined): string | undefined {
  const cwd = exec?.agent?.session?.header?.cwd;
  return typeof cwd === 'string' && cwd.trim() ? cwd : undefined;
}

function retainItem(record: TurnRecord, session: Session, config: HindsightConfig): RetainItem {
  const item: RetainItem = {
    content: record.text,
    timestamp: record.startedAt,
    context: config.retainContext,
    // A per-turn document is a full snapshot of that turn — `record.text` is the
    // WHOLE turn, never a delta — so `replace` is the idempotent choice: a
    // re-send (a batch kept after a failure, a replay, a resumed session)
    // rewrites the document with the content it already holds.
    //
    // Appending the snapshot to itself does not survive a real server: its
    // append guard requires the new body to extend the stored one byte-for-byte,
    // and the stored body is `JSON.stringify` output (compact) that the server
    // re-serializes with `", "` / `": "` separators. The append is then refused
    // forever — `AppendWouldTruncateDocument` — and the turn never lands.
    update_mode: "replace",
    document_id: `${session.id}-turn-${record.turn}`,
    metadata: {
      source: 'dsh-hindsight',
      sessionId: String(session.id),
      turn: String(record.turn),
      messageCount: String(record.messages.length),
      retainedAt: new Date().toISOString(),
      ...(session.header.cwd ? {cwd: String(session.header.cwd)} : {}),
    },
    // Visibility scope: the configured default tags, the session's working
    // directory (`cwd:<cwd>`) and the session id. The cwd tag is what a
    // project mental model scoped to the same cwd can read back.
    tags: mergeTags(
      config.retainTags,
      [cwdTag(config.retainCwdTagPrefix, session.header.cwd)],
      [`session:${session.id}`],
    ),
  };
  if (config.retainDocumentId) {
    // Every turn accumulates into ONE document: appending is the point of this
    // mode, so it stays the default here.
    item.document_id = config.retainDocumentId;
    item.update_mode = config.retainUpdateMode ?? "append";
  } else if (config.retainUpdateMode) {
    item.update_mode = config.retainUpdateMode;
  }
  return item;
}

/** Forget the retained turns in place and by identity: the buffer may hold more. */
function dropRetained(state: SessionState, records: TurnRecord[]): void {
  for (const record of records) {
    const index = state.buffer.indexOf(record);
    if (index >= 0) state.buffer.splice(index, 1);
  }
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

function toolDefinitions(client: HindsightClient, config: HindsightConfig, logger: Logger): ToolDefinition[] {
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
      const tags = mergeTags(
        config.retainTags,
        [cwdTag(config.retainCwdTagPrefix, agentCwd(exec))],
        optionalTags(args, []),
      );
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

  // Read-only knowledge-page tools. DSH has no MCP-style readOnlyHint; the
  // descriptions state it and the renderers return only the useful text so a
  // read costs its content, not a JSON envelope. The agent is expected to
  // list/search first and read only the pages it needs (pull, not push).
  if (config.knowledgePagesEnabled) {
    definitions.push({
      name: 'hindsight_list_pages',
      description:
        'List the curated Hindsight knowledge pages for the active bank (read-only). '
        + 'Returns each page name, folder and a one-line summary. Start here when you need standing project knowledge; '
        + 'then read only the pages that matter with hindsight_read_page.',
      parameters: {type: 'object', additionalProperties: false, properties: {}},
      output: {
        schema: {
          type: 'object',
          additionalProperties: false,
          properties: {
            ok: {type: 'boolean'},
            message: {type: 'string'},
            bankId: {type: 'string'},
            count: {type: 'integer'},
            pages: {
              type: 'array',
              items: {
                type: 'object',
                additionalProperties: false,
                properties: {
                  name: {type: 'string'},
                  pageId: {type: 'string'},
                  folder: {type: 'string'},
                  summary: {type: 'string'},
                  stale: {type: 'boolean'},
                },
                required: ['name', 'pageId'],
              },
            },
          },
          required: ['ok', 'message', 'bankId', 'count', 'pages'],
        },
        render: (_args, value) => [{type: 'text', text: knowledgePagesValueToText(value)}],
      },
      async execute(_args, exec) {
        const entries = await fetchKnowledgePages(client, config, exec?.signal, logger);
        if (!entries) {
          throw new Error(`hindsight_list_pages failed: cannot reach the knowledge base at ${config.apiUrl}`);
        }
        const pages = entries.map(({page, folder}) => ({
          name: page.name,
          pageId: page.id,
          folder,
          summary: knowledgePageSummary(page),
          stale: page.is_stale === true,
        }));
        return {
          ok: true,
          message: pages.length
            ? `Knowledge pages (${pages.length}) in bank '${config.bankId}'.`
            : `No knowledge pages in bank '${config.bankId}'.`,
          bankId: config.bankId,
          count: pages.length,
          pages,
        };
      },
    });

    definitions.push({
      name: 'hindsight_search_pages',
      description:
        'Search the curated Hindsight knowledge pages by text (read-only). '
        + 'Hybrid keyword+semantic search over page bodies; returns small snippets. '
        + 'Use it when you know what you are looking for; use hindsight_list_pages when you do not.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          query: {type: 'string', description: 'What to search for in page bodies.'},
          limit: {type: 'integer', description: 'Maximum page hits (defaults to the configured page search limit).'},
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
                  pageId: {type: 'string'},
                  name: {type: 'string'},
                  snippet: {type: 'string'},
                  score: {type: 'number'},
                  updatedAt: {type: 'string'},
                },
                required: ['pageId', 'name'],
              },
            },
          },
          required: ['ok', 'message', 'bankId', 'count', 'results'],
        },
        render: (_args, value) => [{type: 'text', text: knowledgePageSearchValueToText(value)}],
      },
      async execute(args, exec) {
        const query = requireString(args, 'query');
        const limit = optionalInteger(args, 'limit', config.pageSearchLimit, 1) ?? config.pageSearchLimit;
        try {
          const response = await client.searchKnowledgePages({
            bankId: config.bankId,
            query,
            limit,
            signal: exec?.signal,
            timeoutMs: config.timeoutMs,
          });
          const results = (Array.isArray(response.results) ? response.results : []).map(hit => ({
            pageId: String(hit.id ?? ''),
            name: String(hit.name ?? ''),
            snippet: truncateText(String(hit.snippet ?? ''), 320),
            score: typeof hit.score === 'number' ? hit.score : 0,
            updatedAt: String(hit.updated_at ?? ''),
          }));
          return {
            ok: true,
            message: results.length
              ? `Found ${results.length} knowledge page(s) matching ${JSON.stringify(query)}.`
              : `No knowledge page matches ${JSON.stringify(query)}.`,
            bankId: config.bankId,
            count: results.length,
            results,
          };
        } catch (error) {
          throw new Error(`hindsight_search_pages failed: ${error instanceof Error ? error.message : error}`, {cause: error});
        }
      },
    });

    definitions.push({
      name: 'hindsight_read_page',
      description:
        'Read one curated Hindsight knowledge page (read-only). '
        + 'Accepts a page name (exact or unique substring) or a page id. '
        + 'The body is generated from memory and can be long (up to a few thousand chars); '
        + 'read only the pages a task actually needs, and use maxChars to keep it small.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          page: {type: 'string', description: "Page name (exact or unique substring) or page id ('kp-...')."},
          maxChars: {type: 'integer', description: 'Body character cap (defaults to the configured page read limit).'},
        },
        required: ['page'],
      },
      output: {
        schema: {
          type: 'object',
          additionalProperties: false,
          properties: {
            ok: {type: 'boolean'},
            message: {type: 'string'},
            bankId: {type: 'string'},
            name: {type: 'string'},
            pageId: {type: 'string'},
            folder: {type: 'string'},
            stale: {type: 'boolean'},
            truncated: {type: 'boolean'},
            body: {type: 'string'},
            updatedAt: {type: 'string'},
          },
          required: ['ok', 'message', 'bankId', 'name', 'pageId', 'body'],
        },
        render: (_args, value) => [{type: 'text', text: String((value as {body?: JsonValue;} | null)?.body ?? '')}],
      },
      async execute(args, exec) {
        const ref = requireString(args, 'page');
        const maxChars = optionalInteger(args, 'maxChars', config.pageReadMaxChars, 200) ?? config.pageReadMaxChars;
        const entries = await fetchKnowledgePages(client, config, exec?.signal, logger);
        if (!entries) {
          throw new Error(`hindsight_read_page failed: cannot reach the knowledge base at ${config.apiUrl}`);
        }
        const resolved = resolveKnowledgePage(entries, ref);
        if (!resolved.entry) {
          const hint = resolved.candidates && resolved.candidates.length
            ? ` Candidates: ${resolved.candidates.join('; ')}.`
            : ' Call hindsight_list_pages to see the available pages.';
          throw new Error(`hindsight_read_page: no page matches ${JSON.stringify(ref)}.${hint}`);
        }
        let page: KnowledgePage;
        try {
          page = await client.getKnowledgePage(config.bankId, resolved.entry.page.id, {
            signal: exec?.signal,
            timeoutMs: config.timeoutMs,
          });
        } catch (error) {
          throw new Error(`hindsight_read_page failed: ${error instanceof Error ? error.message : error}`, {cause: error});
        }
        const raw = String(page.body ?? page.markdown ?? '');
        const clipped = clip(raw, maxChars);
        return {
          ok: true,
          message: `Read knowledge page '${page.name}' (${raw.length} chars).`,
          bankId: config.bankId,
          name: String(page.name ?? resolved.entry.page.name),
          pageId: resolved.entry.page.id,
          folder: resolved.entry.folder,
          stale: resolved.entry.page.is_stale === true,
          truncated: clipped !== raw,
          body: clipped,
          updatedAt: String(page.timestamp ?? resolved.entry.page.timestamp ?? ''),
        };
      },
    });
  }

  // Write side: the agent opens ONE page per initiative. The page body is still
  // generated by the server from memory; this tool only creates the page and
  // writes the marker that tells the page's refresh what the work is.
  if (config.knowledgePagesEnabled && config.knowledgePageCaptureEnabled) {
    definitions.push({
      name: 'hindsight_capture_initiative',
      description:
        'Open (or update) the Hindsight knowledge page for a piece of feature work (write). '
        + 'Call it once AFTER the user approves a plan and BEFORE writing code: title is a short name, '
        + 'summary says what is being built or changed, why, and the current state. '
        + 'If the goal, scope or rationale materially changes later, call it again with relates_to_page_id '
        + 'set to the page id this tool returned — never create a second page for the same work. '
        + 'Skip it for bug fixes, small refactors, chores and trivial edits.',
      parameters: {
        type: 'object',
        additionalProperties: false,
        properties: {
          title: {type: 'string', description: 'Short human name for the work; becomes the page name.'},
          summary: {type: 'string', description: 'What is being built or changed, why, and its current state.'},
          relates_to_page_id: {
            type: 'string',
            description: 'Page id (or unique page name) this initiative already owns. Set it on a material plan change to update that page instead of creating another.',
          },
        },
        required: ['title', 'summary'],
      },
      output: {
        schema: {
          type: 'object',
          additionalProperties: false,
          properties: {
            ok: {type: 'boolean'},
            message: {type: 'string'},
            pageId: {type: 'string'},
            created: {type: 'boolean'},
            folderId: {type: 'string'},
            folder: {type: 'string'},
            markerDocumentId: {type: 'string'},
            operationId: {type: 'string'},
          },
          required: ['ok', 'message', 'pageId', 'created'],
        },
        render: (_args, value) => [{type: 'text', text: String((value as {message?: string;} | null)?.message ?? '')}],
      },
      async execute(args, exec) {
        const title = requireString(args, 'title').trim();
        const summary = requireString(args, 'summary').trim();
        const relates = optionalString(args, 'relates_to_page_id')?.trim() || undefined;
        const tags = mergeTags(
          [config.initiativeTierTag],
          [cwdTag(config.retainCwdTagPrefix, agentCwd(exec))],
          config.initiativeTags,
        );
        try {
          const result = await client.captureInitiative({
            bankId: config.bankId,
            title,
            summary,
            relatesToPageId: relates,
            folderName: config.initiativeFolder,
            tags,
            maxTokens: config.initiativePageMaxTokens,
            refreshCron: config.initiativeRefreshCron,
            metadata: agentMetadata(exec),
            signal: exec?.signal,
            timeoutMs: config.timeoutMs,
          });
          const value: Record<string, unknown> = {
            ok: true,
            message: result.created
              ? `Created initiative page '${title}' (${result.pageId}) under '${config.initiativeFolder}'. Its body is generated in the background; pass this page id as relates_to_page_id if the plan changes.`
              : `Updated initiative page ${result.pageId}: recorded the current intent. The body refreshes on its schedule.`,
            pageId: result.pageId,
            created: result.created,
            markerDocumentId: result.markerDocumentId,
          };
          if (result.folderId) {
            value.folderId = result.folderId;
            value.folder = config.initiativeFolder;
          }
          if (result.operationId) value.operationId = result.operationId;
          return value;
        } catch (error) {
          throw new Error(`hindsight_capture_initiative failed: ${error instanceof Error ? error.message : error}`, {cause: error});
        }
      },
    });
  }

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

/** Shorten text for a one-line summary without the generic clip marker. */
function truncateText(text: string, maxChars: number): string {
  const value = text.trim();
  if (maxChars <= 0 || value.length <= maxChars) return value;
  return `${value.slice(0, Math.max(1, maxChars - 1))}…`;
}

interface KnowledgePageEntry {
  page: KnowledgePageNode;
  folder: string;
}

/** Flatten a knowledge-page tree into pages, carrying their folder path. */
function flattenKnowledgePages(
  nodes: readonly KnowledgePageNode[] | undefined,
  folder = '',
  out: KnowledgePageEntry[] = [],
): KnowledgePageEntry[] {
  for (const node of nodes ?? []) {
    if (node.kind === 'folder') {
      const path = folder ? `${folder}/${node.name}` : node.name;
      flattenKnowledgePages(node.children, path, out);
    } else {
      out.push({page: node, folder});
    }
  }
  return out;
}

/**
 * One-line summary of a page. The server stores the full source query in
 * `description`, whose first sentence is the most descriptive part.
 */
function knowledgePageSummary(node: KnowledgePageNode, maxChars = 80): string {
  const raw = String(node.description ?? '').trim();
  if (!raw) return '';
  // The server stores the whole source_query here, including its "输出要求"
  // block. Keep the first paragraph, drop the "为仓库 X 产出" preamble, and
  // fall back to the whole paragraph when the first sentence is so short it
  // does not tell the agent whether the page is relevant.
  const firstParagraph = (raw.split(/\n\s*\n|\n输出要求/)[0] ?? raw).trim();
  const cleaned = firstParagraph
    .replace(/^为仓库\s+\S+\s+产出\s*/, '')
    .replace(/^产出\s+\S+\s+/, '')
    .replace(/^产出\s*/, '')
    .replace(/^的\s*/, '')
    .trim();
  const firstSentence = (cleaned.split(/[。;]/)[0] ?? cleaned).trim();
  const summary = firstSentence.length >= 12 ? firstSentence : cleaned;
  return truncateText(summary || firstParagraph || raw, maxChars);
}

/** Render the compact page roster injected at session start. */
function knowledgePageRoster(entries: KnowledgePageEntry[], config: HindsightConfig): string {
  const lines = entries.map(({page, folder}) => {
    const summary = knowledgePageSummary(page);
    const where = folder ? `  (${folder})` : '';
    return summary ? `- ${page.name}${where} — ${summary}` : `- ${page.name}${where}`;
  });
  const head = `Knowledge pages (${entries.length}, bank ${config.bankId}) — curated and read-only; pull one when it is relevant:`;
  const tail = 'hindsight_read_page({page:"<name or id>"}) reads one; '
    + 'hindsight_search_pages({query}) searches page bodies. '
    + 'Prefer the smallest source that answers the question; do not read every page.';
  return clip([head, ...lines, tail].join('\n'), config.pageRosterMaxChars);
}

const PAGE_REQUEST_TIMEOUT_MS = 8000;

/** Fetch and flatten the knowledge-page tree. Fail-open: undefined on error/timeout (both logged). */
async function fetchKnowledgePages(
  client: HindsightClient,
  config: HindsightConfig,
  signal: AbortSignal | undefined,
  logger: Logger,
): Promise<KnowledgePageEntry[] | undefined> {
  try {
    const tree = await Promise.race([
      client.knowledgePageTree({bankId: config.bankId, signal, timeoutMs: config.timeoutMs}),
      sleep(PAGE_REQUEST_TIMEOUT_MS).then(() => undefined),
    ]);
    if (!tree) {
      logger.warn(`hindsight: knowledge page tree fetch timed out after ${PAGE_REQUEST_TIMEOUT_MS}ms`);
      return undefined;
    }
    return flattenKnowledgePages(tree.roots);
  } catch (error) {
    logFailure(logger, 'hindsight knowledge page tree fetch', error);
    return undefined;
  }
}

/**
 * Resolve a page reference as a page id, a name, or a unique name substring.
 * Returns candidates when the reference is ambiguous or unknown.
 */
function resolveKnowledgePage(
  entries: KnowledgePageEntry[],
  ref: string,
): {entry?: KnowledgePageEntry; candidates?: string[];} {
  const needle = ref.trim();
  if (!needle) return {candidates: []};
  if (needle.startsWith('kp-') || needle.startsWith('kf-')) {
    const byId = entries.find(item => item.page.id === needle);
    return byId ? {entry: byId} : {candidates: []};
  }
  const exact = entries.filter(item => item.page.name === needle);
  if (exact.length === 1) return {entry: exact[0]!};
  if (exact.length > 1) return {candidates: exact.map(item => item.page.name)};
  const lower = needle.toLowerCase();
  const partial = entries.filter(item => item.page.name.toLowerCase().includes(lower));
  if (partial.length === 1) return {entry: partial[0]!};
  return {candidates: partial.map(item => item.page.name)};
}

/** Compact text for hindsight_list_pages. */
function knowledgePagesValueToText(value: JsonValue): string {
  const record = (value ?? {}) as {message?: JsonValue; pages?: Array<Record<string, JsonValue>>;} | null;
  const lines = [String(record?.message ?? '')];
  for (const page of record?.pages ?? []) {
    const parts = [`- ${String(page.name ?? '')}`];
    if (page.folder) parts.push(`(${String(page.folder)})`);
    if (page.summary) parts.push(`— ${String(page.summary)}`);
    parts.push(`  [${String(page.pageId ?? '')}]`);
    lines.push(parts.join(' '));
  }
  return lines.filter(Boolean).join('\n');
}

/** Compact text for hindsight_search_pages. */
function knowledgePageSearchValueToText(value: JsonValue): string {
  const record = (value ?? {}) as {message?: JsonValue; results?: Array<Record<string, JsonValue>>;} | null;
  const lines = [String(record?.message ?? '')];
  for (const hit of record?.results ?? []) {
    lines.push(`- ${String(hit.name ?? '')} [${String(hit.pageId ?? '')}]`);
    if (hit.snippet) lines.push(`  ${String(hit.snippet)}`);
  }
  return lines.filter(Boolean).join('\n');
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

/**
 * Recall with a soft timeout.
 *
 * `recallTimeoutMs` is a slow-call threshold, not a hard cutoff: a real bank's
 * recall is LLM-backed and measured 12.6–14.8s, so the old `Promise.race`
 * against it dropped every injection while the silent catch hid the reason.
 * The call now keeps waiting up to the client's `timeoutMs`; crossing the
 * threshold logs one warning and a later success logs its elapsed time.
 *
 * Fail-open: errors and aborts are logged and yield undefined so a pre-step
 * never breaks because memory is unavailable.
 */
async function recallWithTimeout(
  client: HindsightClient,
  config: HindsightConfig,
  query: string,
  signal: AbortSignal | undefined,
  logger: Logger,
): Promise<RecallResult[] | undefined> {
  const started = Date.now();
  let slowWarned = false;
  const slowTimer = setTimeout(() => {
    slowWarned = true;
    logger.warn(`hindsight: recall is slow (no response after ${config.recallTimeoutMs}ms), still waiting up to ${config.timeoutMs}ms`);
  }, config.recallTimeoutMs);
  try {
    const result = await client.recall({
      bankId: config.bankId,
      query: clip(query, config.recallMaxInputChars),
      budget: config.budget,
      maxTokens: config.recallMaxTokens,
      types: config.recallTypes,
      tags: config.recallTags,
      tagsMatch: config.recallTagsMatch,
      signal,
      timeoutMs: config.timeoutMs,
    });
    const elapsed = Date.now() - started;
    if (slowWarned) logger.info(`hindsight: recall completed in ${elapsed}ms`);
    const results = result?.results?.filter(r => typeof r.text === 'string' && r.text.trim());
    if (!results || results.length === 0) {
      logger.debug(`hindsight: recall returned no results in ${elapsed}ms`);
      return undefined;
    }
    return results;
  } catch (error) {
    if (signal?.aborted) logger.debug('hindsight: recall aborted with the agent');
    else logFailure(logger, 'hindsight recall', error);
    return undefined;
  } finally {
    clearTimeout(slowTimer);
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
    source: {kind: 'dsh-hindsight', form: 'recall'},
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
    source: {
      kind: 'dsh-hindsight',
      form: 'recall',
    },
  };
}

/** Build a user-role knowledge-page roster message for injection. */
function knowledgePagesMessage(text: string): UserMessage {
  const preamble = '<hindsight-knowledge-pages>\n# Hindsight Knowledge Pages (roster)\n\n';
  const footer = '\n</hindsight-knowledge-pages>';
  return {
    id: (`hindsight-pages-${Date.now()}-${++memoryMessageSeq}`) as UserMessage['id'],
    role: 'user',
    content: [{type: 'text', text: `${preamble}${text}${footer}`}],
    source: {kind: 'dsh-hindsight', form: 'recall'},
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
): Promise<boolean> {
  const results = await recallWithTimeout(deps.client, deps.config, query, signal, deps.logger);
  if (!results) return false;
  try {
    if (typeof agent.inject !== 'function') {
      deps.logger.warn('hindsight: agent.inject is unavailable, recall not injected');
      return false;
    }
    agent.inject(memoryMessage(recallResultsToText(results)));
    deps.logger.info(`hindsight recall injected for agent ${agent.id}`);
    return true;
  } catch (error) {
    deps.logger.warn(`hindsight recall injection failed: ${String(error)}`);
    return false;
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
  /**
   * Whether the server may refresh this model on its own. The user-preference
   * model keeps its automatic refresh; a project model does not unless
   * `mentalModelProjectAutoRefresh` is set.
   */
  autoRefresh: boolean;
}

function wantedMentalModels(config: HindsightConfig, cwd: string | undefined): MentalModelWanted[] {
  const wants: MentalModelWanted[] = [];
  if (config.mentalModelUserQuery.trim()) {
    wants.push({
      key: 'user',
      name: config.mentalModelUserQuery.trim(),
      sourceQuery: config.mentalModelUserQuery.trim(),
      tags: config.mentalModelUserTags,
      autoRefresh: true,
    });
  }
  if (config.mentalModelProjectEnabled && cwd && config.mentalModelProjectQueryTemplate.trim()) {
    const query = config.mentalModelProjectQueryTemplate.replace(/{cwd}/g, cwd);
    wants.push({
      key: 'project:' + cwd,
      name: '项目 ' + cwd,
      sourceQuery: query,
      // The project model is scoped to memories retained from this cwd. The
      // server defaults a tagged model to `tags_match: 'all_strict'`, so the
      // model reads ONLY memories carrying the same cwd tag — an untagged
      // memory (another project, or data retained before cwd tagging) is
      // invisible to it. Extra configured tags narrow the scope further.
      tags: mergeTags([cwdTag(config.retainCwdTagPrefix, cwd)], config.mentalModelProjectTags),
      autoRefresh: config.mentalModelProjectAutoRefresh,
    });
  }
  return wants;
}

/** Order-insensitive set equality for tag lists. */
function sameTags(left: string[] | undefined, right: string[] | undefined): boolean {
  const a = left ?? [];
  const b = right ?? [];
  if (a.length !== b.length) return false;
  const seen = new Set(a);
  return b.every(tag => seen.has(tag));
}

/**
 * Bring an existing mental model in line with the wanted scope and refresh mode.
 *
 * A model created before cwd tagging carries no tags, which makes it read the
 * WHOLE bank — every project, plus data from other clients sharing the bank.
 * Patching its tags scopes it to this cwd; the content is left alone, so the
 * model keeps its knowledge until the server's next scoped refresh.
 *
 * Only models with a non-empty wanted scope are reconciled: an empty wanted
 * list means "leave the scope exactly as it is", never "clear it". The refresh
 * switch is reconciled independently: a model whose `autoRefresh` is off has
 * its trigger patched to manual instead of being refreshed.
 */
async function reconcileMentalModel(
  client: HindsightClient,
  config: HindsightConfig,
  model: MentalModel,
  want: MentalModelWanted,
  logger: Logger,
  signal?: AbortSignal,
): Promise<MentalModel> {
  const patch: {tags?: string[]; trigger?: Record<string, unknown>;} = {};
  if (want.tags.length > 0 && !sameTags(model.tags, want.tags)) patch.tags = want.tags;
  const trigger = (model.trigger ?? {}) as Record<string, unknown>;
  const autoRefreshes = trigger.refresh_after_consolidation !== false
    || (trigger.refresh_cron !== undefined && trigger.refresh_cron !== null);
  if (!want.autoRefresh && autoRefreshes) {
    patch.trigger = {
      mode: trigger.mode ?? config.mentalModelRefreshMode,
      refresh_after_consolidation: false,
      refresh_cron: null,
    };
  }
  if (patch.tags === undefined && patch.trigger === undefined) return model;
  try {
    return await client.updateMentalModel({
      bankId: config.bankId,
      id: model.id,
      ...(patch.tags ? {tags: patch.tags} : {}),
      ...(patch.trigger ? {trigger: patch.trigger} : {}),
      signal,
      timeoutMs: config.mentalModelRequestTimeoutMs,
    });
  } catch (error) {
    // Fail-open: an un-reconciled model still injects (with its old, wider
    // scope) rather than dropping the mental model for this turn.
    logFailure(logger, 'hindsight mental model reconcile', error);
    return model;
  }
}

/**
 * Silence automatic refresh on project models the plugin owns.
 *
 * A project model carries a `cwd:<path>` tag. When project models are disabled
 * (the default) or explicitly manual, an already-created model would otherwise
 * keep reflecting after every consolidation — a recurring LLM cost with no
 * reader, because the model is no longer injected. Each matching model is
 * patched once per process; the user-preference model (no cwd tag) and
 * knowledge-page backed models (`project:*` / `knowledge:*` tags) are never
 * touched. Fail-open: a failed mute only logs.
 */
async function muteProjectModelAutoRefresh(
  client: HindsightClient,
  config: HindsightConfig,
  signal: AbortSignal | undefined,
  state: {scanned: boolean; muted: Set<string>;},
  logger: Logger,
): Promise<void> {
  // One sweep per process: later turns must not re-list mental models (the
  // same reason the injection itself is keyed per session).
  if (state.scanned || config.mentalModelProjectAutoRefresh) return;
  const prefix = config.retainCwdTagPrefix;
  if (!prefix) {
    state.scanned = true;
    return;
  }
  try {
    const list = await client.listMentalModels(config.bankId, {signal, timeoutMs: config.mentalModelRequestTimeoutMs});
    const items = Array.isArray(list.items) ? list.items : [];
    for (const item of items) {
      if (!item?.id || state.muted.has(item.id)) continue;
      const tags = Array.isArray(item.tags) ? item.tags.map(String) : [];
      if (!tags.some(tag => tag.startsWith(prefix))) continue;
      const model = await client.getMentalModel(config.bankId, item.id, {signal, timeoutMs: config.mentalModelRequestTimeoutMs});
      const trigger = (model.trigger ?? {}) as Record<string, unknown>;
      const autoRefreshes = trigger.refresh_after_consolidation !== false
        || (trigger.refresh_cron !== undefined && trigger.refresh_cron !== null);
      if (autoRefreshes) {
        await client.updateMentalModel({
          bankId: config.bankId,
          id: item.id,
          trigger: {
            mode: trigger.mode ?? config.mentalModelRefreshMode,
            refresh_after_consolidation: false,
            refresh_cron: null,
          },
          signal,
          timeoutMs: config.mentalModelRequestTimeoutMs,
        });
        logger.info('hindsight: disabled auto-refresh for project mental model ' + item.name);
      }
      state.muted.add(item.id);
    }
  } catch (error) {
    logger.warn('hindsight: project mental model auto-refresh mute failed: ' + String(error));
  } finally {
    // Never retry every turn: a failure is retried on the next process start.
    state.scanned = true;
  }
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
  logger: Logger,
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
        const model = await client.getMentalModel(config.bankId, found.id, {signal, timeoutMs: config.mentalModelRequestTimeoutMs});
        return await reconcileMentalModel(client, config, model, want, logger, signal);
      }
      if (!config.mentalModelAutoCreate) return undefined;
      const trigger: Record<string, unknown> = {
        mode: config.mentalModelRefreshMode,
        refresh_after_consolidation: want.autoRefresh,
      };
      // Spelled out even though the default is null: an explicit null makes the
      // "manual refresh only" intent visible in the stored model.
      if (!want.autoRefresh) trigger.refresh_cron = null;
      if (config.mentalModelFactTypes.length > 0) trigger.fact_types = config.mentalModelFactTypes;
      // Spelled out rather than relying on the server default: a tagged model
      // must exclude untagged memories, not merely OR its tags with them.
      if (want.tags.length > 0) trigger.tags_match = 'all_strict';
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
          if (status.status === 'failed') {
            logger.warn(`hindsight: mental model creation failed for "${want.name}" (operation ${operationId})`);
            return undefined;
          }
          await sleep(config.mentalModelPollIntervalMs);
        }
      }
      const id = created.mental_model_id;
      if (!id) {
        logger.warn(`hindsight: mental model creation returned no id for "${want.name}"`);
        return undefined;
      }
      return await client.getMentalModel(config.bankId, id, {signal, timeoutMs: config.mentalModelRequestTimeoutMs});
    } catch (error) {
      logFailure(logger, `hindsight mental model ensure (${want.name})`, error);
      return undefined;
    }
  })();
  creating.set(want.key, task);
  task.finally(() => {
    if (creating.get(want.key) === task) creating.delete(want.key);
  }).catch(error => logFailure(logger, 'hindsight mental model cleanup', error));
  return task;
}

/**
 * Background: ensure the user-preference mental model (and, only when
 * `mentalModelProjectEnabled` is on, the per-project one) exists and inject it
 * into the agent's context — at most once per session (agent). The injection is
 * keyed by agent id + wanted model, so every new session gets the current
 * settled knowledge exactly once, and later turns of the same session skip
 * the query entirely. When a model is not ready yet (auto-create still
 * reflecting) it is not marked, so a later turn retries. Fail-open: any
 * error only logs; the agent never waits on the memory fetch.
 *
 * Project models are off by default (knowledge pages carry project knowledge)
 * and are muted so an already-created one stops refreshing itself.
 */
async function mentalModelsAndInject(
  agent: Agent,
  signal: AbortSignal | undefined,
  deps: {client: HindsightClient; config: HindsightConfig; creatingMentalModels: Map<string, Promise<MentalModel | undefined>>; injectedMentalModels: Set<string>; projectMute: {scanned: boolean; muted: Set<string>;}; logger: Logger;},
): Promise<void> {
  if (!deps.config.autoMentalModel) return;
  await muteProjectModelAutoRefresh(deps.client, deps.config, signal, deps.projectMute, deps.logger);
  const cwd = typeof agent.session?.header?.cwd === 'string' ? agent.session.header.cwd : undefined;
  const wants = wantedMentalModels(deps.config, cwd);
  for (const want of wants) {
    const sessionKey = agent.id + ':' + want.key;
    if (deps.injectedMentalModels.has(sessionKey)) continue;
    const mm = await ensureMentalModel(deps.client, deps.config, want, deps.creatingMentalModels, deps.logger, signal);
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
 * Background: inject the compact knowledge-page roster at session start and
 * refresh it every N user turns. The roster is a map, not content: it lets the
 * agent know which pages exist so it can pull one deliberately, instead of the
 * plugin pushing page bodies into every turn. Fail-open.
 */
async function knowledgePagesAndInject(
  agent: Agent,
  turn: number,
  signal: AbortSignal | undefined,
  deps: {
    client: HindsightClient;
    config: HindsightConfig;
    rosterTurns: Map<string, number>;
    logger: Logger;
  },
): Promise<void> {
  if (!deps.config.knowledgePagesEnabled || !deps.config.injectPageRoster) return;
  const every = deps.config.pageRosterRefreshEveryTurns;
  const last = deps.rosterTurns.get(agent.id);
  if (last !== undefined && (every <= 0 || turn - last < every)) return;
  const entries = await fetchKnowledgePages(deps.client, deps.config, signal, deps.logger);
  if (!entries || entries.length === 0) return;
  try {
    if (typeof agent.inject !== 'function') {
      deps.logger.warn('hindsight: agent.inject is unavailable, page roster not injected');
      return;
    }
    agent.inject(knowledgePagesMessage(knowledgePageRoster(entries, deps.config)));
    deps.rosterTurns.set(agent.id, turn);
    deps.logger.info(`hindsight page roster injected (${entries.length} pages) for agent ${agent.id}`);
  } catch (error) {
    deps.logger.warn(`hindsight page roster injection failed: ${String(error)}`);
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
  deps: {
    client: HindsightClient;
    config: HindsightConfig;
    injectedTurns: Set<string>;
    creatingMentalModels: Map<string, Promise<MentalModel | undefined>>;
    injectedMentalModels: Set<string>;
    projectMute: {scanned: boolean; muted: Set<string>;};
    recallInjectedAgents: Set<string>;
    rosterTurns: Map<string, number>;
    logger: Logger;
  },
): Promise<PreStepDecision> {
  const pagesEnabled = deps.config.knowledgePagesEnabled;
  if (
    (!deps.config.autoRecall && !deps.config.autoMentalModel && !pagesEnabled)
    || deps.config.memoryMode === 'tools'
  ) return next();
  const decision = await next();
  if (decision.kind !== 'enter') return decision;
  if (decision.messages.length === 0) return decision;
  const key = agent.id + ':' + turn;
  if (deps.injectedTurns.has(key)) return decision;
  if (deps.config.skipSubagents && agent.session.header.origin === 'subagent') return decision;
  const query = extractUserQuery(messages);
  deps.injectedTurns.add(key);
  // Pull, don't push: with autoRecallOnce (the default) the first user turn of
  // a session gets one recall injection; later turns use the recall tool. The
  // 0.9.x benchmarks showed per-turn memory injection can hurt focus.
  const recallDue = deps.config.autoRecall
    && !!query
    && (!deps.config.autoRecallOnce || !deps.recallInjectedAgents.has(agent.id));
  if (recallDue) {
    void recallAndInject(agent, query, signal, deps)
      .then(injected => {
        if (injected) deps.recallInjectedAgents.add(agent.id);
      })
      .catch(error => logFailure(deps.logger, 'hindsight recall scheduling', error));
  }
  if (deps.config.autoMentalModel) {
    void mentalModelsAndInject(agent, signal, deps)
      .catch(error => logFailure(deps.logger, 'hindsight mental model injection', error));
  }
  if (pagesEnabled) {
    void knowledgePagesAndInject(agent, turn, signal, deps)
      .catch(error => logFailure(deps.logger, 'hindsight page roster injection', error));
  }
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
    logger,
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
  // One-shot sweep state: project models whose auto-refresh this process muted.
  const projectMute = {scanned: false, muted: new Set<string>()};
  // autoRecallOnce: agents that already received their one recall injection.
  const recallInjectedAgents = new Set<string>();
  // agent id -> the turn its page roster was last injected on.
  const rosterTurns = new Map<string, number>();

  function stateFor(sessionId: string): SessionState {
    let state = states.get(sessionId);
    if (!state) {
      state = {buffer: [], chain: Promise.resolve()};
      states.set(sessionId, state);
    }
    return state;
  }

  /**
   * Retain the buffered turns, dropping only the turns that actually landed.
   *
   * A batch used to be all-or-nothing for the caller: one failing item kept the
   * WHOLE buffer, so every later `turn/end` re-sent every turn before it, and
   * the batch could never drain while that one item kept failing. Retaining the
   * turns one at a time after a batch failure isolates the failure and leaves
   * only the turns that really failed buffered for the next retry.
   */
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
      await retainIndividually(session, state, records, items);
      return;
    }
    try {
      await retainAndWait(client, config, {bankId: config.bankId, items});
    } catch (error) {
      // With one item the batch IS the item: there is nothing to isolate, so let
      // the caller keep it buffered for the next completed turn to retry.
      if (items.length === 1) throw error;
      logger.warn(
        `hindsight retain of ${items.length} turn(s) failed for session ${session.id};`
        + ' retrying them one at a time',
      );
      await retainIndividually(session, state, records, items);
      return;
    }
    dropRetained(state, records);
    logger.info(`hindsight retained ${items.length} turn(s) for session ${session.id}`);
  }

  /** Retain one turn per request and forget each turn as soon as it lands. */
  async function retainIndividually(
    session: Session,
    state: SessionState,
    records: TurnRecord[],
    items: RetainItem[],
  ): Promise<void> {
    const failures: string[] = [];
    for (let index = 0; index < items.length; index += 1) {
      const item = items[index]!;
      const record = records[index]!;
      try {
        await retainAndWait(client, config, {bankId: config.bankId, items: [item]});
        dropRetained(state, [record]);
      } catch (error) {
        failures.push(`turn ${record.turn}: ${error instanceof Error ? error.message : String(error)}`);
      }
    }
    if (failures.length > 0) {
      logger.warn(
        `hindsight retained ${items.length - failures.length} of ${items.length} turn(s) for session ${session.id}`,
      );
      throw new Error(`${failures.length} of ${items.length} turn(s) failed — ${failures.join('; ')}`);
    }
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
    recallInjectedAgents.delete(String(session.id));
    rosterTurns.delete(String(session.id));
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
    const captureLine = config.knowledgePagesEnabled && config.knowledgePageCaptureEnabled
      ? 'Initiative pages (write): hindsight_capture_initiative({title, summary}) once a plan is approved and before code is written;'
      + ' on a material change to the goal, scope or rationale call it again with relates_to_page_id (never open a second page).'
      + ' Skip bug fixes, small refactors, chores and trivial edits.'
      : '';
    const pageLines = config.knowledgePagesEnabled
      ? [
        'Curated knowledge pages (read-only, generated from memory): hindsight_list_pages -> hindsight_search_pages -> hindsight_read_page.'
        + ' A page roster (names + one-line summaries) is injected at session start;'
        + ' use it to decide what to look up, then read only the pages a task needs — bodies are long.',
        ...(captureLine ? [captureLine] : []),
      ]
      : [];
    ctx.systemPrompt.section({
      name: 'hindsight:memory',
      order: TOOL_SECTION_ORDER,
      text: [
        '# Hindsight Memory',
        `Active. Bank: ${config.bankId}; recall budget: ${config.budget}.`,
        'Long-term memory is pull-based: query it when it helps instead of relying on it being pushed into every turn.',
        ...pageLines,
        'Use hindsight_recall to search raw memories, hindsight_reflect to synthesize across memories, and hindsight_retain to store information.',
        ...(statusLine ? [statusLine] : []),
        `
## 知识页读取协议

1. 开工前看知识页名册（已在上下文里，不要重复拉取）。名册里没有相关页 → 直接干活，
   不要为了"确认没有"去搜索。
2. 需要背景时：hindsight_search_pages(query = 任务里的名词, limit = 2)。
   只读命中页的前 40 行（TL;DR 段）；不够再读全文。
3. 每页每会话只读一次。
4. 一次任务最多读 2 页全文。要读第 3 页，先说明"现有两页缺了什么"。
5. 问"为什么这么设计" → Key decisions；问"模块/调用关系" → Component map；
   问"怎么写/怎么测/命名" → Conventions；问"精确值/某次事故/某人偏好" → recall。
6. 页与代码冲突：以代码为准。任务结束时 ingest 一条 Correction 页
   （标题 'Correction: <topic>'，正文三段：原说法 / 现事实 / 证据）。
7. 引用页内容时署名： From Hindsight memory(<page>): ...${config.knowledgePagesEnabled && config.knowledgePageCaptureEnabled ? `

## Initiative 页（写入）

- 计划获批、动手写代码之前：hindsight_capture_initiative({title, summary})。
  summary 写清"要做什么 / 为什么 / 当前状态"，不要只写一句标题。
- 目标、范围或理由发生实质变化：带 relates_to_page_id（首次调用返回的 pageId）再调一次，
  不要为同一件事新建第二页。
- bugfix、小改、重构、杂务：不调用。` : ''}`
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
      projectMute,
      recallInjectedAgents,
      rosterTurns,
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
          const snapshots = await sp.list();
          const sessions = snapshots.map(s => ({
            id: String(s.header.id),
            cwd: String(s.header.cwd ?? ''),
            createdAt: String(s.header.createdAt),
          }));
          if (sessions.length === 0) return {kind: 'success', text: 'No importable sessions found.'};
          const lines = sessions.map(s => `  ${s.id}(cwd: ${s.cwd}, created: ${s.createdAt})`);
          return {kind: 'success', text: `Found ${sessions.length} session(s): \n${lines.join('\n')}`};
        }

        // Import mode.
        const sessionId = args.sessionId as SessionId;
        // 0.1.7 removed `inspect`: a stored session is read through a read-only
        // handle whose `read()` returns its contiguous logical log slice.
        const handle = await sp.open(sessionId, 'read');
        let events: SessionEvent[];
        try {
          const slice = await handle.read();
          events = Array.isArray(slice.events) ? [...slice.events] : ([] as SessionEvent[]);
        } finally {
          await handle.close();
        }
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
            tags: mergeTags(config.retainTags, [`session: ${sessionId}`]),
          };
          try {
            await retainAndWait(client, config, {
              bankId: targetBank,
              items: [item],
            });
            retained++;
          } catch (error) {
            logFailure(logger, `hindsight -import retain turn ${record.turn}`, error);
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
    for (const definition of toolDefinitions(client, config, logger)) {
      ctx.tools.register(definition);
    }
  }
}
