/**
 * Turn-transcript projection for automatic retention and recall queries.
 *
 * `session.snapshotEvents()` (dsh-session >= 0.1.2-rc.1) / `session.events`
 * (older versions) is the durable, append-only source of truth.
 * user/message events do NOT carry a `turn` field in their data;
 * only assistant/message and tool/result do.  We track the current
 * turn by scanning for turn/start and turn/end markers.
 */

import type {HindsightConfig} from './config.js';
import type {Session, SessionEvent} from '@deepseek-ai/dsh-session';

/**
 * Structural adapter over the session event log.  dsh-session 0.1.2-rc.1
 * replaced the `events` getter with `snapshotEvents()`; older versions (and
 * test mocks) still expose `events`.  Prefer the new API when present.
 */
interface SessionEventSource {
  snapshotEvents?(fromSeq?: number, toSeqExclusive?: number): readonly SessionEvent[];
  events?: readonly SessionEvent[];
}

function sessionEvents(session: Session): readonly SessionEvent[] {
  const source = session as unknown as SessionEventSource;
  if (typeof source.snapshotEvents === 'function') return source.snapshotEvents();
  return source.events ?? [];
}

export interface TurnMessage {
  role: 'user' | 'assistant';
  content: string;
  timestamp: string;
}

export interface TurnRecord {
  turn: number;
  messages: TurnMessage[];
  text: string;
  query: string;
  startedAt: string;
}

interface ContentBlockLike {
  type?: string;
  text?: string;
  name?: string;
  arguments?: string;
  content?: ContentBlockLike[];
  [key: string]: unknown;
}

function textOfBlocks(content: unknown): string {
  if (typeof content === 'string') return content;
  if (!Array.isArray(content)) return '';
  const parts: string[] = [];
  for (const block of content as ContentBlockLike[]) {
    if (!block || typeof block !== 'object') continue;
    if (block.type === 'text' && typeof block.text === 'string') parts.push(block.text);
    if (block.type === 'tool-call') {
      parts.push(`[tool call ${String(block.name ?? 'unknown')}(${String(block.arguments ?? '')})]`);
    }
  }
  return parts.filter(part => part.trim()).join('\n').trim();
}

function toolResultText(content: unknown): string {
  if (!Array.isArray(content)) return '';
  const parts: string[] = [];
  for (const block of content as ContentBlockLike[]) {
    if (!block || typeof block !== 'object') continue;
    if (block.type === 'tool-result' && Array.isArray(block.content)) parts.push(textOfBlocks(block.content));
  }
  return parts.filter(Boolean).join('\n').trim();
}

function clip(text: string, maxChars: number): string {
  if (maxChars <= 0 || text.length <= maxChars) return text;
  return `${text.slice(0, maxChars - 20)}\n…[truncated]`;
}

/**
 * Collect every event that belongs to `turn` by tracking turn/start
 * boundaries.  user/message events do not carry `turn` in their data
 * (harness 0.1.0-rc.6); they are attributed to the current turn.
 */
function eventsForTurn(events: readonly SessionEvent[], turn: number): SessionEvent[] {
  const out: SessionEvent[] = [];
  let currentTurn: number | null = null;
  for (const event of events) {
    if (event.type === 'turn/start') {
      currentTurn = Number(event.data.turn);
      if (currentTurn === turn) continue;
      if (out.length > 0) break; // past the target turn
      continue;
    }
    if (event.type === 'turn/end') {
      if (Number(event.data.turn) === turn) break;
      continue;
    }
    if (currentTurn === turn) out.push(event);
  }
  return out;
}

/**
 * Build one retainable turn record from the session event log.
 */
export function buildTurnRecord(session: Session, turn: number, config: HindsightConfig): TurnRecord | null {
  const events = eventsForTurn(sessionEvents(session), turn);
  const messages: TurnMessage[] = [];
  let query = '';
  let totalChars = 0;
  let startedAt = '';

  for (const event of events) {
    if (event.type === 'user/message' && event.data.source.kind === 'user') {
      const text = clip(textOfBlocks(event.data.content), config.retainMaxChars);
      if (!text) continue;
      if (!query) query = text;
      if (!startedAt) startedAt = new Date(event.time).toISOString();
      const content = `${config.retainUserPrefix}: ${text}`;
      totalChars += content.length;
      messages.push({
        role: 'user',
        content: totalChars <= config.retainMaxChars ? content : '',
        timestamp: new Date(event.time).toISOString(),
      });
      continue;
    }

    if (event.type === 'assistant/message') {
      const text = textOfBlocks(event.data.message.content);
      const content = text ? `${config.retainAssistantPrefix}: ${text}` : '';
      if (!content) continue;
      totalChars += content.length;
      messages.push({
        role: 'assistant',
        content: totalChars <= config.retainMaxChars ? content : '',
        timestamp: new Date(event.time).toISOString(),
      });
      continue;
    }

    if (config.includeToolResults && event.type === 'tool/result') {
      const text = clip(toolResultText(event.data.message.content), config.retainMaxChars);
      if (!text) continue;
      totalChars += text.length;
      messages.push({
        role: 'user',
        content: `[tool result] ${totalChars <= config.retainMaxChars ? text : ''}`,
        timestamp: new Date(event.time).toISOString(),
      });
    }
  }

  const nonEmpty = messages.filter(message => message.content);
  if (nonEmpty.length === 0) return null;

  query = clip(query, config.recallMaxInputChars);
  return {
    turn,
    messages: nonEmpty,
    text: JSON.stringify(nonEmpty),
    query,
    startedAt: startedAt || new Date().toISOString(),
  };
}

/** Extract the user query that should drive the next-turn recall. */
export function turnQuery(session: Session, turn: number, config: HindsightConfig): string {
  const events = eventsForTurn(sessionEvents(session), turn);
  for (const event of events) {
    if (event.type !== 'user/message') continue;
    if (event.data.source.kind !== 'user') continue;
    const text = textOfBlocks(event.data.content);
    if (text) return clip(text, config.recallMaxInputChars);
  }
  return '';
}

/**
 * Build TurnRecord[] from the full event log, grouped by turn/start..turn/end
 * boundaries.  Only turns whose `reason.kind` is in `config.retainTurnKinds`
 * are included.  This is the reusable export used by the new
 * /hindsight-import slash command.
 */
export function buildTurnRecordsFromEvents(
  events: readonly SessionEvent[],
  config: HindsightConfig,
): TurnRecord[] {
  const records: TurnRecord[] = [];
  let currentTurn: number | null = null;
  let turnEvents: SessionEvent[] = [];
  let turnReasonKind = '';

  const flush = () => {
    if (currentTurn == null || turnEvents.length === 0) return;
    if (!config.retainTurnKinds.includes(turnReasonKind)) return;
    const messages: TurnMessage[] = [];
    let query = '';
    let totalChars = 0;
    let startedAt = '';

    for (const event of turnEvents) {
      if (event.type === 'user/message' && event.data.source.kind === 'user') {
        const text = clip(textOfBlocks(event.data.content), config.retainMaxChars);
        if (!text) continue;
        if (!query) query = text;
        if (!startedAt) startedAt = new Date(event.time).toISOString();
        const content = `${config.retainUserPrefix}: ${text}`;
        totalChars += content.length;
        messages.push({
          role: 'user',
          content: totalChars <= config.retainMaxChars ? content : '',
          timestamp: new Date(event.time).toISOString(),
        });
        continue;
      }
      if (event.type === 'assistant/message') {
        const text = textOfBlocks(event.data.message.content);
        const content = text ? `${config.retainAssistantPrefix}: ${text}` : '';
        if (!content) continue;
        totalChars += content.length;
        messages.push({
          role: 'assistant',
          content: totalChars <= config.retainMaxChars ? content : '',
          timestamp: new Date(event.time).toISOString(),
        });
        continue;
      }
      if (config.includeToolResults && event.type === 'tool/result') {
        const text = clip(toolResultText(event.data.message.content), config.retainMaxChars);
        if (!text) continue;
        totalChars += text.length;
        messages.push({
          role: 'user',
          content: `[tool result] ${totalChars <= config.retainMaxChars ? text : ''}`,
          timestamp: new Date(event.time).toISOString(),
        });
      }
    }

    const nonEmpty = messages.filter(m => m.content);
    if (nonEmpty.length === 0) return;
    records.push({
      turn: currentTurn,
      messages: nonEmpty,
      text: JSON.stringify(nonEmpty),
      query: clip(query, config.recallMaxInputChars),
      startedAt: startedAt || new Date().toISOString(),
    });
  };

  for (const event of events) {
    if (event.type === 'turn/start') {
      flush();
      currentTurn = Number(event.data.turn);
      turnEvents = [];
      turnReasonKind = '';
      continue;
    }
    if (event.type === 'turn/end') {
      turnReasonKind = event.data.reason.kind;
      flush();
      currentTurn = null;
      turnEvents = [];
      continue;
    }
    if (currentTurn != null) turnEvents.push(event);
  }
  // flush any trailing turn that never got a turn/end
  if (currentTurn != null) {
    flush();
  }

  return records;
}
