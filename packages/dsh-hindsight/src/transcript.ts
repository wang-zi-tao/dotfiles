/**
 * Turn-transcript projection for automatic retention and recall queries.
 *
 * `session.events` is the durable, append-only source of truth; message data
 * on user/message, assistant/message and tool/result events also carries the
 * owning turn/step numbers, so a single turn can be reconstructed without
 * depending on `deriveMessages()` or compaction surface rewrites.
 */

import type { HindsightConfig } from './config.js'
import type { Session, SessionEvent } from '@deepseek-ai/dsh-session'

export interface TurnMessage {
  role: 'user' | 'assistant'
  content: string
  timestamp: string
}

export interface TurnRecord {
  turn: number
  messages: TurnMessage[]
  text: string
  query: string
  startedAt: string
}

interface ContentBlockLike {
  type?: string
  text?: string
  name?: string
  arguments?: string
  content?: ContentBlockLike[]
  [key: string]: unknown
}

function textOfBlocks(content: unknown): string {
  if (typeof content === 'string') return content
  if (!Array.isArray(content)) return ''
  const parts: string[] = []
  for (const block of content as ContentBlockLike[]) {
    if (!block || typeof block !== 'object') continue
    if (block.type === 'text' && typeof block.text === 'string') parts.push(block.text)
    if (block.type === 'tool-call') {
      parts.push(`[tool call ${String(block.name ?? 'unknown')}(${String(block.arguments ?? '')})]`)
    }
  }
  return parts.filter(part => part.trim()).join('\n').trim()
}

function toolResultText(content: unknown): string {
  if (!Array.isArray(content)) return ''
  const parts: string[] = []
  for (const block of content as ContentBlockLike[]) {
    if (!block || typeof block !== 'object') continue
    if (block.type === 'tool-result' && Array.isArray(block.content)) parts.push(textOfBlocks(block.content))
  }
  return parts.filter(Boolean).join('\n').trim()
}

function clip(text: string, maxChars: number): string {
  if (maxChars <= 0 || text.length <= maxChars) return text
  return `${text.slice(0, maxChars - 20)}\n…[truncated]`
}

function eventsForTurn(events: readonly SessionEvent[], turn: number): SessionEvent[] {
  return events.filter(event => ('turn' in event.data) ? Number(event.data.turn) === Number(turn) : false)
}

/**
 * Build one retainable turn record from the session event log.
 */
export function buildTurnRecord(session: Session, turn: number, config: HindsightConfig): TurnRecord | null {
  const events = eventsForTurn(session?.events ?? [], turn)
  const messages: TurnMessage[] = []
  let query = ''
  let totalChars = 0
  let startedAt = ''

  for (const event of events) {
    if (event.type === 'user/message' && event.data?.source?.kind === 'user') {
      const text = clip(textOfBlocks(event.data?.content), config.retainMaxChars)
      if (!text) continue
      if (!query) query = text
      if (!startedAt) startedAt = new Date(event.time).toISOString()
      const content = `${config.retainUserPrefix}: ${text}`
      totalChars += content.length
      messages.push({
        role: 'user',
        content: totalChars <= config.retainMaxChars ? content : '',
        timestamp: new Date(event.time).toISOString(),
      })
      continue
    }

    if (event.type === 'assistant/message') {
      const text = textOfBlocks(event.data?.message?.content)
      const content = text ? `${config.retainAssistantPrefix}: ${text}` : ''
      if (!content) continue
      totalChars += content.length
      messages.push({
        role: 'assistant',
        content: totalChars <= config.retainMaxChars ? content : '',
        timestamp: new Date(event.time).toISOString(),
      })
      continue
    }

    if (config.includeToolResults && event.type === 'tool/result') {
      const text = clip(toolResultText(event.data?.message?.content), config.retainMaxChars)
      if (!text) continue
      totalChars += text.length
      messages.push({
        role: 'user',
        content: `[tool result] ${totalChars <= config.retainMaxChars ? text : ''}`,
        timestamp: new Date(event.time).toISOString(),
      })
    }
  }

  const nonEmpty = messages.filter(message => message.content)
  if (nonEmpty.length === 0) return null

  query = clip(query, config.recallMaxInputChars)
  return {
    turn,
    messages: nonEmpty,
    text: JSON.stringify(nonEmpty),
    query,
    startedAt: startedAt || new Date().toISOString(),
  }
}

/** Extract the user query that should drive the next-turn recall. */
export function turnQuery(session: Session, turn: number, config: HindsightConfig): string {
  const events = eventsForTurn(session?.events ?? [], turn)
  for (const event of events) {
    if (event.type !== 'user/message') continue
    if (event.data?.source?.kind !== 'user') continue
    const text = textOfBlocks(event.data?.content)
    if (text) return clip(text, config.recallMaxInputChars)
  }
  return ''
}
