/**
 * File logging for dsh-hindsight.
 *
 * dsh hosts register no cordis logger sink, so without an exporter every
 * hindsight log line only lands in the in-memory logger buffer. This module
 * installs a file exporter that persists structured cordis log messages to
 * <logDir>/hindsight.log, and renders them with a plain-text formatter
 * mirroring Logger.format() for the placeholder subset the plugin uses
 * (colors are dropped — this is a log file, not a terminal).
 */

import { mkdirSync, appendFileSync } from 'node:fs'
import { homedir } from 'node:os'
import { join } from 'node:path'
import type { Context, Exporter, Logger, Message } from '@deepseek-ai/cordis'

/** Cap per-line length (mirrors the cordis exporter default). */
const MAX_LINE_LENGTH = 10240

/** Expand a leading ~ to the current user's home directory (dsh-lsp style). */
export function expandHome(path: string): string {
  return path.replace(/^~(?=$|[\\/])/, homedir())
}

function safeString(value: unknown): string {
  if (typeof value === 'string') return value
  if (value instanceof Error) return value.stack ?? String(value)
  try {
    return JSON.stringify(value) ?? String(value)
  } catch {
    return String(value)
  }
}

/**
 * Render a cordis log message as a single plain-text line. Handles the
 * printf-style placeholders cordis logs use: %s %d %i %f %o %O %c %C %%.
 * An Error first argument renders its stack (like Logger.format does);
 * %c/%C color decorations are consumed and dropped.
 */
export function formatLogMessage(message: Message): string {
  const args = message.args.slice()
  if (args.length === 0) return ''
  let template: string
  let rest: unknown[]
  const head = args[0]
  if (head instanceof Error) {
    template = '%s'
    rest = [head.stack ?? String(head)]
  } else if (typeof head === 'string') {
    template = head
    rest = args.slice(1)
  } else {
    template = '%o'
    rest = args
  }
  let out = ''
  let cursor = 0
  let index = 0
  const placeholder = /%[sdifocO%]/g
  let match: RegExpExecArray | null
  while ((match = placeholder.exec(template)) !== null) {
    out += template.slice(cursor, match.index)
    const token = match[0]
    if (token === '%%') {
      out += '%'
    } else if (token === '%c' || token === '%C') {
      index++ // color decoration is dropped in a plain-text file
    } else if (token === '%o' || token === '%O') {
      out += safeString(rest[index++])
    } else {
      out += String(rest[index++] ?? '')
    }
    cursor = match.index + token.length
  }
  out += template.slice(cursor)
  if (index < rest.length) {
    out += ' ' + rest.slice(index).map(safeString).join(' ')
  }
  return out.length > MAX_LINE_LENGTH
    ? `${out.slice(0, MAX_LINE_LENGTH)}…[truncated]`
    : out
}

/**
 * Install a file exporter on the cordis logger service. The exporter is
 * disposed with the plugin's fiber automatically (ctx.logger.exporter()
 * registers the cleanup via ctx.effect). Logging is fail-open: any setup or
 * write failure is reported once and never breaks the plugin.
 *
 * @param ctx     the plugin context (its logger service must exist)
 * @param logDir  target directory; empty string disables file logging
 * @param logger  a hindsight logger for reporting setup failures
 */
export function installFileLogger(ctx: Context, logDir: string, logger: Logger): void {
  if (!logDir) return
  const service = ctx.logger as unknown as (Logger & { exporter?: (exporter: Exporter) => void }) | undefined
  if (!service || typeof service.exporter !== 'function') return
  let file: string
  try {
    const dir = expandHome(logDir)
    mkdirSync(dir, { recursive: true })
    file = join(dir, 'hindsight.log')
  } catch (error) {
    logger.warn(`hindsight: cannot create log directory ${JSON.stringify(logDir)}: ${(error as Error).message}`)
    return
  }
  // levels.default=3 (DEBUG) captures every severity; per-name overrides can
  // be layered on top by other exporters without affecting this sink.
  service.exporter({
    levels: { default: 3 },
    export(message: Message): void {
      try {
        const line = `${new Date(message.ts).toISOString()} [${message.type}] ${message.name}: ${formatLogMessage(message)}\n`
        appendFileSync(file, line, 'utf8')
      } catch {
        // Never let logging break the plugin; drop the line.
      }
    },
  })
}
