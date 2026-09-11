import test from 'node:test'
import assert from 'node:assert/strict'
import { mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { homedir, tmpdir } from 'node:os'
import { join } from 'node:path'
import { expandHome, formatLogMessage, installFileLogger } from '../src/logger.js'
import type { Context, Exporter, Logger, Message } from '@deepseek-ai/cordis'

function makeMessage(partial: Partial<Message> = {}): Message {
  return {
    sn: 1,
    ts: new Date('2026-09-10T20:26:40.000Z').getTime(),
    name: 'hindsight',
    type: 'debug',
    level: 3,
    args: [],
    ...partial,
  }
}

function noopLogger(): Logger {
  return { error() {}, info() {}, warn() {}, debug() {} } as unknown as Logger
}

test('expandHome replaces a leading ~ with the home directory', () => {
  assert.equal(expandHome('~/logs'), homedir() + '/logs')
  assert.equal(expandHome('~'), homedir())
  assert.equal(expandHome('C:\\logs'), 'C:\\logs')
  assert.equal(expandHome('/var/log'), '/var/log')
})

test('formatLogMessage renders placeholders', () => {
  assert.equal(formatLogMessage(makeMessage({ args: ['hello %s %d', 'world', 42] })), 'hello world 42')
  assert.equal(formatLogMessage(makeMessage({ args: ['%o', { a: 1 }] })), '{"a":1}')
  assert.equal(formatLogMessage(makeMessage({ args: ['100%%'] })), '100%')
  assert.equal(formatLogMessage(makeMessage({ args: ['plain'] })), 'plain')
  assert.equal(formatLogMessage(makeMessage({ args: [] })), '')
  assert.equal(formatLogMessage(makeMessage({ args: [42] })), '42')
  // color decoration %c is consumed and dropped
  assert.equal(formatLogMessage(makeMessage({ args: ['warn %c message', 'red', 'tail'] })), 'warn  message tail')
})

test('formatLogMessage renders an Error head as its stack', () => {
  const error = new Error('boom')
  const text = formatLogMessage(makeMessage({ args: [error] }))
  assert.match(text, /boom/)
  assert.match(text, /Error: boom/)
})

test('formatLogMessage truncates over-long lines', () => {
  const long = 'x'.repeat(20000)
  const text = formatLogMessage(makeMessage({ args: [long] }))
  assert.ok(text.length < long.length)
  assert.match(text, /…\[truncated\]$/)
})

test('installFileLogger registers a file exporter and writes lines', () => {
  const dir = mkdtempSync(join(tmpdir(), 'hindsight-log-'))
  try {
    const exporters: Exporter[] = []
    const service = Object.assign((_name?: string) => noopLogger(), {
      exporter: (exporter: Exporter) => {
        exporters.push(exporter)
        return () => {}
      },
    })
    installFileLogger({ logger: service } as unknown as Context, dir, noopLogger())
    assert.equal(exporters.length, 1)

    exporters[0]!.export(makeMessage({
      ts: new Date('2026-09-10T20:26:40.000Z').getTime(),
      type: 'info',
      level: 1,
      name: 'hindsight',
      args: ['recall injected for agent %s', 'a-1'],
    }))

    const content = readFileSync(join(dir, 'hindsight.log'), 'utf8')
    assert.match(content, /2026-09-10T/)
    assert.match(content, /\[info\] hindsight: recall injected for agent a-1/)
  } finally {
    rmSync(dir, { recursive: true, force: true })
  }
})

test('installFileLogger no-ops when logDir is empty or the service lacks exporter', () => {
  const exporters: Exporter[] = []
  const service = Object.assign((_name?: string) => noopLogger(), {
    exporter: (exporter: Exporter) => {
      exporters.push(exporter)
      return () => {}
    },
  })
  installFileLogger({ logger: service } as unknown as Context, '', noopLogger())
  assert.equal(exporters.length, 0)
  installFileLogger({} as unknown as Context, '/tmp/hindsight', noopLogger())
  assert.equal(exporters.length, 0)
})
