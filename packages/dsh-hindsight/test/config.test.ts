import test from 'node:test'
import assert from 'node:assert/strict'
import { DEFAULTS, resolveConfig } from '../src/config.js'

const emptyEnv: NodeJS.ProcessEnv = {}

test('resolveConfig applies defaults', () => {
  const config = resolveConfig({}, emptyEnv)
  assert.equal(config.apiUrl, DEFAULTS.apiUrl)
  assert.equal(config.bankId, 'dsh')
  assert.equal(config.budget, 'mid')
  assert.equal(config.memoryMode, 'hybrid')
  assert.equal(config.timeoutMs, 120000)
})

test('resolveConfig accepts snake_case aliases and env overrides', () => {
  const config = resolveConfig(
    { api_url: 'http://127.0.0.1:8888', bank_id: 'work', recall_budget: 'low' },
    { HINDSIGHT_BANK_ID: 'env-bank', HINDSIGHT_BUDGET: 'high' },
  )
  assert.equal(config.apiUrl, 'http://127.0.0.1:8888')
  assert.equal(config.bankId, 'env-bank')
  assert.equal(config.budget, 'high')
})

test('resolveConfig normalizes tags and types', () => {
  const config = resolveConfig({
    retainTags: 'a:1, b:2,a:1',
    recallTypes: 'world, experience',
  }, emptyEnv)
  assert.deepEqual(config.retainTags, ['a:1', 'b:2'])
  assert.deepEqual(config.recallTypes, ['world', 'experience'])
})

test('resolveConfig logDir defaults, aliases, env, and disable', () => {
  assert.equal(resolveConfig({}, emptyEnv).logDir, '~/.dsh/logs/dsh-hindsight')
  assert.equal(resolveConfig({ log_dir: '/tmp/hindsight' }, emptyEnv).logDir, '/tmp/hindsight')
  assert.equal(resolveConfig({}, { HINDSIGHT_LOG_DIR: '/var/log/hindsight' }).logDir, '/var/log/hindsight')
  assert.equal(resolveConfig({ logDir: '' }, emptyEnv).logDir, '')
})

test('resolveConfig rejects invalid values', () => {
  assert.throws(() => resolveConfig({ bankId: '' }, emptyEnv), /bankId/)
  assert.throws(() => resolveConfig({ budget: 'extreme' }, emptyEnv), /budget/)
  assert.throws(() => resolveConfig({ apiUrl: 'ftp://x' }, emptyEnv), /apiUrl/)
  assert.throws(() => resolveConfig({ retainEveryNTurns: 0 }, emptyEnv), /retainEveryNTurns/)
})
