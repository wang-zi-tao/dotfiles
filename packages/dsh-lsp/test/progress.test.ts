import test from 'node:test'
import { deepEqual, equal, match } from 'node:assert/strict'

import { foldIndexProgress, indexProgressMessage, progressToken, renderIndexProgress } from '../src/progress.js'
import type { IndexProgress } from '../src/types.js'

const TOKEN = 'backgroundIndexProgress'

function begin(previous: IndexProgress | undefined = undefined, now = 1000): IndexProgress {
  const next = foldIndexProgress(previous, { token: TOKEN, value: { kind: 'begin', title: 'indexing', percentage: true } }, now)
  if (!next) throw new Error('begin was not folded')
  return next
}

test('progressToken stringifies numeric tokens and rejects token-less params', () => {
  equal(progressToken({ token: TOKEN }), TOKEN)
  equal(progressToken({ token: 7 }), '7')
  equal(progressToken({ token: '' }), null)
  equal(progressToken({}), null)
  equal(progressToken(null), null)
})

test('begin resets the counters of a previous round', () => {
  const reported = foldIndexProgress(
    begin(),
    { token: TOKEN, value: { kind: 'report', percentage: 40, message: '2/5' } },
    1500,
  )!
  const restarted = foldIndexProgress(reported, { token: TOKEN, value: { kind: 'begin', title: 'indexing' } }, 2000)!
  equal(restarted.done, false)
  equal(restarted.percentage, null)
  equal(restarted.completed, null)
  equal(restarted.enqueued, null)
  equal(restarted.title, 'indexing')
})

test('report parses clangd\'s "completed/enqueued" message', () => {
  const next = foldIndexProgress(
    begin(),
    { token: TOKEN, value: { kind: 'report', percentage: 42, message: '210/500' } },
    2500,
  )!
  equal(next.percentage, 42)
  equal(next.completed, 210)
  equal(next.enqueued, 500)
  equal(next.done, false)
  equal(next.updatedAt, 2500)
})

test('report derives the percentage when the server only sends counters', () => {
  const next = foldIndexProgress(begin(), { token: TOKEN, value: { kind: 'report', message: '1/4' } }, 2600)!
  equal(next.percentage, 25)
})

test('report clamps an out-of-range percentage', () => {
  const next = foldIndexProgress(begin(), { token: TOKEN, value: { kind: 'report', percentage: 250 } }, 2700)!
  equal(next.percentage, 100)
})

test('end marks the stream done and promotes the enqueued total', () => {
  const reported = foldIndexProgress(
    begin(),
    { token: TOKEN, value: { kind: 'report', percentage: 42, message: '210/500' } },
    2000,
  )!
  const done = foldIndexProgress(reported, { token: TOKEN, value: { kind: 'end' } }, 3000)!
  equal(done.done, true)
  equal(done.percentage, 100)
  equal(done.completed, 500)
  equal(done.enqueued, 500)
})

test('end without prior counters still reports done', () => {
  const done = foldIndexProgress(undefined, { token: TOKEN, value: { kind: 'end' } }, 3000)!
  equal(done.done, true)
  equal(done.completed, null)
  equal(done.enqueued, null)
})

test('a foreign token does not inherit another stream\'s state', () => {
  const other = foldIndexProgress(begin(), { token: 'other', value: { kind: 'report', percentage: 10, message: '1/10' } }, 2000)!
  equal(other.token, 'other')
  equal(other.completed, 1)
})

test('unknown kinds, missing values and token-less params are ignored', () => {
  equal(foldIndexProgress(undefined, { token: TOKEN, value: { kind: 'nonsense' } }), null)
  equal(foldIndexProgress(undefined, { token: TOKEN, value: null }), null)
  equal(foldIndexProgress(undefined, { value: { kind: 'begin' } }), null)
})

test('renderIndexProgress renders running, counted and done states', () => {
  equal(renderIndexProgress(begin()), 'indexing')
  const reported = foldIndexProgress(begin(), { token: TOKEN, value: { kind: 'report', percentage: 42, message: '210/500' } }, 2000)!
  equal(renderIndexProgress(reported), 'indexing 42% (210/500)')
  const done = foldIndexProgress(reported, { token: TOKEN, value: { kind: 'end' } }, 3000)!
  equal(renderIndexProgress(done), 'indexing done (500/500)')
})

test('indexProgressMessage names the server, the root and the retry', () => {
  const reported = foldIndexProgress(begin(), { token: TOKEN, value: { kind: 'report', message: '500/500' } }, 2000)!
  const message = indexProgressMessage('clangd', 'D:/wpsmain', reported)
  equal(message.role, 'user')
  deepEqual(message.source, { kind: 'dsh-lsp', form: 'notice', summary: 'LSP 后台索引就绪: clangd (D:/wpsmain)' })
  const text = (message.content[0] as { type: string; text: string }).text
  match(text, /后台索引已完成 \(clangd, root=D:\/wpsmain, 500 个编译单元\)/)
  match(text, /workspaceSymbol 现在可以命中/)
  match(text, /"No symbols\." 的查询可以重试/)
})
