/**
 * format.ts unit tests — pure markdown renderers, no Neovim or DAP needed.
 */

import test from 'node:test'
import assert from 'node:assert/strict'
import { fmtDisasm, fmtPause, fmtSwitchThread } from '../src/format.js'

const SAMPLE = {
  pc_index: 2,
  instructions: [
    { address: '0x401000', instructionBytes: '48 89 e5', instruction: 'mov rbp, rsp' },
    { address: '0x401003', instructionBytes: '48 83 ec 20', instruction: 'sub rsp, 0x20' },
    { address: '0x401007', instructionBytes: 'e8 00 00 00 00', instruction: 'call 0x40100c' },
  ],
}

test('fmtDisasm renders an aligned asm block', () => {
  const out = fmtDisasm(SAMPLE)
  assert.ok(out.startsWith('```asm\n'))
  assert.ok(out.endsWith('\n```'))
  const lines = out.split('\n')
  // mark ' ' + literal space = 2 leading spaces; address padded to 8;
  // bytes padded to 14 (longest byte string)
  assert.equal(lines[1], '  0x401000  48 89 e5        mov rbp, rsp')
  assert.equal(lines[2], '► 0x401003  48 83 ec 20     sub rsp, 0x20')
  assert.equal(lines[3], '  0x401007  e8 00 00 00 00  call 0x40100c')
})

test('fmtDisasm marks the PC line with ► using 1-based pc_index', () => {
  const out = fmtDisasm(SAMPLE)
  const lines = out.split('\n')
  assert.ok((lines[2] ?? '').startsWith('►')) // pc_index 2 → second instruction line
  assert.ok((lines[1] ?? '').startsWith(' '))
  assert.ok((lines[3] ?? '').startsWith(' '))
})

test('fmtDisasm has no ► marker when pc_index is absent', () => {
  const { pc_index: _pc, ...noIndex } = SAMPLE
  const out = fmtDisasm(noIndex)
  assert.ok(!out.includes('►'))
})

test('fmtDisasm returns "(no disassembly)" for empty instruction list', () => {
  assert.equal(fmtDisasm({ instructions: [] }), '(no disassembly)')
  assert.equal(fmtDisasm({}), '(no disassembly)')
})

test('fmtPause renders the stop state after a pause request', () => {
  const out = fmtPause({
    status: 'stopped',
    reason: 'pause',
    paused_thread_id: 1234,
    thread_id: 1234,
    totalFrames: 42,
    frames: [
      { name: 'foo', source: './a.cpp', line: 10 },
      { name: 'bar', line: 99 },
    ],
  })
  assert.equal(out.split('\n')[0], '## 调试器已暂停 (pause)')
  assert.ok(out.includes('**pause 线程:** 1234'))
  assert.ok(out.includes('**原因:** pause'))
  assert.ok(out.includes('Thread: 1234 (top 2 of 42 frames)'))
  assert.ok(out.includes('\n- 0. foo'))
})

test('fmtPause reports an already-paused session without a pause request', () => {
  const out = fmtPause({ status: 'stopped', already: true, reason: 'already stopped', thread_id: 9, frames: [] })
  assert.equal(out.split('\n')[0], '## 会话已处于暂停状态（未发送 pause）')
  assert.ok(!out.includes('pause 线程'))
  assert.ok(out.includes('Thread: 9'))
  assert.ok(out.includes('(no frames)'))
})

test('fmtPause explains a timeout instead of claiming the target is paused', () => {
  const out = fmtPause({
    status: 'timeout',
    paused_thread_id: 5,
    error: 'pause timeout: no stop event within 30000ms',
  })
  assert.equal(out.split('\n')[0], '## 等待暂停事件超时')
  assert.ok(out.includes('**pause 线程:** 5'))
  assert.ok(out.includes('nvim_dap_wait_stop'))
})

test('fmtPause reports a terminated target and tolerates a null result', () => {
  assert.equal(fmtPause({ status: 'terminated' }), '调试目标已退出')
  assert.equal(fmtPause(null), '(no response)')
})

test('fmtSwitchThread lists the new thread and its frames', () => {
  const out = fmtSwitchThread({
    thread_id: 1234,
    thread_name: 'worker-3',
    totalFrames: 42,
    frames: [
      { name: 'foo', source: './a.cpp', line: 10, column: 3, sourceLine: 'int foo()' },
      { name: 'bar', line: 99 },
    ],
  })
  assert.equal(out.split('\n')[0], '已切换到线程 1234 (worker-3) — 共 42 帧，显示前 2 帧')
  assert.ok(out.includes('\n0 | foo | ./a.cpp:10:3'))
  assert.ok(out.includes('\n1 | bar | line 99'))
})

test('fmtSwitchThread tolerates a legacy single-frame payload', () => {
  const out = fmtSwitchThread({ thread_id: 7, frame: { name: 'main', source: './m.cpp', line: 1 } })
  assert.ok(out.startsWith('已切换到线程 7\n'))
  assert.ok(out.includes('0 | main | ./m.cpp:1'))
})

test('fmtSwitchThread returns "(no frames)" when there are none', () => {
  assert.equal(fmtSwitchThread({ thread_id: 1, frames: [] }), '已切换到线程 1\n(no frames)')
})
