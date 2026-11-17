/**
 * Markdown formatting helpers for nvim-dap results.
 *
 * Pure functions over the JSON values returned by the Lua bridge, kept in one
 * module so the tool bodies stay declarative and the renderings stay testable.
 */

export function fmtFrame(f: any, idx?: number): string {
  const prefix = idx !== undefined ? `${idx}` : '-';
  const loc = f.source ? `${f.source}:${f.line}` : `line ${f.line}`;
  return `${prefix} | ${f.name || '?'} | ${loc}${f.column ? `:${f.column}` : ''}`;
}

export function fmtStack(data: any): string {
  const frames: any[] = data.frames || [];
  const tid = data.thread_id ?? '?';
  const total = data.totalFrames ?? frames.length;
  let out = `Thread: ${tid}`;
  if (total > frames.length) out += ` (top ${frames.length} of ${total} frames)`;
  out += `\n`;
  if (frames.length === 0) return out + '(no frames)';
  for (let i = 0; i < frames.length; i++) {
    const f = frames[i];
    const loc = f.source ? `${f.source}:${f.line}` : `line ${f.line}`;
    out += `\n- ${i}. ${f.name || '?'}`;
    out += `\nlocation: ${loc}`;
    if (f.sourceLine) out += `\ncode: ${f.sourceLine}`;
  }
  return out;
}

/**
 * Render a pause result. A pause that found the session already stopped, one that
 * timed out waiting for the adapter's stopped event, and a terminated target each
 * mean something different to the reader, so they get their own headings.
 */
export function fmtPause(data: any): string {
  if (!data) return '(no response)'
  if (data.status === 'terminated') return '调试目标已退出'
  if (data.status === 'timeout') {
    let out = '## 等待暂停事件超时\n'
    out += `**pause 线程:** ${data.paused_thread_id ?? '?'}\n`
    out += `**原因:** ${data.error || 'no stop event'}\n`
    return out + '目标可能已暂停但适配器未发 event_stopped；可用 nvim_dap_get_stack / nvim_dap_wait_stop 复核'
  }
  const tid = data.thread_id ?? data.paused_thread_id
  let out = data.already ? '## 会话已处于暂停状态（未发送 pause）\n' : '## 调试器已暂停 (pause)\n'
  if (!data.already && data.paused_thread_id != null) out += `**pause 线程:** ${data.paused_thread_id}\n`
  if (data.reason) out += `**原因:** ${data.reason}\n`
  return out + fmtStack({ frames: data.frames || [], thread_id: tid, totalFrames: data.totalFrames })
}

/** Render a thread switch: the newly current thread plus its top frames. */
export function fmtSwitchThread(data: any): string {
  const frames: any[] = data.frames || (data.frame ? [data.frame] : [])
  const total = data.totalFrames ?? frames.length
  const name = data.thread_name ? ` (${data.thread_name})` : ''
  let out = `已切换到线程 ${data.thread_id}${name}`
  if (total > frames.length) out += ` — 共 ${total} 帧，显示前 ${frames.length} 帧`
  out += '\n'
  if (frames.length === 0) return out + '(no frames)'
  for (let i = 0; i < frames.length; i++) {
    const f = frames[i]
    const loc = f.source ? `${f.source}:${f.line}` : `line ${f.line}`
    out += `\n${i} | ${f.name || '?'} | ${loc}${f.column ? `:${f.column}` : ''}`
    if (f.sourceLine) out += `\n    ${f.sourceLine}`
  }
  return out
}

export function fmtBreakpoints(data: any): string {
  const bps: any[] = data.breakpoints || [];
  if (bps.length === 0) return '(no breakpoints)';
  let out =
    '| File | Line | Condition | Hit | Log | Verified |\n|------|------|-----------|-----|-----|----------|\n';
  for (const bp of bps) {
    const file = bp.file ? bp.file.replace(/^.*[\\/]/, '') : '?';
    out += `| ${file} | ${bp.line} | ${bp.condition || ''} | ${bp.hitCondition || ''} | ${bp.logMessage || ''} | ${bp.verified != null ? bp.verified : ''} |\n`;
  }
  return out;
}

export function fmtConfigs(data: any): string {
  const cfgs: any[] = data.configurations || [];
  if (cfgs.length === 0) return '(no configurations)';
  let out =
    '| Language | Name | Type | Request |\n|----------|------|------|--------|\n';
  for (const c of cfgs) {
    out += `| ${c.lang || ''} | ${c.name || ''} | ${c.type || ''} | ${c.request || ''} |\n`;
  }
  return out;
}

export function fmtSessions(data: any): string {
  const sessions: any[] = data.sessions || [];
  if (sessions.length === 0) return '(no active sessions)';
  let out = '| Name | ID | Type | CWD |\n|------|----|------|-----|\n';
  for (const s of sessions) {
    out += `| ${s.name || ''} | ${s.id ?? ''} | ${s.type || ''} | ${s.root || ''} |\n`;
  }
  return out;
}

export function fmtThreads(data: any): string {
  const threads: any[] = data.threads || [];
  if (threads.length === 0) return '(no threads)';
  let out = '| ID | Name | Stopped |\n|----|------|--------|\n';
  for (const t of threads) {
    out += `| ${t.id} | ${t.name || ''} | ${t.stopped ? '◼' : ''} |\n`;
  }
  return out;
}

/** Render a disassembly result as an aligned asm block, PC line marked with ►. */
export function fmtDisasm(data: any): string {
  const ins: any[] = data.instructions || [];
  if (ins.length === 0) return '(no disassembly)';
  const addrW = Math.max(0, ...ins.map((it) => (it.address || '').length));
  const bytesW = Math.max(0, ...ins.map((it) => (it.instructionBytes || '').length));
  const pcIndex = data.pc_index;
  let out = '```asm\n';
  for (let i = 0; i < ins.length; i++) {
    const it = ins[i];
    const mark = i + 1 === pcIndex ? '►' : ' ';
    const addr = (it.address || '').padEnd(addrW);
    const bytes = (it.instructionBytes || '').padEnd(bytesW);
    out += `${mark} ${addr}  ${bytes}  ${it.instruction || ''}\n`;
  }
  out += '```';
  return out;
}
