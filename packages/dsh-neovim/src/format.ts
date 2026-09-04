/**
 * Markdown formatting helpers for nvim-dap results.
 *
 * Pure functions over the JSON values returned by the Lua bridge, kept in one
 * module so the tool bodies stay declarative and the renderings stay testable.
 */

export function fmtFrame(f: any, idx?: number): string {
  const prefix = idx !== undefined ? `${idx}` : '-'
  const loc = f.source ? `${f.source}:${f.line}` : `line ${f.line}`
  return `${prefix} | ${f.name || '?'} | ${loc}${f.column ? `:${f.column}` : ''}`
}

export function fmtStack(data: any): string {
  const frames: any[] = data.frames || []
  const tid = data.thread_id ?? '?'
  const total = data.totalFrames ?? frames.length
  let out = `Thread: ${tid}`
  if (total > frames.length) out += ` (top ${frames.length} of ${total} frames)`
  out += `\n`
  if (frames.length === 0) return out + '(no frames)'
  for (let i = 0; i < frames.length; i++) {
    const f = frames[i]
    const loc = f.source ? `${f.source}:${f.line}` : `line ${f.line}`
    out += `\n- ${i}. ${f.name || '?'}`
    out += `\nlocation: ${loc}`
    if (f.sourceLine) out += `\ncode: ${f.sourceLine}`
  }
  return out
}

export function fmtBreakpoints(data: any): string {
  const bps: any[] = data.breakpoints || []
  if (bps.length === 0) return '(no breakpoints)'
  let out =
    '| File | Line | Condition | Hit | Log | Verified |\n|------|------|-----------|-----|-----|----------|\n'
  for (const bp of bps) {
    const file = bp.file ? bp.file.replace(/^.*[\\/]/, '') : '?'
    out += `| ${file} | ${bp.line} | ${bp.condition || ''} | ${bp.hitCondition || ''} | ${bp.logMessage || ''} | ${bp.verified != null ? bp.verified : ''} |\n`
  }
  return out
}

export function fmtConfigs(data: any): string {
  const cfgs: any[] = data.configurations || []
  if (cfgs.length === 0) return '(no configurations)'
  let out =
    '| Language | Name | Type | Request |\n|----------|------|------|--------|\n'
  for (const c of cfgs) {
    out += `| ${c.lang || ''} | ${c.name || ''} | ${c.type || ''} | ${c.request || ''} |\n`
  }
  return out
}

export function fmtSessions(data: any): string {
  const sessions: any[] = data.sessions || []
  if (sessions.length === 0) return '(no active sessions)'
  let out = '| Name | ID | Type | CWD |\n|------|----|------|-----|\n'
  for (const s of sessions) {
    out += `| ${s.name || ''} | ${s.id ?? ''} | ${s.type || ''} | ${s.root || ''} |\n`
  }
  return out
}

export function fmtThreads(data: any): string {
  const threads: any[] = data.threads || []
  if (threads.length === 0) return '(no threads)'
  let out = '| ID | Name | Stopped |\n|----|------|--------|\n'
  for (const t of threads) {
    out += `| ${t.id} | ${t.name || ''} | ${t.stopped ? '◼' : ''} |\n`
  }
  return out
}

/** Render a disassembly result as an aligned asm block, PC line marked with ►. */
export function fmtDisasm(data: any): string {
  const ins: any[] = data.instructions || []
  if (ins.length === 0) return '(no disassembly)'
  const addrW = Math.max(0, ...ins.map((it) => (it.address || '').length))
  const bytesW = Math.max(0, ...ins.map((it) => (it.instructionBytes || '').length))
  const pcIndex = data.pc_index
  let out = '```asm\n'
  for (let i = 0; i < ins.length; i++) {
    const it = ins[i]
    const mark = i + 1 === pcIndex ? '►' : ' '
    const addr = (it.address || '').padEnd(addrW)
    const bytes = (it.instructionBytes || '').padEnd(bytesW)
    out += `${mark} ${addr}  ${bytes}  ${it.instruction || ''}\n`
  }
  out += '```'
  return out
}
