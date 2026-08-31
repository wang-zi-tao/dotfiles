/**
 * Markdown formatting helpers for nvim-dap results.
 *
 * Pure functions over the JSON values returned by the Lua bridge, kept in one
 * module so the tool bodies stay declarative and the renderings stay testable.
 */
export function fmtFrame(f, idx) {
    const prefix = idx !== undefined ? `${idx}` : '-';
    const loc = f.source ? `${f.source}:${f.line}` : `line ${f.line}`;
    return `${prefix} | ${f.name || '?'} | ${loc}${f.column ? `:${f.column}` : ''}`;
}
export function fmtStack(data) {
    const frames = data.frames || [];
    const tid = data.thread_id ?? '?';
    const total = data.totalFrames ?? frames.length;
    let out = `Thread: ${tid}`;
    if (total > frames.length)
        out += ` (top ${frames.length} of ${total} frames)`;
    out += `\n`;
    if (frames.length === 0)
        return out + '(no frames)';
    for (let i = 0; i < frames.length; i++) {
        const f = frames[i];
        const loc = f.source ? `${f.source}:${f.line}` : `line ${f.line}`;
        out += `\n- ${i}. ${f.name || '?'}`;
        out += `\nlocation: ${loc}`;
        if (f.sourceLine)
            out += `\ncode: ${f.sourceLine}`;
    }
    return out;
}
export function fmtBreakpoints(data) {
    const bps = data.breakpoints || [];
    if (bps.length === 0)
        return '(no breakpoints)';
    let out = '| File | Line | Condition | Hit | Log | Verified |\n|------|------|-----------|-----|-----|----------|\n';
    for (const bp of bps) {
        const file = bp.file ? bp.file.replace(/^.*[\\/]/, '') : '?';
        out += `| ${file} | ${bp.line} | ${bp.condition || ''} | ${bp.hitCondition || ''} | ${bp.logMessage || ''} | ${bp.verified != null ? bp.verified : ''} |\n`;
    }
    return out;
}
export function fmtConfigs(data) {
    const cfgs = data.configurations || [];
    if (cfgs.length === 0)
        return '(no configurations)';
    let out = '| Language | Name | Type | Request |\n|----------|------|------|--------|\n';
    for (const c of cfgs) {
        out += `| ${c.lang || ''} | ${c.name || ''} | ${c.type || ''} | ${c.request || ''} |\n`;
    }
    return out;
}
export function fmtSessions(data) {
    const sessions = data.sessions || [];
    if (sessions.length === 0)
        return '(no active sessions)';
    let out = '| Name | ID | Type | CWD |\n|------|----|------|-----|\n';
    for (const s of sessions) {
        out += `| ${s.name || ''} | ${s.id ?? ''} | ${s.type || ''} | ${s.root || ''} |\n`;
    }
    return out;
}
export function fmtThreads(data) {
    const threads = data.threads || [];
    if (threads.length === 0)
        return '(no threads)';
    let out = '| ID | Name | Stopped |\n|----|------|--------|\n';
    for (const t of threads) {
        out += `| ${t.id} | ${t.name || ''} | ${t.stopped ? '◼' : ''} |\n`;
    }
    return out;
}
