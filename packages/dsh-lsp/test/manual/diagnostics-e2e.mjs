#!/usr/bin/env node
/**
 * Manual end-to-end check for the diagnostics tool's bounded-wait guarantee.
 *
 *   node test/manual/diagnostics-e2e.mjs
 *
 * Scenario A  real pyright on a real .py file  -> findings come back, bounded
 * Scenario B  a server that answers initialize but never answers
 *             textDocument/diagnostic          -> pull deadline + note
 * Scenario C  a server that never answers initialize at all
 *                                             -> LspDeadlineError, bounded
 *
 * Requires a built package (npm run build) and pyright-langserver. Override the
 * binary with DSH_LSP_PYRIGHT=/path/to/pyright-langserver if it is not on PATH.
 * Each scenario carries its own wall-clock watchdog, so a regression fails the
 * run instead of hanging it.
 */
import { spawn } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { dirname, join } from 'node:path'
import { tmpdir } from 'node:os'

const HERE = dirname(fileURLToPath(import.meta.url))
const LIB = new URL('../../lib/', import.meta.url).href
const { resolveConfig } = await import(LIB + 'config.js')
const { ServerRegistry } = await import(LIB + 'registry.js')
const { isLspDeadlineError } = await import(LIB + 'deadline.js')

const SAMPLE = join(HERE, 'sample.py')
const STUB = join(HERE, 'hang-lsp-stub.cjs')
const LOG_DIR = join(tmpdir(), 'dsh-lsp-e2e-logs')
const PYRIGHT = process.env.DSH_LSP_PYRIGHT ?? 'pyright-langserver'

const log = {
  debug: (m) => console.log('    [debug] ' + m),
  info: (m) => console.log('    [info ] ' + m),
  warn: (m) => console.log('    [warn ] ' + m),
  error: (m) => console.log('    [error] ' + m),
}

/** Minimal implementation of the plugin's SubprocessRuntime seam. */
function createRuntime() {
  return {
    spawn(spec) {
      const child = spawn(spec.argv[0], spec.argv.slice(1), {
        cwd: spec.cwd,
        stdio: [spec.stdio.stdin === 'ignore' ? 'ignore' : 'pipe', 'pipe', 'pipe'],
        env: { ...process.env, ...(spec.env ?? {}) },
        windowsHide: true,
      })
      let stderrText = ''
      child.stderr?.on('data', (chunk) => { stderrText += chunk.toString('utf8') })
      const done = new Promise((resolveDone) => {
        child.on('exit', (exitCode, signal) => resolveDone({ exitCode, signal }))
        child.on('error', () => resolveDone({ exitCode: -1, signal: null }))
      })
      return {
        pid: child.pid ?? -1,
        stdin: spec.stdio.stdin === 'ignore' ? undefined : child.stdin,
        stdout: child.stdout,
        stderr: child.stderr,
        done,
        collected: {
          stdout: { finalize: () => ({ text: '', truncated: false }) },
          stderr: { finalize: () => ({ text: stderrText, truncated: false }) },
        },
        terminate() { try { child.kill() } catch {} },
        async waitForExit() { await done; return true },
      }
    },
  }
}

const runtime = createRuntime()
const results = []

function guard(ms, label) {
  let timer
  const timeout = new Promise((_, reject) => {
    timer = setTimeout(() => reject(new Error('WATCHDOG: ' + label + ' exceeded ' + ms + 'ms')), ms)
  })
  return { timeout, clear: () => clearTimeout(timer) }
}

function stubConfig(mode, timeoutMs) {
  return resolveConfig({
    logDir: LOG_DIR,
    timeoutMs,
    diagnosticsTimeoutMs: timeoutMs,
    servers: [{
      id: 'pyright',
      command: process.execPath,
      args: [STUB, mode],
      extensions: ['py', 'pyi'],
      languageId: 'python',
      rootMarkers: ['pyrightconfig.json'],
    }],
  })
}

function pyrightConfig(timeoutMs) {
  return resolveConfig({
    logDir: LOG_DIR,
    timeoutMs,
    diagnosticsTimeoutMs: timeoutMs,
    servers: [{
      id: 'pyright',
      command: PYRIGHT,
      args: ['--stdio'],
      extensions: ['py', 'pyi'],
      languageId: 'python',
      rootMarkers: ['pyrightconfig.json', 'pyproject.toml', 'setup.py'],
    }],
  })
}

const SEVERITY = { 1: 'error', 2: 'warning', 3: 'info', 4: 'hint' }

function describe(entry) {
  const severity = SEVERITY[entry.severity] ?? String(entry.severity)
  const line = entry.range?.startLine ?? 0
  return '  ' + severity.padEnd(7) + ' line ' + String(line).padStart(2) + '  ' + entry.message.replace(/\n/g, ' ')
}

async function run(registry, label, watchdogMs) {
  const started = Date.now()
  const g = guard(watchdogMs, label)
  try {
    const report = await Promise.race([registry.diagnosticsFor(SAMPLE, HERE), g.timeout])
    return { report, elapsed: Date.now() - started }
  } finally {
    g.clear()
  }
}

async function scenarioA() {
  console.log('\n=== A: real pyright on a real .py file ===')
  const registry = new ServerRegistry(pyrightConfig(20000), runtime, log)
  try {
    const cold = await run(registry, 'scenario A', 40000)
    console.log('  cold: ' + cold.elapsed + 'ms, transport=' + registry.diagnosticsMode(SAMPLE) + ', entries=' + cold.report.entries.length)
    if (cold.report.note) console.log('  note: ' + cold.report.note)
    for (const entry of cold.report.entries.slice(0, 8)) console.log(describe(entry))
    const warm = await run(registry, 'scenario A warm', 40000)
    console.log('  warm: ' + warm.elapsed + 'ms, entries=' + warm.report.entries.length + (warm.report.note ? ', note=' + warm.report.note : ''))
    const ok = cold.report.entries.length >= 4 && warm.report.entries.length >= 4 && !cold.report.note && !warm.report.note
    results.push({ name: 'A pyright real file', ok, detail: cold.report.entries.length + ' entries cold / ' + warm.report.entries.length + ' warm, ' + cold.elapsed + 'ms' })
  } finally {
    await registry.stopAll()
  }
}

async function scenarioB() {
  console.log('\n=== B: initialize answered, textDocument/diagnostic never answered (timeoutMs=1500) ===')
  const registry = new ServerRegistry(stubConfig('diag-hang', 1500), runtime, log)
  try {
    const { report, elapsed } = await run(registry, 'scenario B', 20000)
    console.log('  returned after ' + elapsed + 'ms, entries=' + report.entries.length)
    if (report.note) console.log('  note: ' + report.note)
    const ok = elapsed < 6000 && typeof report.note === 'string' && /timed out/.test(report.note)
    results.push({ name: 'B pull timeout', ok, detail: elapsed + 'ms, note=' + (report.note ?? '(none)') })
  } catch (error) {
    console.log('  FAILED with: ' + (error?.message ?? error))
    results.push({ name: 'B pull timeout', ok: false, detail: String(error?.message ?? error) })
  } finally {
    await registry.stopAll()
  }
}

async function scenarioC() {
  console.log('\n=== C: initialize never answered (timeoutMs=1200) ===')
  const registry = new ServerRegistry(stubConfig('init-hang', 1200), runtime, log)
  let error
  let elapsed = 0
  const started = Date.now()
  try {
    await run(registry, 'scenario C', 20000)
  } catch (err) {
    error = err
  } finally {
    elapsed = Date.now() - started
    await registry.stopAll()
  }
  console.log('  rejected after ' + elapsed + 'ms with ' + (error ? error.name + ': ' + error.message : 'SUCCESS (unexpected)'))
  const ok = Boolean(error) && isLspDeadlineError(error) && elapsed < 6000
  results.push({ name: 'C initialize timeout', ok, detail: elapsed + 'ms, ' + (error ? error.name : 'no error') })
}

for (const [label, scenario] of [['scenario A', scenarioA], ['scenario B', scenarioB], ['scenario C', scenarioC]]) {
  try {
    await scenario()
  } catch (error) {
    console.log('  SCENARIO ERROR: ' + (error?.stack ?? error))
    results.push({ name: label, ok: false, detail: String(error?.message ?? error) })
  }
}

console.log('\n=== summary ===')
for (const r of results) console.log((r.ok ? 'PASS  ' : 'FAIL  ') + r.name.padEnd(26) + r.detail)
const failed = results.filter(r => !r.ok).length
console.log(failed === 0 ? '\nALL PASS' : '\n' + failed + ' FAILED')
process.exitCode = failed === 0 ? 0 : 1
