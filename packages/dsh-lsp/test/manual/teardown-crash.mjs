#!/usr/bin/env node
/**
 * Teardown / cancellation crash check (real stdio connection, no fakes).
 *
 *  1. spawn the stub LSP server (answers initialize, advertises pull
 *     diagnostics, never answers textDocument/diagnostic)
 *  2. open a pull diagnostics request (in flight, unanswered)
 *  3. tear the client down - what plugin unload / hot reload does
 *  4. let the request's deadline fire afterwards
 *
 * Without the guard in LspClient#token, cancelling the request writes a
 * '$/cancelRequest' notification to the disposed connection; vscode-jsonrpc
 * throws 'ConnectionError: Connection is closed' from the abort listener, Node
 * reports that as an uncaughtException and the host process dies. With the
 * guard, the cancellation is dropped and the process stays up.
 *
 * DSH_LSP_LIB selects the lib directory under test (default: this package).
 */
import { spawn } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { dirname, join } from 'node:path'
import { tmpdir } from 'node:os'

const HERE = dirname(fileURLToPath(import.meta.url))
const LIB = (process.env.DSH_LSP_LIB ?? new URL('../../lib/', import.meta.url).href).replace(/\/?$/, '/')
const { LspClient } = await import(LIB + 'client.js')
const { isLspDeadlineError } = await import(LIB + 'deadline.js')

const SAMPLE = join(HERE, 'sample.py')
const STUB = join(HERE, 'hang-lsp-stub.cjs')
const LOG_DIR = join(tmpdir(), 'dsh-lsp-crash-logs')

let crashed = null
process.on('uncaughtException', (error) => {
  crashed = error
  console.log('UNCAUGHT EXCEPTION (this is the dsh crash): ' + error.name + ': ' + error.message)
})
process.on('unhandledRejection', (reason) => {
  console.log('UNHANDLED REJECTION: ' + String(reason))
})

function createRuntime() {
  return {
    spawn(spec) {
      const child = spawn(spec.argv[0], spec.argv.slice(1), {
        cwd: spec.cwd,
        stdio: [spec.stdio.stdin === 'ignore' ? 'ignore' : 'pipe', 'pipe', 'pipe'],
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

const logger = { debug() {}, info() {}, warn() {}, error() {} }
const spec = {
  id: 'pyright',
  command: process.execPath,
  args: [STUB, 'diag-hang'],
  extensions: ['py'],
  languageId: 'python',
  rootMarkers: ['pyrightconfig.json'],
}

const client = new LspClient(spec, createRuntime(), logger, {}, LOG_DIR, 1500)
await client.start(HERE)
console.log('client started, state=' + client.state + ', pull=' + client.supportsPullDiagnostics())

const inFlight = client.diagnostics(SAMPLE)
inFlight.catch(() => {})
console.log('pull diagnostics in flight (server never answers)')

await client.stop()
console.log('client stopped: connection disposed, server terminated')

// The 1500ms deadline fires only after the connection is already gone.
await new Promise((resolve) => setTimeout(resolve, 2500))

let outcome = 'no rejection seen'
try {
  await inFlight
} catch (error) {
  outcome = (isLspDeadlineError(error) ? 'LspDeadlineError' : error.name) + ': ' + error.message
}
console.log('in-flight request settled as: ' + outcome)
await new Promise((resolve) => setTimeout(resolve, 200))

if (crashed) {
  console.log('\nRESULT: CRASHED (cancellation written to a closed connection)')
  process.exit(2)
}
console.log('\nRESULT: survived, no uncaught exception')
process.exit(0)
