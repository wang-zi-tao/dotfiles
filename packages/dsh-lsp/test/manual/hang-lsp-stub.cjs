#!/usr/bin/env node
/**
 * A deliberately unhelpful LSP server: it proves the plugin never waits forever.
 *
 *   node hang-lsp-stub.cjs diag-hang   answer initialize (advertising pull
 *                                      diagnostics), then ignore
 *                                      textDocument/diagnostic forever
 *   node hang-lsp-stub.cjs init-hang   never answer initialize at all
 */
const mode = process.argv[2] ?? 'diag-hang'

let buffer = Buffer.alloc(0)

function send(message) {
  const body = Buffer.from(JSON.stringify(message), 'utf8')
  process.stdout.write('Content-Length: ' + body.length + '\r\n\r\n')
  process.stdout.write(body)
}

process.stdin.on('data', (chunk) => {
  buffer = Buffer.concat([buffer, chunk])
  for (;;) {
    const headerEnd = buffer.indexOf('\r\n\r\n')
    if (headerEnd < 0) return
    const header = buffer.subarray(0, headerEnd).toString('ascii')
    const match = /Content-Length: (\d+)/i.exec(header)
    if (!match) {
      buffer = buffer.subarray(headerEnd + 4)
      continue
    }
    const length = Number(match[1])
    const start = headerEnd + 4
    if (buffer.length < start + length) return
    const body = buffer.subarray(start, start + length).toString('utf8')
    buffer = buffer.subarray(start + length)
    handle(JSON.parse(body))
  }
})

function handle(message) {
  if (message.method === 'initialize') {
    if (mode === 'init-hang') return
    send({
      jsonrpc: '2.0',
      id: message.id,
      result: {
        serverInfo: { name: 'hang-stub', version: '1.0.0' },
        capabilities: {
          diagnosticProvider: { interFileDependencies: false, workspaceDiagnostics: false },
        },
      },
    })
    return
  }
  if (message.method === 'shutdown') {
    send({ jsonrpc: '2.0', id: message.id, result: null })
    return
  }
  if (message.method === 'exit') {
    process.exit(0)
  }
  // Every other request (textDocument/diagnostic included) is ignored on purpose.
}
