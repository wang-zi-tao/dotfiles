import test from 'node:test'
import { equal, ok } from 'node:assert/strict'
import { mkdtemp, mkdir, readFile, rm, writeFile } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { join } from 'node:path'

import { apply, inject, name } from '../src/index.js'

type Listener = (...args: any[]) => unknown

interface FakeContext {
  readonly listeners: Map<string, Listener[]>
}

function makeContext(): FakeContext {
  const listeners = new Map<string, Listener[]>()
  const ctx: any = {
    logger: () => ({ debug: () => {}, info: () => {}, warn: () => {}, error: () => {} }),
    on: (event: string, listener: Listener) => {
      const bucket = listeners.get(event) ?? []
      bucket.push(listener)
      listeners.set(event, bucket)
    },
    listeners,
  }
  return ctx as FakeContext
}

function makeExec(toolName: string, filePath: string, cwd: string): any {
  return {
    name: toolName,
    arguments: { file_path: filePath },
    agent: { session: { header: { cwd } } },
    signal: new AbortController().signal,
  }
}

function makeResult(path: string, isError = false): any {
  return isError
    ? { isError: true, error: { message: 'nope' }, content: [] }
    : { isError: false, value: { path }, content: [] }
}

/** The pre-execute stage, exactly as the registry runs it. */
async function runPre(ctx: FakeContext, exec: any, decision: unknown = { kind: 'allow' }): Promise<unknown> {
  let out: unknown
  for (const listener of ctx.listeners.get('tools/pre-execute') ?? []) {
    out = await listener(exec, async () => decision)
  }
  return out
}

/** The post-execute stage, exactly as the registry runs it. */
async function runPost(ctx: FakeContext, exec: any, result: any): Promise<unknown> {
  let out: unknown
  for (const listener of ctx.listeners.get('tools/post-execute') ?? []) {
    out = await listener(exec, result, async () => ({ kind: 'accept' }))
  }
  return out
}

/**
 * One full tool call: snapshot in pre-execute, the tool's write lands, then the
 * repair runs in post-execute.
 */
async function simulateWrite(
  ctx: FakeContext,
  exec: any,
  file: string,
  bytes: string | Uint8Array,
): Promise<unknown> {
  await runPre(ctx, exec)
  await writeFile(file, bytes as any)
  return runPost(ctx, exec, makeResult(file))
}

const BOM = Buffer.from([0xef, 0xbb, 0xbf])
const CRLF_SOURCE = '// \u4e2d\u6587\u6ce8\u91ca\r\nint main() {\r\n    return 0;\r\n}\r\n'
const NON_ASCII = '// \u4e2d\u6587\nint x;\n'

async function withTempDir<T>(body: (dir: string) => Promise<T>): Promise<T> {
  const dir = await mkdtemp(join(tmpdir(), 'dsh-encoding-'))
  try {
    return await body(dir)
  } finally {
    await rm(dir, { recursive: true, force: true })
  }
}

test('the plugin declares its identity', () => {
  equal(name, 'dsh-encoding')
  ok(inject.includes('tools'))
})

test('a mangled CRLF+BOM source is restored byte-for-byte', async () => {
  await withTempDir(async (dir) => {
    const file = join(dir, 'sample.cpp')
    // A real WPS C++ source with non-ASCII carries the BOM already.
    await writeFile(file, Buffer.concat([BOM, Buffer.from(CRLF_SOURCE, 'utf8')]))
    const original = await readFile(file)

    const ctx = makeContext()
    apply(ctx as any, { log: 'off' })
    const exec = makeExec('edit', file, dir)

    // The harness strips the BOM and LF-normalizes everything the tool writes.
    const mangled = CRLF_SOURCE.replaceAll('\r\n', '\n')
    await simulateWrite(ctx, exec, file, mangled)

    equal(Buffer.compare(await readFile(file), original), 0)
  })
})

test('a mangled edit only rewrites the lines the edit touched', async () => {
  await withTempDir(async (dir) => {
    const file = join(dir, 'mixed.cpp')
    const body = 'int a;\r\nint b;\nint c;\r\n'
    await writeFile(file, body, 'utf8')

    const ctx = makeContext()
    apply(ctx as any, { log: 'off' })
    const exec = makeExec('edit', file, dir)
    await simulateWrite(ctx, exec, file, 'int a;\r\nlong b;\nint c;\r\n')

    equal(await readFile(file, 'utf8'), 'int a;\r\nlong b;\r\nint c;\r\n')
  })
})

test('a write that changes nothing produces no line-ending churn', async () => {
  await withTempDir(async (dir) => {
    const file = join(dir, 'stable.cpp')
    const body = 'int a;\r\nint b;\nint c;\r\n'
    await writeFile(file, body, 'utf8')
    const original = await readFile(file)

    const ctx = makeContext()
    apply(ctx as any, { log: 'off' })
    await simulateWrite(ctx, makeExec('write', file, dir), file, 'int a;\nint b;\nint c;\n')

    equal(Buffer.compare(await readFile(file), original), 0)
  })
})

test('a new non-ASCII source gains a BOM and CRLF', async () => {
  await withTempDir(async (dir) => {
    const file = join(dir, 'fresh.h')
    const ctx = makeContext()
    apply(ctx as any, { log: 'off' })
    const exec = makeExec('write', file, dir)
    await simulateWrite(ctx, exec, file, NON_ASCII)

    const bytes = await readFile(file)
    equal(bytes.subarray(0, 3).equals(BOM), true)
    equal(bytes.subarray(3).toString('utf8'), '// \u4e2d\u6587\r\nint x;\r\n')
  })
})

test('a pure-ASCII new source gets no BOM but does get CRLF', async () => {
  await withTempDir(async (dir) => {
    const file = join(dir, 'plain.cpp')
    const ctx = makeContext()
    apply(ctx as any, { log: 'off' })
    await simulateWrite(ctx, makeExec('write', file, dir), file, 'int x;\n')

    const bytes = await readFile(file)
    equal(bytes.subarray(0, 3).equals(BOM), false)
    equal(bytes.toString('utf8'), 'int x;\r\n')
  })
})

test('non-target file types are never touched', async () => {
  await withTempDir(async (dir) => {
    const file = join(dir, 'script.ts')
    const body = 'const a = 1;\n'
    const ctx = makeContext()
    apply(ctx as any, { log: 'off' })
    await simulateWrite(ctx, makeExec('write', file, dir), file, body)
    equal(await readFile(file, 'utf8'), body)
  })
})

test('excluded directories are never touched', async () => {
  await withTempDir(async (dir) => {
    const nested = join(dir, 'node_modules', 'pkg')
    await mkdir(nested, { recursive: true })
    const file = join(nested, 'a.cpp')
    const body = 'int x;\n'
    const ctx = makeContext()
    apply(ctx as any, { log: 'off' })
    await simulateWrite(ctx, makeExec('write', file, dir), file, body)
    equal(await readFile(file, 'utf8'), body)
  })
})

test('a failed write is left alone', async () => {
  await withTempDir(async (dir) => {
    const file = join(dir, 'x.cpp')
    const body = 'int x;\n'
    await writeFile(file, body, 'utf8')
    const ctx = makeContext()
    apply(ctx as any, { log: 'off' })
    const exec = makeExec('write', file, dir)
    await runPre(ctx, exec)
    await runPost(ctx, exec, makeResult(file, true))
    equal(await readFile(file, 'utf8'), body)
  })
})

test('unrelated tools are ignored', async () => {
  await withTempDir(async (dir) => {
    const file = join(dir, 'x.cpp')
    const body = 'int x;\n'
    const ctx = makeContext()
    apply(ctx as any, { log: 'off' })
    await simulateWrite(ctx, makeExec('read', file, dir), file, body)
    equal(await readFile(file, 'utf8'), body)
  })
})

test('the plugin can be switched off', async () => {
  await withTempDir(async (dir) => {
    const file = join(dir, 'x.cpp')
    const ctx = makeContext()
    apply(ctx as any, { log: 'off', enabled: false })
    await simulateWrite(ctx, makeExec('write', file, dir), file, NON_ASCII)
    equal((await readFile(file, 'utf8')).includes('\r\n'), false)
  })
})

test('dryRun reports without writing', async () => {
  await withTempDir(async (dir) => {
    const file = join(dir, 'x.cpp')
    const ctx = makeContext()
    apply(ctx as any, { log: 'off', dryRun: true })
    await simulateWrite(ctx, makeExec('write', file, dir), file, NON_ASCII)
    equal(await readFile(file, 'utf8'), NON_ASCII)
  })
})

test('a binary payload is refused even when the path matches', async () => {
  await withTempDir(async (dir) => {
    const file = join(dir, 'blob.cpp')
    const bytes = Buffer.from([0x00, 0x01, 0x02, 0x03])
    const ctx = makeContext()
    apply(ctx as any, { log: 'off' })
    await simulateWrite(ctx, makeExec('write', file, dir), file, bytes)
    equal(Buffer.compare(await readFile(file), bytes), 0)
  })
})

test('invalid UTF-8 is refused rather than rewritten', async () => {
  await withTempDir(async (dir) => {
    const file = join(dir, 'latin1.cpp')
    const bytes = Buffer.from([0x2f, 0x2f, 0xe9, 0x2d, 0x0a])
    const ctx = makeContext()
    apply(ctx as any, { log: 'off' })
    await simulateWrite(ctx, makeExec('write', file, dir), file, bytes)
    equal(Buffer.compare(await readFile(file), bytes), 0)
  })
})

test('the plugin never throws into the pipeline', async () => {
  await withTempDir(async (dir) => {
    const ctx = makeContext()
    apply(ctx as any, { log: 'off' })
    const missing = join(dir, 'missing', 'deep', 'x.cpp')
    const exec = makeExec('write', missing, dir)
    await runPre(ctx, exec)
    const post = await runPost(ctx, exec, makeResult(missing))
    ok(post !== undefined)
  })
})

test('a listener always returns the pipeline decision it was handed', async () => {
  await withTempDir(async (dir) => {
    const ctx = makeContext()
    apply(ctx as any, { log: 'off' })
    const exec = makeExec('write', join(dir, 'x.cpp'), dir)
    const pre = await runPre(ctx, exec, { kind: 'deny', reason: 'keep' })
    equal((pre as any).kind, 'deny')
    equal((pre as any).reason, 'keep')

    const post = await runPost(ctx, exec, makeResult(join(dir, 'x.cpp')))
    equal((post as any).kind, 'accept')
  })
})

test('a CMake file never gains a BOM, even with non-ASCII content', async () => {
  await withTempDir(async (dir) => {
    const file = join(dir, 'CMakeLists.txt')
    const ctx = makeContext()
    apply(ctx as any, { log: 'off' })
    await simulateWrite(ctx, makeExec('write', file, dir), file, '# \u4e2d\u6587\nadd_library(x)\n')

    const bytes = await readFile(file)
    equal(bytes.subarray(0, 3).equals(BOM), false)
    // the line-ending half of the repair still applies
    equal(bytes.toString('utf8'), '# \u4e2d\u6587\r\nadd_library(x)\r\n')
  })
})

test('a CMake file keeps the BOM it already had', async () => {
  await withTempDir(async (dir) => {
    const file = join(dir, 'x.cmake')
    await writeFile(file, Buffer.concat([BOM, Buffer.from('add_library(x)\r\n', 'utf8')]))
    const ctx = makeContext()
    apply(ctx as any, { log: 'off' })
    // the harness strips the BOM; 'preserve' puts it back rather than churning it away
    await simulateWrite(ctx, makeExec('edit', file, dir), file, 'add_library(y)\n')

    const bytes = await readFile(file)
    equal(bytes.subarray(0, 3).equals(BOM), true)
    equal(bytes.subarray(3).toString('utf8'), 'add_library(y)\r\n')
  })
})

test('CMake can be switched to BOM-free with a row override', async () => {
  await withTempDir(async (dir) => {
    const file = join(dir, 'CMakeLists.txt')
    await writeFile(file, Buffer.concat([BOM, Buffer.from('add_library(x)\r\n', 'utf8')]))
    const ctx = makeContext()
    apply(ctx as any, { log: 'off', overrides: [{ match: ['**/CMakeLists.txt'], bom: 'never' }] })
    await simulateWrite(ctx, makeExec('edit', file, dir), file, 'add_library(y)\n')

    const bytes = await readFile(file)
    equal(bytes.subarray(0, 3).equals(BOM), false)
    equal(bytes.toString('utf8'), 'add_library(y)\r\n')
  })
})

test('a C++ file with the same content still gains the BOM', async () => {
  await withTempDir(async (dir) => {
    const file = join(dir, 'same.h')
    const ctx = makeContext()
    apply(ctx as any, { log: 'off' })
    await simulateWrite(ctx, makeExec('write', file, dir), file, '# \u4e2d\u6587\nadd_library(x)\n')
    equal((await readFile(file)).subarray(0, 3).equals(BOM), true)
  })
})

test('a bare CR left behind by a Mac-style write is repaired', async () => {
  await withTempDir(async (dir) => {
    const file = join(dir, 'mac.cpp')
    const ctx = makeContext()
    apply(ctx as any, { log: 'off' })
    await simulateWrite(ctx, makeExec('write', file, dir), file, 'int a;\rint b;\n')
    equal(await readFile(file, 'utf8'), 'int a;\r\nint b;\r\n')
  })
})
