import test from 'node:test'
import { equal } from 'node:assert/strict'
import { mkdtempSync, mkdirSync, writeFileSync, rmSync } from 'node:fs'
import { join } from 'node:path'
import { tmpdir } from 'node:os'

import { findRoot } from '../src/root.js'
import type { ServerSpec } from '../src/types.js'

const spec: ServerSpec = {
  id: 'clangd',
  command: 'clangd',
  args: [],
  extensions: ['cpp'],
  languageId: 'cpp',
  rootMarkers: ['compile_commands.json', 'Cargo.toml'],
}

test('findRoot walks up to the compile_commands.json ancestor', () => {
  const root = mkdtempSync(join(tmpdir(), 'dsh-lsp-root-'))
  try {
    mkdirSync(join(root, 'src', 'deep', 'nested'), { recursive: true })
    writeFileSync(join(root, 'compile_commands.json'), '[]')
    const start = join(root, 'src', 'deep', 'nested')
    equal(findRoot(start, spec), root)
  } finally {
    rmSync(root, { recursive: true, force: true })
  }
})

test('findRoot prefers the nearest marker over a farther git ancestor', () => {
  const root = mkdtempSync(join(tmpdir(), 'dsh-lsp-root-'))
  try {
    const sub = join(root, 'sub', 'proj')
    mkdirSync(sub, { recursive: true })
    mkdirSync(join(root, '.git'))
    writeFileSync(join(sub, 'Cargo.toml'), '')
    equal(findRoot(sub, spec), sub)
  } finally {
    rmSync(root, { recursive: true, force: true })
  }
})

test('findRoot falls back to the .git ancestor when no marker exists', () => {
  const root = mkdtempSync(join(tmpdir(), 'dsh-lsp-root-'))
  try {
    const deep = join(root, 'a', 'b')
    mkdirSync(deep, { recursive: true })
    mkdirSync(join(root, '.git'))
    equal(findRoot(deep, spec), root)
  } finally {
    rmSync(root, { recursive: true, force: true })
  }
})

test('findRoot falls back to the start directory when nothing matches', () => {
  const root = mkdtempSync(join(tmpdir(), 'dsh-lsp-root-'))
  try {
    const start = join(root, 'src')
    mkdirSync(start, { recursive: true })
    equal(findRoot(start, spec), start)
  } finally {
    rmSync(root, { recursive: true, force: true })
  }
})
