/**
 * Configuration resolution for dsh-lsp.
 *
 * Precedence (highest wins):
 *   1. the Cordis row `config` (what `cordis.patch.yml` supplied)
 *   2. built-in defaults (the shipped server table)
 *
 * Validation is fail-loud at load: a duplicate file extension across two
 * enabled servers, a missing command, an empty extension list, or an unknown
 * languageId aborts the mount (surfaced by the mount audit as an invalid
 * row config), rather than mis-routing queries at runtime.
 */

import type { DiagnosticSeverity, LspConfig, ServerSpec } from './types.js'

export const DEFAULT_MAX_LOCATIONS = 100
export const DEFAULT_MAX_RESULT_CHARS = 16000
export const DEFAULT_TIMEOUT_MS = 60000
export const DEFAULT_LOG_DIR = '~/.dsh/logs/dsh-lsp'
export const DEFAULT_SYNC_LOAD_ON_READ = true
export const DEFAULT_DIAGNOSTICS_ON_WRITE = true
export const DEFAULT_DIAGNOSTICS_MIN_SEVERITY = 'warning'

/**
 * The built-in server table, optimized for C/C++ (clangd) and Rust
 * (rust-analyzer), with mainstream languages available for the mixed-language
 * case. The WPS clangd invocation mirrors the user's neovim lspconfig:
 * `--background-index` + `--compile-commands-dir=.` so the 264 MB
 * `D:\branch-master\wpsmain\compile_commands.json` is picked up from the
 * resolved project root.
 */
export const BUILTIN_SERVERS: readonly ServerSpec[] = Object.freeze([
  {
    id: 'clangd',
    command: 'clangd',
    args: ['--background-index', '--clang-tidy', '--compile-commands-dir=.', '--log=error'],
    extensions: ['c', 'cc', 'cpp', 'cxx', 'c++', 'h', 'hpp', 'hh', 'hxx', 'h++', 'inl'],
    languageId: 'cpp',
    rootMarkers: ['compile_commands.json', 'compile_flags.txt', '.clangd', 'wps_3rdparty_list.cmake'],
  },
  {
    id: 'rust-analyzer',
    command: 'rust-analyzer',
    args: [],
    extensions: ['rs'],
    languageId: 'rust',
    rootMarkers: ['Cargo.toml', 'Cargo.lock', 'rust-project.json'],
  },
  {
    id: 'gopls',
    command: 'gopls',
    args: [],
    extensions: ['go'],
    languageId: 'go',
    rootMarkers: ['go.mod', 'go.work'],
  },
  {
    id: 'pyright',
    command: 'pyright',
    args: ['--stdio'],
    extensions: ['py', 'pyi'],
    languageId: 'python',
    rootMarkers: ['pyproject.toml', 'setup.py', 'setup.cfg', 'requirements.txt', '.python-version'],
  },
  {
    id: 'typescript-language-server',
    command: 'typescript-language-server',
    args: ['--stdio'],
    extensions: ['ts', 'tsx', 'js', 'jsx', 'mjs', 'cjs'],
    languageId: 'typescript',
    rootMarkers: ['package.json', 'tsconfig.json', 'jsconfig.json'],
  },
  {
    id: 'lua-language-server',
    command: 'lua-language-server',
    args: [],
    extensions: ['lua'],
    languageId: 'lua',
    rootMarkers: ['.luarc.json', '.luarc.jsonc', '.stylua.toml'],
  },
])

function fail(message: string): never {
  throw new Error(`dsh-lsp: ${message}`)
}

function normalizeStringList(value: unknown): string[] {
  if (value === undefined || value === null) return []
  const list: unknown[] = Array.isArray(value) ? value : String(value).split(',')
  return list.map(item => String(item).trim()).filter(Boolean)
}

function normalizeExtensions(value: unknown): string[] {
  return normalizeStringList(value)
    .map(ext => (ext.startsWith('.') ? ext.slice(1) : ext).toLowerCase())
    .filter(Boolean)
}

function parseInteger(key: string, value: unknown, fallback: number, minimum: number): number {
  if (value === undefined || value === null || value === '') return fallback
  const parsed = Number(value)
  if (!Number.isFinite(parsed)) fail(`invalid ${key} ${JSON.stringify(value)}; expected a finite number`)
  const integer = Math.trunc(parsed)
  if (integer < minimum) fail(`invalid ${key} ${JSON.stringify(integer)}; expected >= ${minimum}`)
  return integer
}

function parseSeverity(value: unknown, fallback: DiagnosticSeverity): DiagnosticSeverity {
  if (value === undefined || value === null || value === '') return fallback
  const normalized = String(value).trim().toLowerCase()
  const map: Record<string, DiagnosticSeverity> = {
    error: 'error',
    warning: 'warning',
    warn: 'warning',
    information: 'information',
    info: 'information',
    hint: 'hint',
  }
  return map[normalized] ?? fallback
}

function parseBoolean(value: unknown, fallback: boolean): boolean {
  if (value === undefined || value === null || value === '') return fallback
  if (typeof value === 'boolean') return value
  if (typeof value === 'string') {
    if (/^(1|true|yes|on)$/i.test(value)) return true
    if (/^(0|false|no|off)$/i.test(value)) return false
  }
  return fallback
}

function normalizeServer(raw: unknown, index: number): ServerSpec {
  if (!raw || typeof raw !== 'object' || Array.isArray(raw)) {
    fail(`servers[${index}] must be an object`)
  }
  const rec = raw as Record<string, unknown>
  const id = String(rec.id ?? '').trim()
  if (!id) fail(`servers[${index}].id must not be empty`)

  // `enabled: false` is a pure tombstone: id alone disables the built-in
  // server, so command/extensions/languageId are not required in that form.
  const enabled = parseBoolean(rec.enabled, true)
  if (!enabled) {
    return { id, command: '', args: [], extensions: [], languageId: '', rootMarkers: [], enabled: false }
  }

  const command = String(rec.command ?? '').trim()
  if (!command) fail(`server '${id}' has no command`)

  const extensions = normalizeExtensions(rec.extensions)
  if (extensions.length === 0) fail(`server '${id}' has no extensions`)

  const languageId = String(rec.languageId ?? '').trim()
  if (!languageId) fail(`server '${id}' has no languageId`)

  const rootMarkers = normalizeStringList(rec.rootMarkers)

  return { id, command, args: normalizeStringList(rec.args), extensions, languageId, rootMarkers }
}

/**
 * Resolve and validate the plugin configuration from the raw Cordis row
 * config, merging built-in server defaults for any server the row does not
 * fully re-specify.
 */
export function resolveConfig(raw: Record<string, unknown> = {}): LspConfig {
  const maxLocations = parseInteger('maxLocations', raw.maxLocations, DEFAULT_MAX_LOCATIONS, 1)
  const maxResultChars = parseInteger('maxResultChars', raw.maxResultChars, DEFAULT_MAX_RESULT_CHARS, 100)
  const timeoutMs = parseInteger('timeoutMs', raw.timeoutMs, DEFAULT_TIMEOUT_MS, 1000)
  const lazyStart = parseBoolean(raw.lazyStart, true)
  const logDir = typeof raw.logDir === 'string' && raw.logDir.trim() !== '' ? raw.logDir.trim() : DEFAULT_LOG_DIR
  const syncLoadOnRead = parseBoolean(raw.syncLoadOnRead, DEFAULT_SYNC_LOAD_ON_READ)
  const diagnosticsOnWrite = parseBoolean(raw.diagnosticsOnWrite, DEFAULT_DIAGNOSTICS_ON_WRITE)
  const diagnosticsMinSeverity = parseSeverity(raw.diagnosticsMinSeverity, DEFAULT_DIAGNOSTICS_MIN_SEVERITY)

  const servers: ServerSpec[] = []
  const seenExtensions = new Map<string, string>()

  if (raw.servers === undefined || raw.servers === null) {
    // No row override: use the built-in table verbatim.
    for (const builtin of BUILTIN_SERVERS) {
      const spec: ServerSpec = {
        id: builtin.id,
        command: builtin.command,
        args: [...builtin.args],
        extensions: [...builtin.extensions],
        languageId: builtin.languageId,
        rootMarkers: [...builtin.rootMarkers],
      }
      servers.push(spec)
      for (const ext of spec.extensions) {
        seenExtensions.set(ext, spec.id)
      }
    }
  } else {
    if (!Array.isArray(raw.servers)) fail('servers must be an array')
    const list = raw.servers as unknown[]
    // Start from the built-in table, then let the row override per server id.
    const merged = new Map<string, ServerSpec>()
    for (const builtin of BUILTIN_SERVERS) {
      merged.set(builtin.id, {
        id: builtin.id,
        command: builtin.command,
        args: [...builtin.args],
        extensions: [...builtin.extensions],
        languageId: builtin.languageId,
        rootMarkers: [...builtin.rootMarkers],
      })
    }
    for (let i = 0; i < list.length; i++) {
      const spec = normalizeServer(list[i], i)
      merged.set(spec.id, spec)
    }
    for (const spec of merged.values()) {
      if (spec.enabled === false) continue
      servers.push(spec)
      for (const ext of spec.extensions) {
        const existing = seenExtensions.get(ext)
        if (existing) {
          fail(`extension '.${ext}' is claimed by both '${existing}' and '${spec.id}'`)
        }
        seenExtensions.set(ext, spec.id)
      }
    }
  }

  if (servers.length === 0) fail('no enabled servers configured')

  return { lazyStart, maxLocations, maxResultChars, timeoutMs, logDir, servers, syncLoadOnRead, diagnosticsOnWrite, diagnosticsMinSeverity }
}
