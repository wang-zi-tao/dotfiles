/**
 * Candidate convergence for `workspaceSymbol`.
 *
 * clangd answers `workspace/symbol` from its Dex index: the dynamic part
 * (documents `didOpen`ed during this session) plus the background index. On a
 * very large project the background index may not be loaded (or is still
 * loading), and then a query for a symbol whose file nobody opened returns
 * nothing even though the symbol exists.
 *
 * This module narrows a project down to a handful of candidate files so the
 * caller can `didOpen` them (which fills the dynamic index immediately) and
 * retry the index query. Discovery is best-effort and fail-open:
 *
 *   1. `name`    — ripgrep `--files -g '*<hint>*'`: cheap, matches the
 *                   "class name ≈ file name" convention of the codebase;
 *   2. `content` — ripgrep `-l --fixed-strings -i`: slower full scan, finds
 *                   the provider when the file name does not contain the hint;
 *   3. `none`    — nothing found, or the search command is unusable.
 *
 * Every failure path (missing search command, timeout, unreadable output)
 * degrades to "no candidates" rather than an error, so a symbol query never
 * fails because the optional fallback could not run.
 */

import { basename, extname, resolve } from 'node:path'

import type {
  CandidateSearchOptions,
  CandidateSearchResult,
  CandidateStrategy,
  LoggerLike,
  SubprocessHandle,
  SubprocessRuntime,
} from './types.js'

/** Declaration-file extensions, preferred when ranking equally named files. */
const HEADER_EXTENSIONS = new Set(['h', 'hpp', 'hh', 'hxx', 'h++', 'inl'])

/** Upper bound on a single search command's captured stdout. */
const MAX_OUTPUT_BYTES = 512 * 1024

/** How many distinct name hints may become `-g` globs. */
const MAX_NAME_GLOBS = 3

/**
 * Derive file-name hints from a symbol query.
 *
 * `KAlgFinancial::PMT` yields the owning terms in several spellings, because
 * the file on disk may be `KAlgFinancial.h` or `alg_financial.cpp`:
 * `KAlgFinancial`, `kalgfinancial`, `kalg_financial`, `alg_financial`, `PMT`,
 * `pmt`. Segments shorter than 3 characters are dropped (too noisy).
 */
export function symbolNameHints(query: string): string[] {
  const segments = query
    .split(/::|->|[{}(),;<>\s.]+/)
    .map(segment => segment.trim())
    .filter(segment => segment.length >= 3)

  const hints: string[] = []
  const push = (value: string): void => {
    if (value.length >= 3 && !hints.includes(value)) hints.push(value)
  }

  for (const segment of segments) {
    push(segment)
    push(segment.toLowerCase())
    const snake = toSnakeCase(segment)
    push(snake.toLowerCase())
    // `KAlgFinancial` -> `k_alg_financial`: also offer the de-prefixed form,
    // which is how the owning module is usually spelled on disk.
    const stripped = snake.replace(/^[A-Za-z]_/, '')
    if (stripped !== snake) push(stripped.toLowerCase())
  }
  return hints
}

/** `KAlgFinancial` -> `K_Alg_Financial`; `KJdeConsole` -> `K_Jde_Console`. */
function toSnakeCase(value: string): string {
  return value.replace(/([A-Z]+)([A-Z][a-z])/g, '$1_$2').replace(/([a-z0-9])([A-Z])/g, '$1_$2')
}

/** Score one path against the hints; 0 means "not a candidate". */
function scorePath(path: string, hints: readonly string[], unmatchedScore: number): number {
  const extension = extname(path).replace(/^\./, '').toLowerCase()
  const base = basename(path, extname(path)).toLowerCase()
  let score = 0
  for (const hint of hints) {
    const lowered = hint.toLowerCase()
    if (base === lowered) score = Math.max(score, 100)
    else if (base.startsWith(lowered)) score = Math.max(score, 60)
    else if (base.endsWith(lowered)) score = Math.max(score, 50)
    else if (base.includes(lowered)) score = Math.max(score, 30)
  }
  if (score === 0) {
    if (unmatchedScore <= 0) return 0
    score = unmatchedScore
  }
  if (HEADER_EXTENSIONS.has(extension)) score += 15
  // Shallow files are usually the owning module rather than a deep copy.
  return score - Math.min(path.split(/[\\/]/).length, 12)
}

export interface RankOptions {
  /** Score for a path that matched content but whose file name holds no hint. */
  unmatchedScore?: number
}

/**
 * Filter candidates by the server's extensions, dedupe case-insensitively
 * (Windows paths), rank by name similarity, and cap the result.
 */
export function rankCandidates(
  paths: readonly string[],
  hints: readonly string[],
  extensions: readonly string[],
  maxCandidates: number,
  options: RankOptions = {},
): { candidates: string[]; matched: number; truncated: boolean } {
  const allowed = new Set(extensions.map(extension => extension.replace(/^\./, '').toLowerCase()))
  const unmatchedScore = options.unmatchedScore ?? 0
  const best = new Map<string, { path: string; score: number }>()

  for (const raw of paths) {
    const path = raw.trim()
    if (!path) continue
    const extension = extname(path).replace(/^\./, '').toLowerCase()
    if (allowed.size > 0 && !allowed.has(extension)) continue
    const score = scorePath(path, hints, unmatchedScore)
    if (score <= 0) continue
    const key = path.toLowerCase()
    const existing = best.get(key)
    if (!existing || existing.score < score) best.set(key, { path, score })
  }

  const ranked = [...best.values()].sort(
    (a, b) => (b.score - a.score) || (a.path < b.path ? -1 : a.path > b.path ? 1 : 0),
  )
  const limit = Math.max(1, maxCandidates)
  return {
    candidates: ranked.slice(0, limit).map(entry => entry.path),
    matched: ranked.length,
    truncated: ranked.length > limit,
  }
}

/** `rg --files -g '*<hint>*'` — name-based discovery, no content scan. */
export function ripgrepNameArgs(hints: readonly string[]): string[] {
  const args = ['--files', '--no-messages']
  for (const hint of hints.slice(0, MAX_NAME_GLOBS)) args.push('-g', `*${hint}*`)
  return args
}

/** `rg -l --fixed-strings -i -g '*.h' ... -- <name>` — content-based discovery. */
export function ripgrepContentArgs(name: string, extensions: readonly string[]): string[] {
  const args = [
    '--files-with-matches',
    '--max-count',
    '1',
    '--no-messages',
    '--fixed-strings',
    '--ignore-case',
  ]
  for (const extension of extensions) args.push('-g', `*.${extension.replace(/^\./, '')}`)
  args.push('--', name)
  return args
}

function splitLines(text: string): string[] {
  return text.split(/\r?\n/)
}

/** Run one search command, capped by `timeoutMs`; never throws. */
async function runSearch(
  subprocess: SubprocessRuntime,
  command: string,
  args: readonly string[],
  cwd: string,
  options: CandidateSearchOptions,
): Promise<string> {
  let handle: SubprocessHandle
  try {
    handle = subprocess.spawn({
      argv: [command, ...args],
      cwd,
      stdio: {
        stdin: 'ignore',
        stdout: { maxBytes: MAX_OUTPUT_BYTES },
        stderr: { maxBytes: 8192 },
      },
      graceMs: 2000,
      ...(options.signal ? { signal: options.signal } : {}),
    })
  } catch {
    // Missing search command (ENOENT) or a refused spawn: no candidates.
    return ''
  }

  const timer = setTimeout(() => {
    try {
      handle.terminate()
    } catch {
      /* the scan is already gone */
    }
  }, Math.max(100, options.timeoutMs))

  try {
    await handle.done
  } catch {
    /* a non-zero exit still may have produced usable output */
  } finally {
    clearTimeout(timer)
  }

  try {
    return handle.collected?.stdout?.finalize().text ?? ''
  } catch {
    return ''
  }
}

/**
 * Converge on candidate files for a `workspaceSymbol` query.
 *
 * Returns absolute paths (best first), the strategy that produced them, and
 * the unmatched match count before capping.
 */
export async function searchCandidates(
  subprocess: SubprocessRuntime,
  logger: LoggerLike,
  options: CandidateSearchOptions,
): Promise<CandidateSearchResult> {
  const empty: CandidateSearchResult = { candidates: [], strategy: 'none', matched: 0, truncated: false }
  const query = options.query.trim()
  if (!query) return empty

  const root = resolve(options.root)
  const command = options.command?.trim() || 'rg'
  const hints = symbolNameHints(query)

  try {
    if (hints.length > 0) {
      const nameOutput = await runSearch(subprocess, command, ripgrepNameArgs(hints), root, options)
      const byName = rankCandidates(splitLines(nameOutput), hints, options.extensions, options.maxCandidates)
      if (byName.candidates.length > 0) {
        return { candidates: byName.candidates, strategy: 'name', matched: byName.matched, truncated: byName.truncated }
      }
    }

    const name = hints[0] ?? query
    const contentOutput = await runSearch(subprocess, command, ripgrepContentArgs(name, options.extensions), root, options)
    const byContent = rankCandidates(splitLines(contentOutput), hints, options.extensions, options.maxCandidates, {
      unmatchedScore: 10,
    })
    if (byContent.candidates.length > 0) {
      return { candidates: byContent.candidates, strategy: 'content', matched: byContent.matched, truncated: byContent.truncated }
    }
    return empty
  } catch (error) {
    logger.debug(
      `dsh-lsp: candidate search failed for '${query}': ${error instanceof Error ? error.message : String(error)}`,
    )
    return empty
  }
}

/** Re-exported so callers can render the strategy that produced a result. */
export type { CandidateStrategy }
