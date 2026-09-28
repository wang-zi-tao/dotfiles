/**
 * Row-config resolution: built-in defaults < cordis.patch.yml.
 *
 * Resolution is tolerant by design - an unknown enum value falls back to the
 * default instead of taking the whole plugin (and therefore every tool call in
 * the session) down with it. The resolved object is what every hot path reads,
 * so nothing downstream re-validates.
 *
 * @module dsh-encoding/config
 */

import { GlobSet } from './glob.js'
import type {
  BomPolicy,
  EncodingConfig,
  EncodingOverride,
  EolPolicy,
  FilePolicy,
  LogLevel,
  ResolvedConfig,
  ResolvedOverride,
} from './types.js'

export const DEFAULT_ENABLED = true
export const DEFAULT_TOOLS: readonly string[] = Object.freeze(['write', 'edit'])

/** C/C++ sources plus the CMake family, matched at any depth. */
export const DEFAULT_INCLUDE: readonly string[] = Object.freeze([
  '**/*.{c,cc,cpp,cxx,c++,h,hh,hpp,hxx,h++,inl,ipp,tpp}',
  '**/CMakeLists.txt',
  '**/*.cmake',
  '**/*.cmake.in',
])

/** Trees where rewriting a file is never helpful. */
export const DEFAULT_EXCLUDE: readonly string[] = Object.freeze([
  '**/.git/**',
  '**/.vs/**',
  '**/.idea/**',
  '**/node_modules/**',
  '**/CMakeFiles/**',
])

export const DEFAULT_BOM: BomPolicy = 'non-ascii'
export const DEFAULT_EOL: EolPolicy = 'crlf'
export const DEFAULT_PRESERVE_UNTOUCHED_EOL = true
export const DEFAULT_FIX_LONE_CR = true
export const DEFAULT_MAX_SNAPSHOT_ENTRIES = 64
export const DEFAULT_SNAPSHOT_TTL_MS = 5 * 60 * 1000
export const DEFAULT_LOG: LogLevel = 'summary'
export const DEFAULT_DRY_RUN = false

/** The CMake family, which the C4819 BOM rule does not apply to. */
export const CMAKE_GLOBS: readonly string[] = Object.freeze([
  '**/CMakeLists.txt',
  '**/*.cmake',
  '**/*.cmake.in',
])

/**
 * CMake files carry no BOM obligation: CMake is not compiled by MSVC, so there
 * is no C4819 to dodge, and 80% of the tree's CMakeLists.txt files have no BOM.
 *
 * A later profile patch can replace this row config wholesale; these built-in
 * overrides are always appended after the row's own, so an override in
 * cordis.patch.yml is consulted first.
 *
 * 'preserve' is the narrow reading of "CMake does not need a BOM": the plugin
 * never adds one for CMake, and never strips one the file already had either,
 * because removing it is a gratuitous diff on a file that configures fine
 * today. Switch this to 'never' to converge the tree on BOM-free CMake.
 */
export const DEFAULT_OVERRIDES: readonly EncodingOverride[] = Object.freeze([
  Object.freeze({ match: CMAKE_GLOBS, bom: 'preserve' as BomPolicy }),
])

const BOM_POLICIES: readonly BomPolicy[] = ['non-ascii', 'always', 'never', 'preserve']
const EOL_POLICIES: readonly EolPolicy[] = ['crlf', 'lf', 'preserve']
const LOG_LEVELS: readonly LogLevel[] = ['off', 'summary', 'debug']

function pickBoolean(value: unknown, fallback: boolean): boolean {
  return typeof value === 'boolean' ? value : fallback
}

function pickInteger(value: unknown, fallback: number, minimum: number): number {
  if (typeof value !== 'number' || !Number.isFinite(value)) return fallback
  const truncated = Math.trunc(value)
  return truncated >= minimum ? truncated : fallback
}

function pickEnum(value: unknown, allowed: readonly string[], fallback: string): any {
  return typeof value === 'string' && allowed.includes(value) ? value : fallback
}

/** Like pickEnum, but "not given" stays undefined so a policy patch can fall through. */
function pickOptionalEnum(value: unknown, allowed: readonly string[]): string | undefined {
  return typeof value === 'string' && allowed.includes(value) ? value : undefined
}

function pickStringArray(value: unknown, fallback: readonly string[]): readonly string[] {
  if (!Array.isArray(value)) return fallback
  const picked = value.filter((entry): entry is string => typeof entry === 'string' && entry.trim() !== '')
  return picked.length > 0 ? Object.freeze(picked) : fallback
}

/**
 * Row-config overrides are tried BEFORE the built-in ones, which is the same
 * "defaults < row config" rule the rest of the plugin follows: a user override
 * that names `bom` for CMake wins, and an unrelated user override does
 * not silently drop the built-in CMake rule.
 */
function resolveOverrides(value: unknown, fallback: readonly EncodingOverride[]): readonly ResolvedOverride[] {
  const source = [...(Array.isArray(value) ? value : []), ...fallback]
  const resolved: ResolvedOverride[] = []
  for (const entry of source) {
    if (entry === null || typeof entry !== 'object') continue
    const record = entry as Record<string, unknown>
    const match = pickStringArray(record.match, [])
    if (match.length === 0) continue
    const bom = pickOptionalEnum(record.bom, BOM_POLICIES)
    const eol = pickOptionalEnum(record.eol, EOL_POLICIES)
    if (bom === undefined && eol === undefined) continue
    resolved.push({
      match: new GlobSet(match),
      ...(bom === undefined ? {} : { bom: bom as BomPolicy }),
      ...(eol === undefined ? {} : { eol: eol as EolPolicy }),
    })
  }
  return Object.freeze(resolved)
}

/** Merge a raw row config over the built-in defaults. */
export function resolveConfig(raw: Record<string, unknown> = {}): ResolvedConfig {
  const config = (raw ?? {}) as EncodingConfig
  const include = pickStringArray(config.include, DEFAULT_INCLUDE)
  const exclude = pickStringArray(config.exclude, DEFAULT_EXCLUDE)
  return {
    enabled: pickBoolean(config.enabled, DEFAULT_ENABLED),
    tools: pickStringArray(config.tools, DEFAULT_TOOLS),
    include,
    exclude,
    includeSet: new GlobSet(include),
    excludeSet: new GlobSet(exclude),
    bom: pickEnum(config.bom, BOM_POLICIES, DEFAULT_BOM) as BomPolicy,
    eol: pickEnum(config.eol, EOL_POLICIES, DEFAULT_EOL) as EolPolicy,
    overrides: resolveOverrides(config.overrides, DEFAULT_OVERRIDES),
    preserveUntouchedEol: pickBoolean(config.preserveUntouchedEol, DEFAULT_PRESERVE_UNTOUCHED_EOL),
    fixLoneCr: pickBoolean(config.fixLoneCr, DEFAULT_FIX_LONE_CR),
    maxSnapshotEntries: pickInteger(config.maxSnapshotEntries, DEFAULT_MAX_SNAPSHOT_ENTRIES, 1),
    snapshotTtlMs: pickInteger(config.snapshotTtlMs, DEFAULT_SNAPSHOT_TTL_MS, 0),
    log: pickEnum(config.log, LOG_LEVELS, DEFAULT_LOG) as LogLevel,
    dryRun: pickBoolean(config.dryRun, DEFAULT_DRY_RUN),
  }
}

/** true when filePath is selected by the include set and not rejected by the exclude set. */
export function isSelected(config: ResolvedConfig, filePath: string): boolean {
  if (!config.includeSet.matches(filePath)) return false
  return !config.excludeSet.matches(filePath)
}

/**
 * Fold the matching overrides into the config, field by field: the first
 * override that names a field wins it, and later overrides still get to supply
 * fields the earlier ones left alone.
 */
export function resolveFilePolicy(config: ResolvedConfig, filePath: string): FilePolicy {
  let bom = config.bom
  let eol = config.eol
  let bomSet = false
  let eolSet = false
  for (const override of config.overrides) {
    if (!override.match.matches(filePath)) continue
    if (!bomSet && override.bom !== undefined) {
      bom = override.bom
      bomSet = true
    }
    if (!eolSet && override.eol !== undefined) {
      eol = override.eol
      eolSet = true
    }
    if (bomSet && eolSet) break
  }
  return { bom, eol, preserveUntouchedEol: config.preserveUntouchedEol, fixLoneCr: config.fixLoneCr }
}
