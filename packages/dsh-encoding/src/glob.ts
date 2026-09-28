/**
 * A tiny dependency-free glob matcher for the plugin's `include`/`exclude`
 * file rules.
 *
 * Supported syntax:
 *   `**`      any number of path segments; as a leading segment followed by
 *             a slash it also matches zero segments
 *   `*`       any number of non-separator characters
 *   `?`       exactly one non-separator character
 *   `{a,b,c}` alternation (nestable)
 *
 * Everything else is literal. Character classes are deliberately NOT
 * supported, so a `[` in a pattern stays a literal bracket instead of
 * silently becoming a malformed class. Backslashes are treated as path
 * separators, so Windows-style rules work unchanged.
 *
 * A pattern containing no `/` is matched against the basename at any depth,
 * mirroring the harness glob tool.
 *
 * @module dsh-encoding/glob
 */

const REGEXP_METACHARACTERS = '.+^$()|[]{}?*\\'

function literal(character: string): string {
  return REGEXP_METACHARACTERS.includes(character) ? '\\' + character : character
}

/** Translate one glob pattern into an unanchored regular-expression source. */
export function globToRegExpSource(pattern: string): string {
  const source = pattern.replace(/\\/g, '/')
  let out = ''
  let index = 0
  while (index < source.length) {
    const character = source[index]!
    if (character === '*') {
      if (source[index + 1] === '*') {
        if (source[index + 2] === '/') {
          // `**/` — zero or more whole segments, so it also matches a bare name.
          out += '(?:[^/]*/)*'
          index += 3
        } else {
          out += '.*'
          index += 2
        }
      } else {
        out += '[^/]*'
        index += 1
      }
      continue
    }
    if (character === '?') {
      out += '[^/]'
      index += 1
      continue
    }
    if (character === '{') {
      const close = source.indexOf('}', index + 1)
      if (close !== -1) {
        const alternatives = source.slice(index + 1, close).split(',')
        out += '(?:' + alternatives.map((part) => globToRegExpSource(part)).join('|') + ')'
        index = close + 1
        continue
      }
    }
    out += literal(character)
    index += 1
  }
  return out
}

export interface GlobSetOptions {
  /** Override the default case-insensitivity (true on Windows, false elsewhere). */
  readonly caseInsensitive?: boolean
}

interface CompiledRule {
  readonly regex: RegExp
  readonly basenameOnly: boolean
}

/**
 * A compiled set of glob rules. An empty set matches nothing, which is what
 * both `include` (nothing selected) and `exclude` (nothing rejected) need.
 */
export class GlobSet {
  private readonly rules: readonly CompiledRule[]

  constructor(patterns: readonly string[] = [], options: GlobSetOptions = {}) {
    const insensitive = options.caseInsensitive ?? process.platform === 'win32'
    const flags = insensitive ? 'i' : ''
    this.rules = patterns
      .filter((pattern): pattern is string => typeof pattern === 'string' && pattern.trim() !== '')
      .map((raw) => {
        const pattern = raw.trim().replace(/\\/g, '/').replace(/^\.\//, '')
        const basenameOnly = !pattern.includes('/')
        return { regex: new RegExp('^' + globToRegExpSource(pattern) + '$', flags), basenameOnly }
      })
  }

  get size(): number {
    return this.rules.length
  }

  matches(filePath: string): boolean {
    if (this.rules.length === 0) return false
    const normalized = filePath.replace(/\\/g, '/')
    const slash = normalized.lastIndexOf('/')
    const basename = slash === -1 ? normalized : normalized.slice(slash + 1)
    return this.rules.some((rule) => rule.regex.test(rule.basenameOnly ? basename : normalized))
  }
}
