/**
 * Pure byte- and text-level transforms for the two repairs this plugin
 * performs. Nothing here touches the filesystem, so every rule is directly
 * unit-testable.
 *
 * The line-ending rule is the interesting one. The harness rewrite path
 * (`dsh-fs-local`) decodes with `TextDecoder`, which drops the UTF-8 BOM, and
 * `edit` then re-applies the file's *majority* terminator to the *whole* file.
 * Both effects are invisible to the model but show up as diff churn. The fix is
 * to remember the pre-write bytes and, line by line, put back exactly what the
 * write did not change while giving genuinely new lines the configured
 * terminator.
 *
 * @module dsh-encoding/encoding
 */

/** UTF-8 byte-order mark. */
export const UTF8_BOM: Uint8Array = Uint8Array.from([0xef, 0xbb, 0xbf])

export type BomKind = 'none' | 'utf8' | 'utf16le' | 'utf16be'

export interface BomSniff {
  readonly kind: BomKind
  /** Bytes the BOM occupies at the head of the buffer. */
  readonly length: number
}

/** Identify a leading BOM without decoding anything. */
export function sniffBom(bytes: Uint8Array): BomSniff {
  if (bytes.length >= 3 && bytes[0] === 0xef && bytes[1] === 0xbb && bytes[2] === 0xbf) {
    return { kind: 'utf8', length: 3 }
  }
  if (bytes.length >= 2 && bytes[0] === 0xff && bytes[1] === 0xfe) return { kind: 'utf16le', length: 2 }
  if (bytes.length >= 2 && bytes[0] === 0xfe && bytes[1] === 0xff) return { kind: 'utf16be', length: 2 }
  return { kind: 'none', length: 0 }
}

/** A NUL byte in the first 8 KiB marks a file that must not be text-processed. */
export function looksBinary(bytes: Uint8Array): boolean {
  const limit = Math.min(bytes.length, 8192)
  for (let index = 0; index < limit; index += 1) {
    if (bytes[index] === 0) return true
  }
  return false
}

const STRICT_DECODER = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true })

/**
 * Decode as UTF-8, or return `undefined` when the bytes are not valid UTF-8.
 * Re-encoding a lossily decoded buffer would rewrite invalid bytes into U+FFFD,
 * so every repair path refuses to touch such a file.
 */
export function decodeUtf8(bytes: Uint8Array): string | undefined {
  try {
    return STRICT_DECODER.decode(bytes)
  } catch {
    return undefined
  }
}

/** `true` when the text contains any code unit above U+007F. */
export function hasNonAscii(text: string): boolean {
  for (let index = 0; index < text.length; index += 1) {
    if (text.charCodeAt(index) > 0x7f) return true
  }
  return false
}

export type LineEnding = '' | '\n' | '\r' | '\r\n'

export interface Line {
  readonly body: string
  readonly eol: LineEnding
}

/**
 * Split text into `{ body, eol }` pairs. The concatenation of every
 * `body + eol` reproduces the input exactly; a trailing terminator does not
 * produce a trailing empty line, and a file without a final terminator yields
 * a last line whose `eol` is `''`. A bare CR is reported as its own
 * `'\\r'` terminator so it can be repaired rather than silently preserved.
 */
export function splitLines(text: string): Line[] {
  const lines: Line[] = []
  let start = 0
  let index = 0
  while (index < text.length) {
    const code = text.charCodeAt(index)
    if (code === 0x0d) {
      const next = index + 1 < text.length ? text.charCodeAt(index + 1) : -1
      if (next === 0x0a) {
        lines.push({ body: text.slice(start, index), eol: '\r\n' })
        index += 2
        start = index
        continue
      }
      lines.push({ body: text.slice(start, index), eol: '\r' })
      index += 1
      start = index
      continue
    }
    if (code === 0x0a) {
      lines.push({ body: text.slice(start, index), eol: '\n' })
      index += 1
      start = index
      continue
    }
    index += 1
  }
  if (start < text.length) lines.push({ body: text.slice(start), eol: '' })
  return lines
}

/** Inverse of {@link splitLines}. */
export function joinLines(lines: readonly Line[]): string {
  let out = ''
  for (const line of lines) out += line.body + line.eol
  return out
}

/** Majority terminator of a file, used by the `'preserve'` EOL policy. */
export function majorityEol(lines: readonly Line[]): '\r\n' | '\n' {
  let crlf = 0
  let lf = 0
  for (const line of lines) {
    if (line.eol === '\r\n') crlf += 1
    else if (line.eol === '\n') lf += 1
  }
  return crlf >= lf ? '\r\n' : '\n'
}

/**
 * Most frequent terminator per line body. Used inside the changed region as a
 * fallback so a line the model re-emitted verbatim keeps the terminator the
 * file already used for that text.
 */
function buildEndingIndex(lines: readonly Line[]): Map<string, LineEnding> {
  const counts = new Map<string, Map<LineEnding, number>>()
  for (const line of lines) {
    let perBody = counts.get(line.body)
    if (perBody === undefined) {
      perBody = new Map()
      counts.set(line.body, perBody)
    }
    perBody.set(line.eol, (perBody.get(line.eol) ?? 0) + 1)
  }
  const index = new Map<string, LineEnding>()
  for (const [body, perBody] of counts) {
    let best: LineEnding = ''
    let bestCount = -1
    for (const [eol, count] of perBody) {
      if (count > bestCount) {
        best = eol
        bestCount = count
      }
    }
    index.set(body, best)
  }
  return index
}

export interface EolPlan {
  /** Terminator for lines the write introduced or changed. */
  readonly target: '\r\n' | '\n'
  /** Keep the pre-write terminator of every unchanged line. */
  readonly preserveUntouched: boolean
  /** Replace any bare CR with {@link EolPlan.target} (MSVC C4335 guard). */
  readonly fixLoneCr: boolean
}

/**
 * Apply {@link EolPlan} to `current`, using `original` (the pre-write text, or
 * `null` for a new file) to decide which lines count as untouched.
 *
 * Alignment is two-tier: exact positional agreement for the common prefix and
 * suffix of the two files (cheap, O(n), and precisely "these lines did not
 * move"), then a body-keyed terminator lookup inside the changed region so a
 * line the model re-emitted verbatim still keeps its own terminator. Only text
 * with no precedent in the original — genuinely new or edited lines — receives
 * {@link EolPlan.target}.
 */
export function repairLineEndings(current: string, original: string | null, plan: EolPlan): string {
  const currentLines = splitLines(current)
  if (currentLines.length === 0) return current

  let originalLines: Line[] | null = original === null ? null : splitLines(original)
  if (originalLines !== null && originalLines.length === 0) originalLines = null

  const preserve = originalLines !== null && plan.preserveUntouched

  let prefix = 0
  let suffix = 0
  if (preserve && originalLines !== null) {
    const bound = Math.min(originalLines.length, currentLines.length)
    while (prefix < bound && originalLines[prefix]!.body === currentLines[prefix]!.body) prefix += 1
    const available = bound - prefix
    while (
      suffix < available &&
      originalLines[originalLines.length - 1 - suffix]!.body ===
        currentLines[currentLines.length - 1 - suffix]!.body
    ) {
      suffix += 1
    }
  }

  const endingIndex = preserve && originalLines !== null ? buildEndingIndex(originalLines) : null
  const originalLength = originalLines?.length ?? 0

  const out: Line[] = []
  for (let index = 0; index < currentLines.length; index += 1) {
    const line = currentLines[index]!
    if (line.eol === '') {
      // The write decided there is no final terminator; never invent one.
      out.push({ body: line.body, eol: '' })
      continue
    }
    let eol: LineEnding
    if (preserve && originalLines !== null && index < prefix) {
      eol = originalLines[index]!.eol
    } else if (preserve && originalLines !== null && suffix > 0 && index >= currentLines.length - suffix) {
      eol = originalLines[originalLength - (currentLines.length - index)]!.eol
    } else if (endingIndex !== null) {
      eol = endingIndex.get(line.body) ?? plan.target
    } else {
      eol = plan.target
    }
    if (eol === '') eol = plan.target
    if (plan.fixLoneCr && eol === '\r') eol = plan.target
    out.push({ body: line.body, eol })
  }
  return joinLines(out)
}

export type BomPolicy = 'non-ascii' | 'always' | 'never' | 'preserve'

/**
 * Decide the BOM for the repaired text.
 *
 * `'non-ascii'` is additive on purpose: it adds the BOM that a non-ASCII source
 * needs, and never strips a BOM the file already carried. Removing an existing
 * BOM could only churn a file that compiles today, and the WPS tree contains no
 * BOM-carrying file that is pure ASCII — so the asymmetric rule and the strict
 * biconditional agree on every real file while the asymmetric one is safe.
 */
export function decideBom(policy: BomPolicy, text: string, originalHadBom: boolean): boolean {
  switch (policy) {
    case 'always':
      return true
    case 'never':
      return false
    case 'preserve':
      return originalHadBom
    case 'non-ascii':
    default:
      return originalHadBom || hasNonAscii(text)
  }
}

/** Encode text back to bytes, optionally prefixed with a UTF-8 BOM. */
export function encodeUtf8(text: string, withBom: boolean): Uint8Array {
  const encoded = new Uint8Array(Buffer.from(text, 'utf8'))
  if (!withBom) return encoded
  const out = new Uint8Array(UTF8_BOM.length + encoded.length)
  out.set(UTF8_BOM, 0)
  out.set(encoded, UTF8_BOM.length)
  return out
}

/** Number of lines whose terminator differs between two versions of a text. */
export function countRewrittenEndings(before: string, after: string): number {
  const left = splitLines(before)
  const right = splitLines(after)
  let changed = 0
  const bound = Math.min(left.length, right.length)
  for (let index = 0; index < bound; index += 1) {
    if (left[index]!.eol !== right[index]!.eol) changed += 1
  }
  return changed + Math.abs(left.length - right.length)
}
