/**
 * Protocol mapping: file URIs and UTF-16 positions.
 *
 * LSP uses zero-based UTF-16 `Position`s and `file:` URIs; the tool surface
 * uses one-based UTF-16 cursor coordinates and filesystem paths. This module
 * owns every conversion so the rest of the plugin never applies host-platform
 * path rules to a possibly-symlinked root.
 */

import { isAbsolute, relative } from 'node:path'
import { readFileSync } from 'node:fs'

import type { Diagnostic, Position, Range } from 'vscode-languageserver-protocol'

import type { DiagnosticEntry, LspLocation, LspRange, SymbolEntry } from './types.js'

/** Convert a Windows or POSIX absolute path to a `file:` URI. */
export function pathToFileUri(path: string): string {
  let normalized = path.replace(/\\/g, '/')
  if (!normalized.startsWith('/')) normalized = '/' + normalized
  return 'file://' + encodeURI(normalized).replace(/#/g, '%23').replace(/\?/g, '%3F')
}

/** Convert a `file:` URI to an absolute path (Windows or POSIX). */
export function fileUriToPath(uri: string): string {
  if (!uri.startsWith('file://')) {
    throw new Error(`not a file URI: ${uri}`)
  }
  const decoded = decodeURIComponent(uri.slice('file://'.length))
  // Windows drive form: file:///D:/path -> /D:/path -> D:/path
  if (/^\/[A-Za-z]:/.test(decoded)) {
    return decoded.slice(1).replace(/\//g, '\\')
  }
  // POSIX: /path stays; but keep forward slashes on POSIX semantics.
  return decoded
}

/** True when `uri` is a `file:` URI. */
export function isFileUri(uri: string): boolean {
  return uri.startsWith('file://')
}

/** Convert a 1-based tool position to a zero-based LSP Position. */
export function toLspPosition(line: number, character: number): Position {
  return { line: Math.max(0, line - 1), character: Math.max(0, character - 1) }
}

/** Convert a zero-based LSP Position to a 1-based tool position. */
export function fromLspPosition(position: Position): { line: number; character: number } {
  return { line: position.line + 1, character: position.character + 1 }
}

/** Convert a zero-based LSP Range to a 1-based tool range. */
export function fromLspRange(range: Range): LspRange {
  const start = fromLspPosition(range.start)
  const end = fromLspPosition(range.end)
  return {
    startLine: start.line,
    startCharacter: start.character,
    endLine: end.line,
    endCharacter: end.character,
  }
}

/** True when `uri` lives inside (or at) `rootUri` (both canonical file URIs). */
export function isUriInside(uri: string, rootUri: string): boolean {
  if (!isFileUri(uri) || !isFileUri(rootUri)) return false
  return uri === rootUri || uri.startsWith(rootUri.endsWith('/') ? rootUri : rootUri + '/')
}

/**
 * Project a raw LSP location onto a model-facing location: an absolute
 * filesystem path when outside the root, a workspace-relative path when
 * inside it.
 */
export function toLspLocation(rawUri: string, rawRange: Range, rootUri: string): LspLocation {
  let path: string
  if (isFileUri(rawUri)) {
    const absolute = fileUriToPath(rawUri)
    if (isUriInside(rawUri, rootUri)) {
      try {
        const rootPath = fileUriToPath(rootUri)
        path = relative(rootPath, absolute) || '.'
      } catch {
        path = absolute
      }
    } else {
      path = absolute
    }
  } else {
    path = rawUri
  }
  return { uri: rawUri, path, range: fromLspRange(rawRange) }
}

/** Project a SymbolInformation-style location onto a model-facing symbol entry. */
export function toSymbolEntry(raw: {
  name: string
  kind: number | string
  location: { uri: string; range?: Range }
  containerName?: string
}, rootUri: string): SymbolEntry {
  let kind: string
  if (typeof raw.kind === 'number') kind = SymbolKindName[raw.kind] ?? String(raw.kind)
  else kind = raw.kind

  const entry: SymbolEntry = {
    name: raw.name,
    kind,
    location: toLspLocation(raw.location.uri, raw.location.range ?? { start: { line: 0, character: 0 }, end: { line: 0, character: 0 } }, rootUri),
  }
  if (raw.containerName) entry.containerName = raw.containerName
  return entry
}

/**
 * Project raw LSP pull-diagnostics onto model-facing entries. Severity maps
 * LSP's numeric DiagnosticSeverity (1=error, 2=warning, 3=information, 4=hint)
 * to the shared severity vocabulary; ranges convert from 0-based UTF-16 to the
 * tool surface's 1-based coordinates.
 */
export function toDiagnosticEntries(raw: readonly Diagnostic[] | null | undefined): DiagnosticEntry[] {
  return (raw ?? []).map(d => {
    const severity = d.severity === 1 ? 'error' : d.severity === 2 ? 'warning' : d.severity === 3 ? 'information' : 'hint'
    const entry: DiagnosticEntry = {
      severity,
      message: typeof d.message === 'string' ? d.message : d.message.value,
      range: {
        startLine: d.range.start.line + 1,
        startCharacter: d.range.start.character + 1,
        endLine: d.range.end.line + 1,
        endCharacter: d.range.end.character + 1,
      },
    }
    if (d.code !== undefined) entry.code = String(d.code)
    if (d.source) entry.source = d.source
    return entry
  })
}

/** Human-readable names for the LSP SymbolKind enum (subset covers the common cases). */
const SymbolKindName: Record<number, string> = {
  1: 'file', 2: 'module', 3: 'namespace', 4: 'package', 5: 'class', 6: 'method',
  7: 'property', 8: 'field', 9: 'constructor', 10: 'enum', 11: 'interface',
  12: 'function', 13: 'variable', 14: 'constant', 15: 'string', 16: 'number',
  17: 'boolean', 18: 'array', 19: 'object', 20: 'key', 21: 'null', 22: 'enumMember',
  23: 'struct', 24: 'event', 25: 'operator', 26: 'typeParameter',
}

/** True when a value is an absolute filesystem path. */
export function isAbsolutePath(path: string): boolean {
  return isAbsolute(path)
}

/**
 * Read one line's trimmed content from a file for a location preview. The
 * location carries an absolute `uri` (a `file:` URI) even when its model-facing
 * `path` is workspace-relative, so the preview resolves through the URI rather
 * than the display path. Returns an empty string when the file or line is
 * unreadable (best-effort; previews must never fail a query).
 */
export function readLinePreview(location: LspLocation): string {
  try {
    if (!isFileUri(location.uri)) return ''
    const absolute = fileUriToPath(location.uri)
    const line = location.range.startLine // 1-based
    if (line < 1) return ''
    const text = readFileSync(absolute, 'utf8')
    const lines = text.split(/\r?\n/)
    const content = lines[line - 1]
    return content === undefined ? '' : content.trim()
  } catch {
    return ''
  }
}

/**
 * Render a location list to stable text, capping at `maxLocations`. Each line
 * is `path:line:character` optionally followed by a ` │ ` code-line preview
 * when `withPreview` is true. Returns `{ text, truncated }`.
 */
export function renderLocations(
  title: string,
  locations: LspLocation[],
  maxLocations: number,
  withPreview = false,
): { text: string; truncated: boolean } {
  const truncated = locations.length > maxLocations
  const shown = truncated ? locations.slice(0, maxLocations) : locations
  const lines = [title]
  for (const loc of shown) {
    const head = `${loc.path}:${loc.range.startLine}:${loc.range.startCharacter}`
    if (withPreview) {
      const preview = readLinePreview(loc)
      lines.push(preview ? `${head} │ ${preview}` : head)
    } else {
      lines.push(head)
    }
  }
  if (truncated) {
    lines.push(`… ${locations.length - maxLocations} more location(s) omitted (maxLocations=${maxLocations})`)
  }
  return { text: lines.join('\n'), truncated }
}
