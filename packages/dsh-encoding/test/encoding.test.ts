import test from 'node:test'
import { deepEqual, equal, notEqual, ok } from 'node:assert/strict'

import {
  countRewrittenEndings,
  decideBom,
  decodeUtf8,
  encodeUtf8,
  hasNonAscii,
  joinLines,
  looksBinary,
  majorityEol,
  repairLineEndings,
  sniffBom,
  splitLines,
  UTF8_BOM,
  type EolPlan,
} from '../src/encoding.js'

const LF = '\n'
const CRLF = '\r\n'
const CR = '\r'

const CRLF_PLAN: EolPlan = { target: '\r\n', preserveUntouched: true, fixLoneCr: true }

function crlf(text: string): string {
  return text.split(LF).join(CRLF)
}

test('splitLines round-trips every terminator shape', () => {
  for (const sample of ['', 'a', 'a' + LF, 'a' + LF + LF, 'a' + CRLF + 'b', 'a' + CR + 'b', 'a' + LF + 'b' + CRLF]) {
    equal(joinLines(splitLines(sample)), sample)
  }
})

test('splitLines reports a trailing terminator without inventing a line', () => {
  deepEqual(splitLines('a' + LF), [{ body: 'a', eol: '\n' }])
  deepEqual(splitLines('a'), [{ body: 'a', eol: '' }])
  deepEqual(splitLines('a' + CR + LF + 'b'), [
    { body: 'a', eol: '\r\n' },
    { body: 'b', eol: '' },
  ])
  deepEqual(splitLines('a' + CR + 'b'), [
    { body: 'a', eol: '\r' },
    { body: 'b', eol: '' },
  ])
})

test('majorityEol prefers CRLF on a tie', () => {
  equal(majorityEol(splitLines('a' + CRLF + 'b' + LF)), '\r\n')
  equal(majorityEol(splitLines('a' + LF + 'b' + LF)), '\n')
  equal(majorityEol(splitLines('a' + LF + 'b' + LF + 'c' + CRLF)), '\n')
})

test('hasNonAscii sees non-ASCII code units', () => {
  equal(hasNonAscii('int main() {}'), false)
  equal(hasNonAscii('// \u4e2d\u6587'), true)
})

test('sniffBom identifies each BOM family', () => {
  equal(sniffBom(encodeUtf8('a', false)).kind, 'none')
  equal(sniffBom(encodeUtf8('a', true)).kind, 'utf8')
  equal(sniffBom(Uint8Array.from([0xff, 0xfe, 0x61, 0x00])).kind, 'utf16le')
  equal(sniffBom(Uint8Array.from([0xfe, 0xff, 0x00, 0x61])).kind, 'utf16be')
  deepEqual(Array.from(UTF8_BOM), [0xef, 0xbb, 0xbf])
})

test('looksBinary spots NUL bytes and ignores ordinary text', () => {
  equal(looksBinary(encodeUtf8('int x;', false)), false)
  equal(looksBinary(Uint8Array.from([0x61, 0x00, 0x62])), true)
})

test('decodeUtf8 rejects invalid UTF-8 instead of producing replacement text', () => {
  equal(decodeUtf8(Uint8Array.from([0x61, 0x62])), 'ab')
  equal(decodeUtf8(Uint8Array.from([0x61, 0xff, 0xfe, 0x62])), undefined)
})

test('encodeUtf8 prefixes the BOM only when asked and ignores an embedded BOM', () => {
  deepEqual(Array.from(encodeUtf8('a', true)), [0xef, 0xbb, 0xbf, 0x61])
  deepEqual(Array.from(encodeUtf8('a', false)), [0x61])
  // A decode with ignoreBOM keeps the character, which is why the caller strips
  // the BOM by offset rather than relying on the decoder.
  equal(decodeUtf8(Uint8Array.from([0xef, 0xbb, 0xbf, 0x61])), '\ufeffa')
})

test('repairLineEndings leaves an unchanged all-LF file alone', () => {
  const original = 'int a;' + LF + 'int b;' + LF
  equal(repairLineEndings(original, original, CRLF_PLAN), original)
})

test('repairLineEndings restores CRLF on an all-CRLF file rewritten with LF', () => {
  const original = crlf('int a;' + LF + 'int b;' + LF)
  const rewritten = 'int a;' + LF + 'int b;' + LF
  equal(repairLineEndings(rewritten, original, CRLF_PLAN), original)
})

test('repairLineEndings keeps each original terminator in a mixed file', () => {
  const original = 'int a;' + CRLF + 'int b;' + LF + 'int c;' + CRLF
  const rewritten = 'int a;' + LF + 'int b;' + LF + 'int c;' + LF
  const repaired = repairLineEndings(rewritten, original, CRLF_PLAN)
  equal(repaired, original)
  // Churn is measured against what the file already had, not against the
  // LF-only text the write produced.
  equal(countRewrittenEndings(original, repaired), 0)
})

test('repairLineEndings gives only genuinely new lines the target terminator', () => {
  const original = 'int a;' + LF + 'int b;' + LF
  const rewritten = 'int a;' + LF + 'int added;' + LF + 'int b;' + LF
  equal(repairLineEndings(rewritten, original, CRLF_PLAN), 'int a;' + LF + 'int added;' + CRLF + 'int b;' + LF)
})

test('repairLineEndings converts a changed line and preserves its neighbours', () => {
  const original = 'int a;' + LF + 'int b;' + LF + 'int c;' + LF
  const rewritten = 'int a;' + LF + 'long b;' + LF + 'int c;' + LF
  equal(repairLineEndings(rewritten, original, CRLF_PLAN), 'int a;' + LF + 'long b;' + CRLF + 'int c;' + LF)
})

test('repairLineEndings converts a bare CR even on an otherwise unchanged line', () => {
  const original = 'int a;' + CR + 'int b;' + LF
  const rewritten = 'int a;' + LF + 'int b;' + LF
  equal(repairLineEndings(rewritten, original, CRLF_PLAN), 'int a;' + CRLF + 'int b;' + LF)
})

test('repairLineEndings keeps a bare CR only when fixLoneCr is off', () => {
  const plan: EolPlan = { target: '\r\n', preserveUntouched: true, fixLoneCr: false }
  const original = 'int a;' + CR + 'int b;' + LF
  equal(repairLineEndings(original, original, plan), original)
})

test('repairLineEndings never invents a trailing terminator', () => {
  const original = 'int a;' + LF + 'int b;'
  const rewritten = 'int a;' + LF + 'int b;'
  equal(repairLineEndings(rewritten, original, CRLF_PLAN), original)
  equal(repairLineEndings('int only;', null, CRLF_PLAN), 'int only;')
})

test('repairLineEndings applies the target to a brand new file', () => {
  equal(repairLineEndings('a' + LF + 'b' + LF, null, CRLF_PLAN), crlf('a' + LF + 'b' + LF))
})

test('repairLineEndings ignores preservation when preserveUntouched is off', () => {
  const plan: EolPlan = { target: '\r\n', preserveUntouched: false, fixLoneCr: true }
  const original = 'int a;' + LF + 'int b;' + LF
  equal(repairLineEndings(original, original, plan), crlf(original))
})

test('repairLineEndings honours an LF target for new lines', () => {
  const plan: EolPlan = { target: '\n', preserveUntouched: true, fixLoneCr: true }
  equal(repairLineEndings('a' + CRLF + 'b' + CRLF, null, plan), 'a' + LF + 'b' + LF)
})

test('countRewrittenEndings measures line-ending churn only', () => {
  equal(countRewrittenEndings('a' + LF, 'a' + CRLF), 1)
  equal(countRewrittenEndings('a' + LF, 'a' + LF), 0)
  equal(countRewrittenEndings('a' + LF + 'b' + LF, 'a' + LF), 1)
})

test('decideBom implements each policy', () => {
  ok(decideBom('non-ascii', '// \u4e2d\u6587', false))
  equal(decideBom('non-ascii', 'int x;', false), false)
  // additive: an existing BOM survives even when the text is pure ASCII
  ok(decideBom('non-ascii', 'int x;', true))
  ok(decideBom('always', 'int x;', false))
  equal(decideBom('never', '// \u4e2d\u6587', true), false)
  ok(decideBom('preserve', 'int x;', true))
  equal(decideBom('preserve', '// \u4e2d\u6587', false), false)
})

test('a repaired file is byte-stable on a second pass', () => {
  const original = 'int a;' + CRLF + 'int b;' + LF
  const first = repairLineEndings(original, original, CRLF_PLAN)
  equal(first, original)
  notEqual(first, '')
})
