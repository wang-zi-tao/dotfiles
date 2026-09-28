import test from 'node:test'
import { equal } from 'node:assert/strict'

import { GlobSet, globToRegExpSource } from '../src/glob.js'

const set = (patterns: string[]): GlobSet => new GlobSet(patterns, { caseInsensitive: false })

test('globToRegExpSource anchors and escapes literals', () => {
  equal(globToRegExpSource('a.cpp'), 'a\\.cpp')
  equal(globToRegExpSource('*.cpp'), '[^/]*\\.cpp')
  equal(globToRegExpSource('**/*.cpp'), '(?:[^/]*/)*[^/]*\\.cpp')
  equal(globToRegExpSource('src/**'), 'src/.*')
})

test('a pattern without a separator matches the basename at any depth', () => {
  const rules = set(['CMakeLists.txt'])
  equal(rules.matches('CMakeLists.txt'), true)
  equal(rules.matches('src/deep/CMakeLists.txt'), true)
  equal(rules.matches('CMakeLists.txt.in'), false)
})

test('braces expand to alternation', () => {
  const rules = set(['**/*.{c,h,cpp}'])
  equal(rules.matches('a/b/main.cpp'), true)
  equal(rules.matches('a/b/main.h'), true)
  equal(rules.matches('a/b/main.c'), true)
  equal(rules.matches('a/b/main.hpp'), false)
  equal(rules.matches('main.cpp'), true)
})

test('an empty set matches nothing', () => {
  equal(set([]).matches('a.cpp'), false)
  equal(set([]).size, 0)
})

test('an exclusion-style rule only matches whole segments', () => {
  const rules = set(['**/build/**'])
  equal(rules.matches('a/build/b.cpp'), true)
  equal(rules.matches('a/build'), false)
  equal(rules.matches('a/building/b.cpp'), false)
})

test('backslashes are path separators', () => {
  const rules = set(['**/src/**/*.h'])
  equal(rules.matches('C:\\proj\\src\\a\\b.h'), true)
})

test('case sensitivity follows the option', () => {
  equal(new GlobSet(['*.cpp'], { caseInsensitive: true }).matches('MAIN.CPP'), true)
  equal(new GlobSet(['*.cpp'], { caseInsensitive: false }).matches('MAIN.CPP'), false)
})
