import test from 'node:test'
import { deepEqual, equal, ok } from 'node:assert/strict'

import { CMAKE_GLOBS, DEFAULT_INCLUDE, isSelected, resolveConfig, resolveFilePolicy } from '../src/config.js'

test('resolveConfig fills every field', () => {
  const config = resolveConfig()
  equal(config.enabled, true)
  deepEqual(config.tools, ['write', 'edit'])
  equal(config.bom, 'non-ascii')
  equal(config.eol, 'crlf')
  equal(config.preserveUntouchedEol, true)
  equal(config.fixLoneCr, true)
  equal(config.log, 'summary')
  equal(config.dryRun, false)
  equal(config.maxSnapshotEntries, 64)
  ok(config.includeSet.size > 0)
})

test('resolveConfig honours the row config', () => {
  const config = resolveConfig({ bom: 'always', eol: 'lf', log: 'off', tools: ['write'], enabled: false })
  equal(config.bom, 'always')
  equal(config.eol, 'lf')
  equal(config.log, 'off')
  equal(config.enabled, false)
  deepEqual(config.tools, ['write'])
})

test('resolveConfig falls back instead of throwing on bad values', () => {
  const config = resolveConfig({ bom: 'bogus', eol: 7, log: 'loud', tools: [], maxSnapshotEntries: -3 })
  equal(config.bom, 'non-ascii')
  equal(config.eol, 'crlf')
  equal(config.log, 'summary')
  deepEqual(config.tools, ['write', 'edit'])
  equal(config.maxSnapshotEntries, 64)
})

test('the default include set covers C/C++ and CMake, not other languages', () => {
  const config = resolveConfig()
  for (const file of ['C:/p/a.cpp', 'C:/p/a.h', 'C:/p/CMakeLists.txt', 'C:/p/x.cmake', 'C:/p/y.cmake.in']) {
    ok(isSelected(config, file), file)
  }
  for (const file of ['C:/p/a.ts', 'C:/p/a.lua', 'C:/p/README.md', 'C:/p/CMakePresets.json']) {
    equal(isSelected(config, file), false)
  }
})

test('the default exclude set wins over include', () => {
  const config = resolveConfig()
  equal(isSelected(config, 'C:/p/node_modules/x/a.cpp'), false)
  equal(isSelected(config, 'C:/p/.git/a.cpp'), false)
  equal(isSelected(config, 'C:/p/CMakeFiles/a.cpp'), false)
  equal(isSelected(config, 'C:/p/src/a.cpp'), true)
})

test('DEFAULT_INCLUDE is a frozen non-empty list', () => {
  ok(DEFAULT_INCLUDE.length >= 3)
  ok(Object.isFrozen(DEFAULT_INCLUDE))
})

test('the CMake family is exempt from the C/C++ BOM policy by default', () => {
  const config = resolveConfig()
  for (const file of ['C:/p/CMakeLists.txt', 'C:/p/x.cmake', 'C:/p/y.cmake.in']) {
    equal(resolveFilePolicy(config, file).bom, 'preserve', file)
    // ...but the EOL policy is still the global one.
    equal(resolveFilePolicy(config, file).eol, 'crlf', file)
  }
  equal(resolveFilePolicy(config, 'C:/p/a.cpp').bom, 'non-ascii')
  equal(resolveFilePolicy(config, 'C:/p/a.h').bom, 'non-ascii')
})

test('a row override is consulted before the built-in CMake rule', () => {
  const config = resolveConfig({ overrides: [{ match: CMAKE_GLOBS, bom: 'never' }] })
  equal(resolveFilePolicy(config, 'C:/p/CMakeLists.txt').bom, 'never')
  equal(resolveFilePolicy(config, 'C:/p/a.cpp').bom, 'non-ascii')
})

test('an unrelated row override does not drop the built-in CMake rule', () => {
  const config = resolveConfig({ overrides: [{ match: ['**/vendor/**'], eol: 'lf' }] })
  equal(resolveFilePolicy(config, 'C:/p/vendor/a.cpp').eol, 'lf')
  equal(resolveFilePolicy(config, 'C:/p/vendor/a.cpp').bom, 'non-ascii')
  equal(resolveFilePolicy(config, 'C:/p/CMakeLists.txt').bom, 'preserve')
  equal(resolveFilePolicy(config, 'C:/p/src/a.cpp').eol, 'crlf')
})

test('an override can restore the global BOM policy for CMake', () => {
  const config = resolveConfig({ overrides: [{ match: CMAKE_GLOBS, bom: 'non-ascii' }] })
  equal(resolveFilePolicy(config, 'C:/p/CMakeLists.txt').bom, 'non-ascii')
})

test('overrides with no usable field are discarded', () => {
  const config = resolveConfig({ overrides: [{ match: [] }, { match: ['**/x/**'] }, { bom: 'never' }] })
  equal(config.overrides.length, 1)
})
