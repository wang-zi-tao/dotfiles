import test from 'node:test'
import { equal } from 'node:assert/strict'

import { normalizePathKey } from '../src/inbox.js'
import { MAX_TOUCHED_AGENTS, MAX_TOUCHED_FILES_PER_AGENT, TouchedFiles } from '../src/touched.js'

const PATH_A = 'C:\\proj\\src\\a.cpp'
const PATH_B = 'C:\\proj\\src\\b.cpp'
const KEY_A = normalizePathKey(PATH_A)
const KEY_B = normalizePathKey(PATH_B)

test('a touched file is scoped to the session that touched it', () => {
  const touched = new TouchedFiles()
  touched.touch('agent-1', PATH_A)

  equal(touched.includes('agent-1', KEY_A), true)
  equal(touched.includes('agent-1', KEY_B), false)
  equal(touched.includes('agent-2', KEY_A), false)
})

test('matching is case-folded like the broadcaster baseline', () => {
  const touched = new TouchedFiles()
  touched.touch('agent-1', 'C:\\Proj\\Src\\A.cpp')

  equal(touched.includes('agent-1', KEY_A), true)
  equal(touched.pathOf('agent-1', KEY_A), 'C:\\Proj\\Src\\A.cpp')
})

test('touching the same file twice is idempotent', () => {
  const touched = new TouchedFiles()
  touched.touch('agent-1', PATH_A)
  touched.touch('agent-1', PATH_A)

  equal(touched.agentCount, 1)
  equal(touched.includes('agent-1', KEY_A), true)
})

test('an empty session id records nothing', () => {
  const touched = new TouchedFiles()
  touched.touch('', PATH_A)

  equal(touched.agentCount, 0)
  equal(touched.includes('', KEY_A), false)
})

test('each session is bounded by a file cap, evicting its oldest entry', () => {
  const touched = new TouchedFiles()
  touched.touch('agent-1', PATH_A)
  for (let i = 0; i < MAX_TOUCHED_FILES_PER_AGENT; i++) {
    touched.touch('agent-1', 'C:\\proj\\gen\\f' + i + '.cpp')
  }

  equal(touched.includes('agent-1', KEY_A), false)
  equal(touched.includes('agent-1', normalizePathKey('C:\\proj\\gen\\f' + (MAX_TOUCHED_FILES_PER_AGENT - 1) + '.cpp')), true)
})

test('the number of tracked sessions is bounded', () => {
  const touched = new TouchedFiles()
  for (let i = 0; i < MAX_TOUCHED_AGENTS + 4; i++) touched.touch('agent-' + i, PATH_A)

  equal(touched.agentCount, MAX_TOUCHED_AGENTS)
  equal(touched.includes('agent-0', KEY_A), false)
  equal(touched.includes('agent-' + (MAX_TOUCHED_AGENTS + 3), KEY_A), true)
})

test('dispose forgets every session', () => {
  const touched = new TouchedFiles()
  touched.touch('agent-1', PATH_A)
  touched.dispose()

  equal(touched.agentCount, 0)
  equal(touched.includes('agent-1', KEY_A), false)
})
