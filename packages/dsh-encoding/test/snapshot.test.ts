import test from 'node:test'
import { deepEqual, equal } from 'node:assert/strict'

import { SnapshotStore } from '../src/snapshot.js'

test('take is destructive so a snapshot serves exactly one write', () => {
  const store = new SnapshotStore(8, 1000)
  store.put('a', Uint8Array.from([1, 2]), 0)
  deepEqual(Array.from(store.take('a', 0)!.bytes!), [1, 2])
  equal(store.take('a', 0), undefined)
})

test('a null snapshot records a create', () => {
  const store = new SnapshotStore(8, 1000)
  store.put('a', null, 0)
  equal(store.take('a', 0)!.bytes, null)
})

test('expired snapshots are dropped', () => {
  const store = new SnapshotStore(8, 100)
  store.put('a', Uint8Array.from([1]), 0)
  equal(store.take('a', 101), undefined)
  equal(store.size, 0)
})

test('the entry bound evicts oldest-first', () => {
  const store = new SnapshotStore(2, 0)
  store.put('a', null, 0)
  store.put('b', null, 0)
  store.put('c', null, 0)
  equal(store.size, 2)
  equal(store.take('a', 0), undefined)
  equal(store.take('b', 0) !== undefined, true)
})

test('re-putting a key refreshes its recency', () => {
  const store = new SnapshotStore(2, 0)
  store.put('a', null, 0)
  store.put('b', null, 0)
  store.put('a', null, 0)
  store.put('c', null, 0)
  equal(store.take('a', 0) !== undefined, true)
  equal(store.take('b', 0), undefined)
})
