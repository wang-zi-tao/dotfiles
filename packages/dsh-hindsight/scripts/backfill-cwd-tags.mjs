#!/usr/bin/env node
/**
 * Backfill the cwd visibility tag onto a Hindsight bank.
 *
 * dsh-hindsight writes 'cwd:<cwd>' on every retained memory and declares the
 * same tag on the per-project mental model, so the model reads ONLY the
 * memories of that project. The server defaults a tagged mental model to
 * 'tags_match: all_strict' - a memory must carry every model tag and untagged
 * memories are excluded - so models re-scoped by the plugin would refresh to
 * EMPTY content until the memories retained before this change carry the tag.
 *
 * This script closes that gap through the public HTTP API:
 *
 *   1. models    - PATCH each '项目 <cwd>' mental model with 'cwd:<cwd>'.
 *   2. documents - PATCH each document whose retain metadata records a cwd,
 *                  adding 'cwd:<cwd>' to its existing tags. The endpoint
 *                  propagates the tags to the document's memory units and
 *                  observations and schedules re-consolidation.
 *
 * Both steps are idempotent: anything already carrying the tag is skipped.
 * Nothing is written unless --apply is passed; the default is a dry run.
 *
 * Usage:
 *   node scripts/backfill-cwd-tags.mjs --bank deepseek-harness --api http://localhost:8888
 *   node scripts/backfill-cwd-tags.mjs --bank deepseek-harness --api http://localhost:8888 --apply
 *
 * Flags:
 *   --api <url>        Hindsight base URL   (env HINDSIGHT_API_URL, default http://localhost:8888)
 *   --api-key <key>    Bearer token         (env HINDSIGHT_API_KEY)
 *   --bank <id>        Bank id              (env HINDSIGHT_BANK_ID)
 *   --tag-prefix <p>   Tag prefix           (default 'cwd:')
 *   --apply            Actually write       (default: dry run)
 *   --models           Only step 1
 *   --documents        Only step 2
 *   --limit <n>        Process at most n items per step
 *   --help
 *
 * WARNING: patching a document invalidates its observations and triggers
 * re-consolidation (LLM work) per document. Use --limit for a first batch.
 */

import process from 'node:process'

const PROJECT_PREFIX = '项目 '

function parseArgs(argv) {
  const options = {
    api: process.env.HINDSIGHT_API_URL ?? 'http://localhost:8888',
    apiKey: process.env.HINDSIGHT_API_KEY ?? '',
    bank: process.env.HINDSIGHT_BANK_ID ?? '',
    tagPrefix: 'cwd:',
    apply: false,
    models: true,
    documents: true,
    limit: Infinity,
  }
  let onlyStep = false
  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index]
    const value = () => {
      const next = argv[index + 1]
      if (next === undefined || next.startsWith('--')) throw new Error(arg + ' needs a value')
      index += 1
      return next
    }
    switch (arg) {
      case '--api': options.api = value(); break
      case '--api-key': options.apiKey = value(); break
      case '--bank': options.bank = value(); break
      case '--tag-prefix': options.tagPrefix = value(); break
      case '--limit': options.limit = Number(value()); break
      case '--apply': options.apply = true; break
      case '--models':
        if (!onlyStep) { options.documents = false; onlyStep = true }
        options.models = true
        break
      case '--documents':
        if (!onlyStep) { options.models = false; onlyStep = true }
        options.documents = true
        break
      case '--help': options.help = true; break
      default: throw new Error('unknown flag ' + arg)
    }
  }
  if (options.tagPrefix === '') throw new Error('--tag-prefix must not be empty')
  if (!Number.isFinite(options.limit) || options.limit <= 0) options.limit = Infinity
  return options
}

const HELP = [
  'Backfill cwd visibility tags onto a Hindsight bank (dry run by default).',
  '',
  '  node scripts/backfill-cwd-tags.mjs --bank <id> [--api <url>] [--apply] [--models|--documents] [--limit <n>]',
  '',
  'See the file header for the full description and the re-consolidation warning.',
].join('\n')

function makeClient(options) {
  const base = options.api.replace(/[/]+$/, '')
  return async function request(path, init = {}) {
    const headers = {Accept: 'application/json', 'Content-Type': 'application/json'}
    if (options.apiKey) headers.Authorization = 'Bearer ' + options.apiKey
    const response = await fetch(base + path, {...init, headers})
    const text = await response.text()
    let body
    try { body = text ? JSON.parse(text) : {} } catch { body = {raw: text.slice(0, 500)} }
    if (!response.ok) {
      const detail = body && body.detail ? JSON.stringify(body.detail) : text.slice(0, 300)
      throw new Error((init.method ?? 'GET') + ' ' + path + ' -> ' + response.status + ' ' + detail)
    }
    return body
  }
}

const bankPath = (bank, suffix) => '/v1/default/banks/' + encodeURIComponent(bank) + suffix

/** The cwd recorded on a document, from either metadata projection. */
function documentCwd(detail) {
  const candidates = [detail && detail.retain_params && detail.retain_params.metadata, detail && detail.document_metadata]
  for (const meta of candidates) {
    const cwd = meta && meta.cwd
    if (typeof cwd === 'string' && cwd.trim()) return cwd.trim()
  }
  return undefined
}

async function backfillModels(request, options, report) {
  const {items} = await request(bankPath(options.bank, '/mental-models?limit=200'))
  for (const model of items ?? []) {
    if (report.models.processed >= options.limit) break
    const name = String(model.name ?? '')
    const cwd = name.startsWith(PROJECT_PREFIX) ? name.slice(PROJECT_PREFIX.length).trim() : ''
    if (!cwd) { report.models.skippedNoCwd.push(model.id + ' (' + name + ')'); continue }
    const tag = options.tagPrefix + cwd
    const current = Array.isArray(model.tags) ? model.tags : []
    if (current.includes(tag)) { report.models.already.push(model.id); continue }
    report.models.processed += 1
    report.models.pending.push({id: model.id, name, tag, from: current})
    if (options.apply) {
      await request(bankPath(options.bank, '/mental-models/' + encodeURIComponent(model.id)), {
        method: 'PATCH',
        body: JSON.stringify({tags: [...current, tag]}),
      })
      report.models.applied.push(model.id)
    }
  }
}

async function backfillDocuments(request, options, report) {
  const pageSize = 100
  for (let offset = 0; ; offset += pageSize) {
    const page = await request(bankPath(options.bank, '/documents?limit=' + pageSize + '&offset=' + offset))
    const items = page.items ?? []
    if (items.length === 0) break
    for (const item of items) {
      if (report.documents.processed >= options.limit) return
      const detail = await request(bankPath(options.bank, '/documents/' + encodeURIComponent(item.id)))
      const cwd = documentCwd(detail)
      if (!cwd) { report.documents.skippedNoCwd += 1; continue }
      const tag = options.tagPrefix + cwd
      const current = Array.isArray(detail.tags) ? detail.tags : []
      if (current.includes(tag)) { report.documents.already += 1; continue }
      report.documents.processed += 1
      report.documents.pending.push({id: item.id, tag})
      if (options.apply) {
        await request(bankPath(options.bank, '/documents/' + encodeURIComponent(item.id)), {
          method: 'PATCH',
          body: JSON.stringify({tags: [...current, tag]}),
        })
        report.documents.applied += 1
      }
    }
    if (items.length < pageSize) break
  }
}

async function main() {
  const options = parseArgs(process.argv.slice(2))
  if (options.help) { console.log(HELP); return }
  if (!options.bank) {
    console.error('error: --bank is required (or set HINDSIGHT_BANK_ID)')
    process.exitCode = 2
    return
  }

  const request = makeClient(options)
  const version = await request('/version').catch(() => ({}))
  console.log('hindsight ' + (version.version ?? '?') + ' @ ' + options.api + ', bank ' + options.bank)
  console.log(options.apply ? 'mode: APPLY (writing)' : 'mode: DRY RUN (pass --apply to write)')
  console.log('tag prefix: ' + JSON.stringify(options.tagPrefix))

  const report = {
    models: {processed: 0, applied: [], already: [], pending: [], skippedNoCwd: []},
    documents: {processed: 0, applied: 0, already: 0, pending: [], skippedNoCwd: 0},
  }
  if (options.models) await backfillModels(request, options, report)
  if (options.documents) await backfillDocuments(request, options, report)

  console.log('')
  if (options.models) {
    const m = report.models
    console.log('mental models: ' + m.processed + ' to tag, ' + m.already.length + ' already tagged, ' + m.skippedNoCwd.length + ' without a project name')
    for (const entry of m.pending) {
      console.log('  ' + (options.apply ? 'patched' : 'would patch') + ' ' + entry.id + '  ' + entry.name + '  tags ' + JSON.stringify(entry.from) + ' -> ' + JSON.stringify([...entry.from, entry.tag]))
    }
  }
  if (options.documents) {
    const d = report.documents
    console.log('documents: ' + d.processed + ' to tag, ' + d.already + ' already tagged, ' + d.skippedNoCwd + ' without cwd metadata')
    const preview = d.pending.slice(0, 20)
    for (const entry of preview) console.log('  ' + (options.apply ? 'patched' : 'would patch') + ' ' + entry.id + '  +' + entry.tag)
    if (d.pending.length > preview.length) console.log('  ... and ' + (d.pending.length - preview.length) + ' more')
  }
  if (!options.apply && (report.models.processed > 0 || report.documents.processed > 0)) {
    console.log('')
    console.log('Re-run with --apply to write. Patching a document triggers re-consolidation.')
  }
}

main().catch(error => {
  console.error('error: ' + (error instanceof Error ? error.message : String(error)))
  process.exitCode = 1
})
