import test from 'node:test'
import { deepEqual, equal, ok } from 'node:assert/strict'

import { LSP_TOOLS, outputSchemaFor } from '../src/tools.js'
import type { LspResultKind, LspToolSpec } from '../src/tools.js'
import type { QueryOperation } from '../src/types.js'

type Json = Record<string, any>

/** The nine queries the plugin has always supported, none added, none lost. */
const QUERY_OPERATIONS: QueryOperation[] = [
  'explore',
  'goToDefinition',
  'findReferences',
  'goToImplementation',
  'typeDefinition',
  'hover',
  'workspaceSymbol',
  'documentSymbol',
  'diagnostics',
]

/** Query each tool routes to; `lsp_symbols` routes to two, chosen by scope. */
function routedOperations(spec: LspToolSpec): QueryOperation[] {
  if (spec.operation === 'status') return []
  if (spec.operation === 'symbols') return ['documentSymbol', 'workspaceSymbol']
  return [spec.operation]
}

test('every tool name is unique and prefixed with lsp_', () => {
  const names = LSP_TOOLS.map(s => s.name)
  equal(new Set(names).size, names.length)
  for (const name of names) ok(name.startsWith('lsp_'), name + ' is not prefixed with lsp_')
})

test('the catalog covers every query operation (no capability lost in the split)', () => {
  const routed = LSP_TOOLS.flatMap(routedOperations)
  deepEqual([...routed].sort(), [...QUERY_OPERATIONS].sort())
})

test('no tool exposes the old operation enum as a parameter', () => {
  for (const spec of LSP_TOOLS) {
    const properties = (spec.parameters as Json).properties as Json
    ok(properties.operation === undefined, spec.name + ' still exposes an operation parameter')
  }
})

test('every tool declares a well-formed object parameter schema', () => {
  for (const spec of LSP_TOOLS) {
    const params = spec.parameters as Json
    equal(params.type, 'object', spec.name)
    equal(params.additionalProperties, false, spec.name + ' must reject unknown arguments')
    ok(params.properties !== null && typeof params.properties === 'object', spec.name + ' has no properties')
    for (const [name, property] of Object.entries(params.properties as Json)) {
      ok(typeof (property as Json).description === 'string' && (property as Json).description !== '',
        spec.name + '.' + name + ' needs a description (the model reads it as part of the prompt)')
    }
    const required = (params.required as string[] | undefined) ?? []
    for (const name of required) {
      ok((params.properties as Json)[name] !== undefined, spec.name + ' requires unknown property ' + name)
    }
  }
})

test('every tool has a non-empty one-line description', () => {
  for (const spec of LSP_TOOLS) {
    ok(spec.description.trim() !== '', spec.name + ' has no description')
    ok(!spec.description.includes('\n'), spec.name + ' description must be one line')
  }
})

test('position tools require filePath, line and character', () => {
  const positionTools = LSP_TOOLS.filter(spec =>
    spec.resultKind === 'locations' || spec.resultKind === 'explore' || spec.resultKind === 'hover')
  ok(positionTools.length > 0)
  for (const spec of positionTools) {
    const params = spec.parameters as Json
    deepEqual(params.required, ['filePath', 'line', 'character'], spec.name)
  }
})

test('lsp_symbols selects document or workspace scope', () => {
  const spec = LSP_TOOLS.find(s => s.name === 'lsp_symbols')
  ok(spec !== undefined)
  const params = spec.parameters as Json
  deepEqual(params.required, ['filePath', 'scope'])
  deepEqual((params.properties.scope as Json).enum, ['document', 'workspace'])
  ok((params.properties.query as Json).description.includes('scope=workspace'))
})

test('lsp_diagnostics filters by minimum severity but only requires filePath', () => {
  const spec = LSP_TOOLS.find(s => s.name === 'lsp_diagnostics')
  ok(spec !== undefined)
  const params = spec.parameters as Json
  deepEqual(params.required, ['filePath'])
  deepEqual((params.properties.severity as Json).enum, ['error', 'warning', 'information', 'hint', 'all'])
})

test('lsp_status takes no arguments', () => {
  const spec = LSP_TOOLS.find(s => s.name === 'lsp_status')
  ok(spec !== undefined)
  const params = spec.parameters as Json
  deepEqual(params.properties, {})
})

test('each tool advertises one result kind', () => {
  const expected: Record<string, LspResultKind> = {
    lsp_explore: 'explore',
    lsp_definition: 'locations',
    lsp_type_definition: 'locations',
    lsp_implementation: 'locations',
    lsp_references: 'locations',
    lsp_hover: 'hover',
    lsp_symbols: 'symbols',
    lsp_diagnostics: 'diagnostics',
    lsp_status: 'status',
  }
  deepEqual(
    Object.fromEntries(LSP_TOOLS.map(spec => [spec.name, spec.resultKind])),
    expected,
  )
})

test('outputSchemaFor pins ok/kind as consts and always offers the error branch', () => {
  for (const kind of new Set(LSP_TOOLS.map(s => s.resultKind))) {
    const schema = outputSchemaFor(kind) as Json
    const branches = schema.oneOf as Json[]
    equal(branches.length, 2)
    const success = branches[0] as Json
    const failure = branches[1] as Json
    equal(success.properties.kind.const, kind)
    equal(success.properties.ok.const, true)
    deepEqual(success.required, ['ok', 'kind', 'text', 'result'])
    equal(failure.properties.ok.const, false)
    equal(failure.properties.kind.const, 'error')
  }
})

test('the status result schema carries the server list', () => {
  const schema = outputSchemaFor('status') as Json
  const result = ((schema.oneOf as Json[])[0] as Json).properties.result as Json
  deepEqual(result.required, ['kind', 'servers'])
  equal(result.properties.servers.type, 'array')
  deepEqual(
    (result.properties.servers.items as Json).required,
    ['id', 'languageId', 'extensions', 'state', 'root', 'pid', 'error'],
  )
})
