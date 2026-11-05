/**
 * Model-facing tool catalog for dsh-lsp.
 *
 * One tool per navigation intent instead of a single `lsp` tool routed by an
 * `operation` enum: the model picks a tool by what it wants to know, and each
 * tool declares only the parameters that intent needs. The prompt shape
 * follows oh-my-opencode's `lsp-core` tool definitions (short name, one-line
 * description, small typed parameter object).
 *
 * Parameters and outputs are raw JSON Schema nodes. The harness consumes
 * `parameters` as `Record<string, unknown>` and deep-freezes the registered
 * copies, so every schema here is treated as immutable data.
 */

import type { JsonSchemaNode } from '@deepseek-ai/dsh-tools'
import type { QueryOperation } from './types.js'

/**
 * Which query a tool routes to. `symbols` picks `documentSymbol` or
 * `workspaceSymbol` from its `scope` argument; `status` lists servers
 * instead of querying one.
 */
export type LspToolOperation = QueryOperation | 'symbols' | 'status'

/** Output payload family; each maps to exactly one success-schema branch. */
export type LspResultKind = 'locations' | 'hover' | 'symbols' | 'diagnostics' | 'explore' | 'status'

/** One model-facing tool: name, description, parameters, and routed query. */
export interface LspToolSpec {
  /** Model-facing tool name, always `lsp_*`. */
  name: string
  operation: LspToolOperation
  /** One-line description: what the tool answers and when to prefer it. */
  description: string
  parameters: Record<string, unknown>
  /** Which result shape `outputSchemaFor` should advertise. */
  resultKind: LspResultKind
}

/**
 * Parameters for every position-based query. `line`/`character` are required
 * because every one of those operations only means something at a cursor;
 * leaving them optional would push a runtime error into the model's turn.
 */
function positionParameters(): Record<string, unknown> {
  return {
    type: 'object',
    additionalProperties: false,
    properties: {
      filePath: {
        type: 'string',
        description: 'Absolute or workspace-relative source file path; its extension selects the language server.',
      },
      line: { type: 'integer', description: '1-based line of the symbol.' },
      character: { type: 'integer', description: '1-based UTF-16 column of the symbol.' },
    },
    required: ['filePath', 'line', 'character'],
  }
}

/**
 * The catalog. Order is the order the tools are registered in; keep the
 * aggregated `explore` first because it is the default entry point.
 */
export const LSP_TOOLS: readonly LspToolSpec[] = [
  {
    name: 'lsp_explore',
    operation: 'explore',
    resultKind: 'explore',
    description:
      'Aggregated symbol lookup: definition, type definition, hover documentation, implementations and references for the symbol at a position. Use it first when several answers are needed; use a narrower lsp_* tool when only one is.',
    parameters: positionParameters(),
  },
  {
    name: 'lsp_definition',
    operation: 'goToDefinition',
    resultKind: 'locations',
    description: 'Find where the symbol at a position is defined.',
    parameters: positionParameters(),
  },
  {
    name: 'lsp_type_definition',
    operation: 'typeDefinition',
    resultKind: 'locations',
    description: 'Find where the type of the symbol at a position is defined.',
    parameters: positionParameters(),
  },
  {
    name: 'lsp_implementation',
    operation: 'goToImplementation',
    resultKind: 'locations',
    description: 'Find the implementations of the interface or virtual method at a position.',
    parameters: positionParameters(),
  },
  {
    name: 'lsp_references',
    operation: 'findReferences',
    resultKind: 'locations',
    description: 'Find every reference to the symbol at a position across the workspace; use it before changing a symbol.',
    parameters: positionParameters(),
  },
  {
    name: 'lsp_hover',
    operation: 'hover',
    resultKind: 'hover',
    description: 'Get the signature, type and documentation of the symbol at a position.',
    parameters: positionParameters(),
  },
  {
    name: 'lsp_symbols',
    operation: 'symbols',
    resultKind: 'symbols',
    description:
      'List the symbols of a file (scope=document) or search workspace symbols by partial/fuzzy name (scope=workspace). Prefer it over grep when the name is known but the position is not.',
    parameters: {
      type: 'object',
      additionalProperties: false,
      properties: {
        filePath: {
          type: 'string',
          description:
            'Source file path; its extension selects the language server (any file of that language works for a workspace search).',
        },
        scope: {
          type: 'string',
          enum: ['document', 'workspace'],
          description: "document for this file's symbols, workspace for a project-wide search.",
        },
        query: {
          type: 'string',
          description: 'Partial or fuzzy symbol name; required when scope=workspace.',
        },
      },
      required: ['filePath', 'scope'],
    },
  },
  {
    name: 'lsp_diagnostics',
    operation: 'diagnostics',
    resultKind: 'diagnostics',
    description: 'Get the errors, warnings and hints a language server reports for one source file.',
    parameters: {
      type: 'object',
      additionalProperties: false,
      properties: {
        filePath: {
          type: 'string',
          description: 'Absolute or workspace-relative source file path; its extension selects the language server.',
        },
        severity: {
          type: 'string',
          enum: ['error', 'warning', 'information', 'hint', 'all'],
          description: 'Minimum severity to report (error = errors only); defaults to all.',
        },
      },
      required: ['filePath'],
    },
  },
  {
    name: 'lsp_status',
    operation: 'status',
    resultKind: 'status',
    description:
      'List configured and active language servers, their state and background-index progress, without starting a new server.',
    parameters: { type: 'object', additionalProperties: false, properties: {} },
  },
]

/** One flattened location in the structured projection. */
const LOCATION_SCHEMA: JsonSchemaNode = {
  type: 'object',
  additionalProperties: false,
  properties: {
    path: { type: 'string', description: 'Workspace-relative path when inside the root, otherwise the absolute path.' },
    line: { type: 'integer', description: '1-based line.' },
    character: { type: 'integer', description: '1-based UTF-16 character.' },
    preview: { type: 'string', description: 'Trimmed content of the line the range starts on; omitted when empty.' },
  },
  required: ['path', 'line', 'character'],
}

/** A null is meaningful for hover (no information available). */
const NULLABLE_STRING_SCHEMA: JsonSchemaNode = {
  oneOf: [{ type: 'string' }, { type: 'null' }],
}

/** A null is meaningful for status (a stopped server has no pid). */
const NULLABLE_INTEGER_SCHEMA: JsonSchemaNode = {
  oneOf: [{ type: 'integer' }, { type: 'null' }],
}

const SYMBOL_SCHEMA: JsonSchemaNode = {
  type: 'object',
  additionalProperties: false,
  properties: {
    name: { type: 'string' },
    kind: { type: 'string', description: 'Human-readable symbol kind (function, class, struct, ...).' },
    containerName: { type: 'string' },
    location: LOCATION_SCHEMA,
  },
  required: ['name', 'kind', 'location'],
}

const DIAGNOSTIC_SCHEMA: JsonSchemaNode = {
  type: 'object',
  additionalProperties: false,
  properties: {
    severity: { type: 'string', enum: ['hint', 'information', 'warning', 'error'] },
    message: { type: 'string' },
    range: {
      type: 'object',
      additionalProperties: false,
      properties: {
        startLine: { type: 'integer' },
        startCharacter: { type: 'integer' },
        endLine: { type: 'integer' },
        endCharacter: { type: 'integer' },
      },
      required: ['startLine', 'startCharacter', 'endLine', 'endCharacter'],
    },
    code: { type: 'string' },
    source: { type: 'string' },
  },
  required: ['severity', 'message', 'range'],
}

/** One row of `lsp_status`: a configured server and its current state. */
const SERVER_STATUS_SCHEMA: JsonSchemaNode = {
  type: 'object',
  additionalProperties: false,
  properties: {
    id: { type: 'string' },
    languageId: { type: 'string' },
    extensions: { type: 'array', items: { type: 'string' } },
    state: { type: 'string', enum: ['running', 'stopped', 'starting', 'failed'] },
    root: NULLABLE_STRING_SCHEMA,
    pid: NULLABLE_INTEGER_SCHEMA,
    error: NULLABLE_STRING_SCHEMA,
    index: {
      type: 'string',
      description: 'Rendered background-index progress, when the server reported any.',
    },
  },
  required: ['id', 'languageId', 'extensions', 'state', 'root', 'pid', 'error'],
}

/** The per-tool payload carried in `result`, keyed by result kind. */
const RESULT_SCHEMA_BY_KIND: Record<LspResultKind, JsonSchemaNode> = {
  locations: {
    type: 'object',
    additionalProperties: false,
    properties: {
      kind: { type: 'string', const: 'locations' },
      locations: { type: 'array', items: LOCATION_SCHEMA },
      truncated: { type: 'boolean' },
    },
    required: ['kind', 'locations', 'truncated'],
  },
  hover: {
    type: 'object',
    additionalProperties: false,
    properties: {
      kind: { type: 'string', const: 'hover' },
      hover: NULLABLE_STRING_SCHEMA,
    },
    required: ['kind', 'hover'],
  },
  symbols: {
    type: 'object',
    additionalProperties: false,
    properties: {
      kind: { type: 'string', const: 'symbols' },
      symbols: { type: 'array', items: SYMBOL_SCHEMA },
      truncated: { type: 'boolean' },
      fallback: {
        type: 'object',
        additionalProperties: false,
        description: 'Present when the index answered nothing and the candidate fallback ran.',
        properties: {
          used: { type: 'boolean' },
          strategy: { type: 'string', enum: ['name', 'content', 'none'] },
          candidates: { type: 'integer', description: 'Candidate files the search returned.' },
          warmed: { type: 'integer', description: 'Candidates parsed into the dynamic index.' },
          matched: { type: 'integer', description: 'Candidates that matched before capping.' },
          truncated: { type: 'boolean' },
        },
        required: ['used', 'strategy', 'candidates', 'warmed', 'matched', 'truncated'],
      },
      indexing: {
        type: 'string',
        description: 'Background index state when the answer was empty because the index was still building.',
      },
    },
    required: ['kind', 'symbols', 'truncated'],
  },
  diagnostics: {
    type: 'object',
    additionalProperties: false,
    properties: {
      kind: { type: 'string', const: 'diagnostics' },
      diagnostics: { type: 'array', items: DIAGNOSTIC_SCHEMA },
      note: {
        type: 'string',
        description: 'Present when the answer is incomplete: a pull timed out or no push arrived in time.',
      },
    },
    required: ['kind', 'diagnostics'],
  },
  explore: {
    type: 'object',
    additionalProperties: false,
    properties: {
      kind: { type: 'string', const: 'explore' },
      symbolName: { type: 'string' },
      hover: NULLABLE_STRING_SCHEMA,
      definition: { type: 'array', items: LOCATION_SCHEMA },
      typeDefinition: { type: 'array', items: LOCATION_SCHEMA },
      implementation: { type: 'array', items: LOCATION_SCHEMA },
      references: { type: 'array', items: LOCATION_SCHEMA },
      truncated: { type: 'boolean' },
    },
    required: ['kind', 'symbolName', 'hover', 'definition', 'typeDefinition', 'implementation', 'references', 'truncated'],
  },
  status: {
    type: 'object',
    additionalProperties: false,
    properties: {
      kind: { type: 'string', const: 'status' },
      servers: { type: 'array', items: SERVER_STATUS_SCHEMA },
    },
    required: ['kind', 'servers'],
  },
}

/** The shared failure envelope: a tool never throws out of `execute`. */
const ERROR_BRANCH: JsonSchemaNode = {
  type: 'object',
  additionalProperties: false,
  properties: {
    ok: { type: 'boolean', const: false },
    kind: { type: 'string', const: 'error' },
    text: { type: 'string' },
  },
  required: ['ok', 'kind', 'text'],
}

/**
 * Canonical output schema for one tool: a human `text` rendering plus the
 * always-present structured `result` projection, or an error envelope. The
 * branches are disjoint (`ok` is a const), and `kind` is a const too, so a
 * client can discriminate the payload without inspecting its shape.
 */
export function outputSchemaFor(kind: LspResultKind): JsonSchemaNode {
  return {
    oneOf: [
      {
        type: 'object',
        additionalProperties: false,
        properties: {
          ok: { type: 'boolean', const: true },
          kind: { type: 'string', const: kind },
          text: { type: 'string' },
          result: RESULT_SCHEMA_BY_KIND[kind],
        },
        required: ['ok', 'kind', 'text', 'result'],
      },
      ERROR_BRANCH,
    ],
  }
}
