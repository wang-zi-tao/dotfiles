/**
 * Minimal Hindsight HTTP API client.
 *
 * Endpoints mirror `hindsight-client` 0.6.x plus the mental-models surface:
 *   retain:      POST /v1/default/banks/{bank_id}/memories
 *   recall:      POST /v1/default/banks/{bank_id}/memories/recall
 *   reflect:     POST /v1/default/banks/{bank_id}/reflect
 *   operation:   GET  /v1/default/banks/{bank_id}/operations/{operation_id}
 *   version:     GET  /version
 *   mental list: GET  /v1/default/banks/{bank_id}/mental-models
 *   mental get:  GET  /v1/default/banks/{bank_id}/mental-models/{id}
 *   mental make: POST /v1/default/banks/{bank_id}/mental-models
 *   mental edit: PATCH /v1/default/banks/{bank_id}/mental-models/{id}
 *   page tree:   GET  /v1/default/banks/{bank_id}/knowledge-base/tree
 *   page read:   GET  /v1/default/banks/{bank_id}/knowledge-base/pages/{page_id}
 *   folder make: POST /v1/default/banks/{bank_id}/knowledge-base/folders
 *   page make:   POST /v1/default/banks/{bank_id}/knowledge-base/pages
 *
 * `captureInitiative` is a composition of that surface (folder + page +
 * marker), not a server endpoint — see its doc comment.
 */

import {createHash} from 'node:crypto'

export interface RetainItem {
  content: string
  timestamp?: string
  context?: string
  metadata?: Record<string, string>
  document_id?: string
  tags?: string[]
  update_mode?: string
}

export interface RetainOptions {
  bankId: string
  items: RetainItem[]
  retainAsync?: boolean
  documentTags?: string[]
  signal?: AbortSignal
  timeoutMs?: number
}

export interface RecallOptions {
  bankId: string
  query: string
  budget?: string
  maxTokens?: number
  types?: string[]
  tags?: string[]
  tagsMatch?: string
  signal?: AbortSignal
  timeoutMs?: number
}

export interface ReflectOptions {
  bankId: string
  query: string
  budget?: string
  context?: string
  maxTokens?: number
  tags?: string[]
  tagsMatch?: string
  includeFacts?: boolean
  signal?: AbortSignal
  timeoutMs?: number
}

export interface RecallResult {
  id: string
  text: string
  type?: string
  tags?: string[]
}

export interface RecallResponse {
  results?: RecallResult[]
  [key: string]: unknown
}

export interface RetainResponse {
  success?: boolean
  items_count?: number
  operation_id?: string
  operation_ids?: string[]
  [key: string]: unknown
}

export interface ReflectResponse {
  text?: string
  [key: string]: unknown
}

export interface VersionResponse {
  version?: string
  [key: string]: unknown
}

export interface OperationStatusResponse {
  status?: string
  [key: string]: unknown
}

export interface MentalModelTrigger {
  mode?: 'full' | 'delta'
  refresh_after_consolidation?: boolean
  fact_types?: string[]
  exclude_mental_models?: boolean
  exclude_mental_model_ids?: string[] | null
  tags_match?: 'any' | 'all' | 'any_strict' | 'all_strict' | null
  [key: string]: unknown
}

export interface MentalModel {
  id: string
  bank_id?: string
  name: string
  source_query?: string | null
  content?: string | null
  tags?: string[]
  max_tokens?: number | null
  trigger?: MentalModelTrigger | null
  last_refreshed_at?: string | null
  created_at?: string | null
  is_stale?: boolean | null
  [key: string]: unknown
}

export interface MentalModelListResponse {
  items: MentalModel[]
}

export interface CreateMentalModelOptions {
  bankId: string
  id?: string
  name: string
  sourceQuery: string
  tags?: string[]
  maxTokens?: number
  trigger?: Record<string, unknown>
  signal?: AbortSignal
  timeoutMs?: number
}

export interface CreateMentalModelResponse {
  mental_model_id?: string | null
  operation_id: string
}

export interface UpdateMentalModelOptions {
  bankId: string
  id: string
  name?: string
  sourceQuery?: string
  tags?: string[]
  maxTokens?: number
  trigger?: Record<string, unknown>
  signal?: AbortSignal
  timeoutMs?: number
}

export interface KnowledgePageNode {
  id: string
  kind?: 'folder' | 'page'
  name: string
  parent_id?: string | null
  mental_model_id?: string | null
  managed?: boolean
  /** For a page node the server stores the full source query here. */
  description?: string | null
  tags?: string[]
  timestamp?: string
  is_stale?: boolean | null
  children?: KnowledgePageNode[]
}

export interface KnowledgePageTreeResponse {
  roots: KnowledgePageNode[]
}

export interface KnowledgePageSearchHit {
  id: string
  name: string
  mental_model_id?: string | null
  source_query?: string
  snippet?: string
  score?: number
  updated_at?: string
}

export interface KnowledgePageSearchResponse {
  results: KnowledgePageSearchHit[]
}

/** A single knowledge page. The generated body lives in `body` (and `markdown`). */
export interface KnowledgePage {
  id: string
  name: string
  type?: string
  description?: string | null
  tags?: string[]
  timestamp?: string
  body?: string | null
  markdown?: string | null
}

export interface ListKnowledgePagesOptions {
  bankId: string
  signal?: AbortSignal
  timeoutMs?: number
}

export interface SearchKnowledgePagesOptions {
  bankId: string
  query: string
  limit?: number
  signal?: AbortSignal
  timeoutMs?: number
}

/** Request body for POST /knowledge-base/pages (server 1.2.0 takes no client page id). */
export interface CreateKnowledgePageOptions {
  bankId: string
  name: string
  sourceQuery: string
  parentId?: string | null
  tags?: string[]
  maxTokens?: number
  trigger?: Record<string, unknown>
  signal?: AbortSignal
  timeoutMs?: number
}

export interface CreateKnowledgePageResponse {
  page_id?: string
  mental_model_id?: string
  operation_id?: string | null
  /** Tolerated alias used by older/newer builds than the one we tested. */
  id?: string
  [key: string]: unknown
}

export interface EnsureKnowledgeFolderOptions {
  bankId: string
  name: string
  parentId?: string | null
  /**
   * A tree already fetched by the caller. Saves one round trip when the caller
   * needs the tree anyway; omitted means this call fetches it.
   */
  roots?: KnowledgePageNode[]
  signal?: AbortSignal
  timeoutMs?: number
}

export interface CaptureInitiativeOptions {
  bankId: string
  /** Short human name for the work; also the page name. */
  title: string
  /** What is being built or changed, why, and the current state. */
  summary: string
  /**
   * The page this initiative already owns. When set, no page is created: the
   * call only writes another marker, so a changed plan keeps one page.
   * Accepts a page id (`kp-...`) or an exact/unique page name.
   */
  relatesToPageId?: string | null
  /** Folder to file a new page under. Defaults to 'Initiatives'. */
  folderName?: string
  /** Tags written to BOTH the page and its markers — the page's scope filter. */
  tags?: string[]
  /** Token cap for the generated page body. Defaults to 4096. */
  maxTokens?: number
  /**
   * Cron template for the page's refresh. `H` (or `H(lo-hi)`) is expanded
   * client-side, because the server rejects the placeholder form. Defaults to
   * 'H * * * *'.
   */
  refreshCron?: string
  /** Fact types read during refresh. Defaults to world/experience/observation. */
  factTypes?: string[]
  tagsMatch?: 'any' | 'all' | 'any_strict' | 'all_strict'
  /** Extra metadata stamped on the marker (e.g. cwd, session id). */
  metadata?: Record<string, string>
  signal?: AbortSignal
  timeoutMs?: number
}

export interface CaptureInitiativeResult {
  pageId: string
  /** True when this call created the page; false when it only updated it. */
  created: boolean
  folderId?: string
  markerDocumentId: string
  operationId?: string
}

export const INITIATIVE_FOLDER_NAME = 'Initiatives'
export const INITIATIVE_TIER_TAG = 'knowledge:feature-work'
export const INITIATIVE_PAGE_MAX_TOKENS = 4096
export const INITIATIVE_REFRESH_CRON = 'H * * * *'
export const INITIATIVE_FACT_TYPES: readonly string[] = ['world', 'experience', 'observation']

/** Minimal logger surface (structurally compatible with cordis' Logger). */
export interface ClientLogger {
  warn(message: string): void
}

export interface HindsightClientOptions {
  apiUrl: string
  apiKey?: string | null
  timeoutMs?: number
  userAgent?: string
  fetchImpl?: typeof globalThis.fetch
  /** Optional sink for fail-open paths that still need to be observable. */
  logger?: ClientLogger
}

interface RequestOptions {
  method?: 'GET' | 'POST' | 'PATCH'
  body?: unknown
  signal?: AbortSignal
  timeoutMs?: number
}

const USER_AGENT = 'dsh-hindsight/0.1.0'

export class HindsightError extends Error {
  readonly status?: number
  readonly body?: string

  constructor(message: string, options: { status?: number; body?: string; cause?: unknown } = {}) {
    super(message, options.cause === undefined ? undefined : { cause: options.cause })
    this.name = 'HindsightError'
    this.status = options.status
    this.body = options.body
  }
}

function abortError(timeoutMs: number): Error {
  const error = new Error(`Hindsight request timed out after ${timeoutMs} ms`)
  error.name = 'TimeoutError'
  return error
}

export class HindsightClient {
  readonly apiUrl: string
  readonly apiKey: string | null
  readonly timeoutMs: number
  readonly userAgent: string
  private readonly fetchImpl: typeof globalThis.fetch
  private readonly logger?: ClientLogger

  constructor(options: HindsightClientOptions) {
    this.apiUrl = String(options.apiUrl).replace(/\/+$/, '')
    this.apiKey = options.apiKey ?? null
    this.timeoutMs = options.timeoutMs ?? 120000
    this.userAgent = options.userAgent ?? USER_AGENT
    this.fetchImpl = options.fetchImpl ?? globalThis.fetch
    this.logger = options.logger
  }

  async request(path: string, options: RequestOptions = {}): Promise<any> {
    if (!this.fetchImpl) throw new Error('global fetch is not available')
    const { method = 'POST', body, signal } = options
    const timeoutMs = options.timeoutMs ?? this.timeoutMs
    const timeoutSignal = AbortSignal.timeout(timeoutMs)
    const requestSignal = signal ? AbortSignal.any([signal, timeoutSignal]) : timeoutSignal
    const headers: Record<string, string> = {
      Accept: 'application/json',
      'Content-Type': 'application/json',
      'User-Agent': this.userAgent,
    }
    if (this.apiKey) headers.Authorization = `Bearer ${this.apiKey}`

    let response: Response
    try {
      response = await this.fetchImpl(`${this.apiUrl}${path}`, {
        method,
        headers,
        body: body === undefined ? undefined : JSON.stringify(body),
        signal: requestSignal,
      })
    } catch (error) {
      if (error instanceof Error && (error.name === 'TimeoutError' || (timeoutSignal.aborted && !signal?.aborted))) {
        throw abortError(timeoutMs)
      }
      throw new HindsightError(`Hindsight request failed: ${(error as Error)?.message ?? String(error)}`, { cause: error })
    }

    const text = await response.text()
    let data: any = {}
    if (text) {
      try {
        data = JSON.parse(text)
      } catch (error) {
        this.logger?.warn(`hindsight: response for ${path} is not JSON: ${(error as Error)?.message ?? String(error)}`)
        data = { raw: text.slice(0, 4096) }
      }
    }
    if (!response.ok) {
      const detail = typeof data === 'object' && data?.detail ? JSON.stringify(data.detail) : text.slice(0, 1024)
      throw new HindsightError(`Hindsight API returned ${response.status}: ${detail || response.statusText}`, {
        status: response.status,
        body: text.slice(0, 4096),
      })
    }
    return data
  }

  async retain(options: RetainOptions): Promise<RetainResponse> {
    const body: Record<string, unknown> = { items: options.items, async: options.retainAsync ?? false }
    if (options.documentTags?.length) body.document_tags = options.documentTags
    return this.request(`/v1/default/banks/${encodeURIComponent(options.bankId)}/memories`, {
      body,
      signal: options.signal,
      timeoutMs: options.timeoutMs,
    })
  }

  async recall(options: RecallOptions): Promise<RecallResponse> {
    const body: Record<string, unknown> = {
      query: options.query,
      budget: options.budget ?? 'mid',
      max_tokens: options.maxTokens ?? 4096,
    }
    if (options.types?.length) body.types = options.types
    if (options.tags?.length) {
      body.tags = options.tags
      body.tags_match = options.tagsMatch ?? 'any'
    }
    return this.request(`/v1/default/banks/${encodeURIComponent(options.bankId)}/memories/recall`, {
      body,
      signal: options.signal,
      timeoutMs: options.timeoutMs,
    })
  }

  async reflect(options: ReflectOptions): Promise<ReflectResponse> {
    const body: Record<string, unknown> = { query: options.query, budget: options.budget ?? 'mid' }
    if (options.context !== undefined && options.context !== null && options.context !== '') {
      body.context = options.context
    }
    if (options.maxTokens !== undefined && options.maxTokens !== null) body.max_tokens = options.maxTokens
    if (options.tags?.length) {
      body.tags = options.tags
      body.tags_match = options.tagsMatch ?? 'any'
    }
    if (options.includeFacts) body.include = { facts: {} }
    return this.request(`/v1/default/banks/${encodeURIComponent(options.bankId)}/reflect`, {
      body,
      signal: options.signal,
      timeoutMs: options.timeoutMs,
    })
  }

  async operationStatus(bankId: string, operationId: string, options: { signal?: AbortSignal; timeoutMs?: number } = {}): Promise<OperationStatusResponse> {
    return this.request(`/v1/default/banks/${encodeURIComponent(bankId)}/operations/${encodeURIComponent(operationId)}`, {
      method: 'GET',
      signal: options.signal,
      timeoutMs: options.timeoutMs,
    })
  }

  async version(options: { signal?: AbortSignal; timeoutMs?: number } = {}): Promise<VersionResponse> {
    return this.request('/version', { method: 'GET', signal: options.signal, timeoutMs: options.timeoutMs ?? 10000 })
  }

  async listMentalModels(
    bankId: string,
    options: { signal?: AbortSignal; timeoutMs?: number } = {},
  ): Promise<MentalModelListResponse> {
    return this.request(`/v1/default/banks/${encodeURIComponent(bankId)}/mental-models`, {
      method: 'GET',
      signal: options.signal,
      timeoutMs: options.timeoutMs,
    })
  }

  async getMentalModel(
    bankId: string,
    mentalModelId: string,
    options: { signal?: AbortSignal; timeoutMs?: number } = {},
  ): Promise<MentalModel> {
    return this.request(
      `/v1/default/banks/${encodeURIComponent(bankId)}/mental-models/${encodeURIComponent(mentalModelId)}`,
      { method: 'GET', signal: options.signal, timeoutMs: options.timeoutMs },
    )
  }

  async createMentalModel(options: CreateMentalModelOptions): Promise<CreateMentalModelResponse> {
    const body: Record<string, unknown> = {
      name: options.name,
      source_query: options.sourceQuery,
    }
    if (options.id) body.id = options.id
    if (options.tags?.length) body.tags = options.tags
    if (options.maxTokens !== undefined && options.maxTokens !== null) body.max_tokens = options.maxTokens
    if (options.trigger) body.trigger = options.trigger
    return this.request(`/v1/default/banks/${encodeURIComponent(options.bankId)}/mental-models`, {
      body,
      signal: options.signal,
      timeoutMs: options.timeoutMs,
    })
  }

  async updateMentalModel(options: UpdateMentalModelOptions): Promise<MentalModel> {
    const body: Record<string, unknown> = {}
    if (options.name !== undefined) body.name = options.name
    if (options.sourceQuery !== undefined) body.source_query = options.sourceQuery
    if (options.tags !== undefined) body.tags = options.tags
    if (options.maxTokens !== undefined) body.max_tokens = options.maxTokens
    if (options.trigger !== undefined) body.trigger = options.trigger
    return this.request(
      `/v1/default/banks/${encodeURIComponent(options.bankId)}/mental-models/${encodeURIComponent(options.id)}`,
      { method: 'PATCH', body, signal: options.signal, timeoutMs: options.timeoutMs },
    )
  }

  /** Full knowledge-page tree for a bank (folders and pages). */
  async knowledgePageTree(options: ListKnowledgePagesOptions): Promise<KnowledgePageTreeResponse> {
    return this.request(`/v1/default/banks/${encodeURIComponent(options.bankId)}/knowledge-base/tree`, {
      method: 'GET',
      signal: options.signal,
      timeoutMs: options.timeoutMs,
    })
  }

  /** Hybrid (BM25 + vector) search across knowledge pages. */
  async searchKnowledgePages(options: SearchKnowledgePagesOptions): Promise<KnowledgePageSearchResponse> {
    const limit = options.limit ?? 3
    return this.request(
      `/v1/default/banks/${encodeURIComponent(options.bankId)}/knowledge-base/search?q=${encodeURIComponent(options.query)}&limit=${encodeURIComponent(String(limit))}`,
      { method: 'GET', signal: options.signal, timeoutMs: options.timeoutMs },
    )
  }

  /** One knowledge page, including its generated body. */
  async getKnowledgePage(bankId: string, pageId: string, options: { signal?: AbortSignal; timeoutMs?: number } = {}): Promise<KnowledgePage> {
    return this.request(
      `/v1/default/banks/${encodeURIComponent(bankId)}/knowledge-base/pages/${encodeURIComponent(pageId)}`,
      { method: 'GET', signal: options.signal, timeoutMs: options.timeoutMs },
    )
  }

  /** Create a knowledge page (a mental model plus a tree node). */
  async createKnowledgePage(options: CreateKnowledgePageOptions): Promise<CreateKnowledgePageResponse> {
    const body: Record<string, unknown> = {
      name: options.name,
      source_query: options.sourceQuery,
    }
    if (options.parentId) body.parent_id = options.parentId
    if (options.tags?.length) body.tags = options.tags
    if (options.maxTokens !== undefined && options.maxTokens !== null) body.max_tokens = options.maxTokens
    if (options.trigger) body.trigger = options.trigger
    return this.request(`/v1/default/banks/${encodeURIComponent(options.bankId)}/knowledge-base/pages`, {
      body,
      signal: options.signal,
      timeoutMs: options.timeoutMs,
    })
  }

  /**
   * Find (or create) a folder by name and return its node id.
   *
   * Fail-open by design: an initiative page is still useful at the root of the
   * tree, so a failed tree read or folder create returns undefined instead of
   * losing the whole call.
   */
  async ensureKnowledgePageFolder(options: EnsureKnowledgeFolderOptions): Promise<string | undefined> {
    const name = String(options.name ?? '').trim()
    if (!name) return undefined
    let roots = options.roots
    if (!roots) {
      try {
        const tree = await this.knowledgePageTree({
          bankId: options.bankId,
          signal: options.signal,
          timeoutMs: options.timeoutMs,
        })
        roots = Array.isArray(tree?.roots) ? tree.roots : []
      } catch (error) {
        this.logger?.warn(`hindsight: knowledge page tree fetch failed: ${(error as Error)?.message ?? String(error)}`)
        return undefined
      }
    }
    const existing = findFolderByName(roots, name)
    if (existing) return existing
    try {
      const created = await this.request(`/v1/default/banks/${encodeURIComponent(options.bankId)}/knowledge-base/folders`, {
        body: options.parentId ? {name, parent_id: options.parentId} : {name},
        signal: options.signal,
        timeoutMs: options.timeoutMs,
      })
      const id = String(created?.id ?? created?.folder_id ?? '').trim()
      return id || undefined
    } catch (error) {
      this.logger?.warn(`hindsight: knowledge page folder create failed for "${name}": ${(error as Error)?.message ?? String(error)}`)
      return undefined
    }
  }

  /**
   * Open (or update) the knowledge page for a piece of feature work.
   *
   * The server has no "initiative" endpoint: this is two writes, like the
   * official coding-agents MCP shim. A new initiative creates a page under the
   * Initiatives folder plus a marker memory; a material change to an existing
   * initiative only rewrites its marker, so the same work never grows a second
   * page.
   *
   * Only the marker's `context` (`[[page:<id>]]`) is read back by the page's
   * refresh: reflect strips memory metadata from search results, and page
   * creation on server 1.2.0 takes no client-supplied id, so tag-based page
   * binding is not available. The marker therefore carries the same tags the
   * page filters on and spells the page id out in its context.
   */
  async captureInitiative(options: CaptureInitiativeOptions): Promise<CaptureInitiativeResult> {
    const title = String(options.title ?? '').trim()
    if (!title) throw new Error('captureInitiative: title must not be empty')
    const summary = String(options.summary ?? '').trim()
    if (!summary) throw new Error('captureInitiative: summary must not be empty')
    const bankId = options.bankId
    const tags = uniqueTags(options.tags)
    const related = String(options.relatesToPageId ?? '').trim()

    let roots: KnowledgePageNode[] | undefined
    try {
      const tree = await this.knowledgePageTree({bankId, signal: options.signal, timeoutMs: options.timeoutMs})
      roots = Array.isArray(tree?.roots) ? tree.roots : []
    } catch (error) {
      this.logger?.warn(`hindsight: knowledge page tree unavailable while resolving page "${related || title}": ${(error as Error)?.message ?? String(error)}`)
      roots = undefined
    }

    let pageId = related
    let folderId: string | undefined
    let operationId: string | undefined
    const created = !related
    if (created) {
      folderId = await this.ensureKnowledgePageFolder({
        bankId,
        name: options.folderName ?? INITIATIVE_FOLDER_NAME,
        roots,
        signal: options.signal,
        timeoutMs: options.timeoutMs,
      })
      const maxTokens = options.maxTokens ?? INITIATIVE_PAGE_MAX_TOKENS
      const page = await this.createKnowledgePage({
        bankId,
        name: title,
        sourceQuery: initiativeSourceQuery(title),
        parentId: folderId,
        tags,
        maxTokens,
        trigger: {
          mode: 'delta',
          refresh_cron: expandCronHash(options.refreshCron ?? INITIATIVE_REFRESH_CRON, `${bankId}\0${title}`),
          fact_types: options.factTypes?.length ? options.factTypes : [...INITIATIVE_FACT_TYPES],
          tags_match: options.tagsMatch ?? 'all_strict',
          recall_max_tokens: maxTokens,
        },
        signal: options.signal,
        timeoutMs: options.timeoutMs,
      })
      const responseId = String(page.page_id ?? page.id ?? '').trim()
      pageId = responseId || `initiative-${slugify(title)}`
      if (page.operation_id) operationId = String(page.operation_id)
    } else if (roots) {
      pageId = findPageRef(roots, related) ?? related
    }

    const update = !created
    const markerDocumentId = `initiative-marker-${slugify(pageId)}${update ? '-update' : ''}`
    const marker: RetainItem = {
      content: `${update ? 'Update to an existing initiative' : 'New initiative'}: ${title}. ${summary}`,
      context: `initiative marker for [[page:${pageId}]]`,
      document_id: markerDocumentId,
      tags,
      metadata: {...(options.metadata ?? {}), relatedPageId: pageId},
    }
    const response = await this.retain({
      bankId,
      items: [marker],
      retainAsync: true,
      signal: options.signal,
      timeoutMs: options.timeoutMs,
    })
    const markerOperationId = firstOperationId(response)
    return {
      pageId,
      created,
      folderId,
      markerDocumentId,
      operationId: operationId ?? markerOperationId,
    }
  }
}

function uniqueTags(tags: readonly unknown[] | undefined): string[] {
  const seen = new Set<string>()
  const out: string[] = []
  for (const raw of tags ?? []) {
    const tag = String(raw ?? '').trim()
    if (tag && !seen.has(tag)) {
      seen.add(tag)
      out.push(tag)
    }
  }
  return out
}

/** Depth-first folder lookup, case-insensitive on the name. */
function findFolderByName(nodes: readonly KnowledgePageNode[] | undefined, name: string): string | undefined {
  const wanted = name.trim().toLowerCase()
  for (const node of nodes ?? []) {
    if (node.kind === 'folder' && String(node.name ?? '').trim().toLowerCase() === wanted) return node.id
    const nested = findFolderByName(node.children, name)
    if (nested) return nested
  }
  return undefined
}

/**
 * Resolve a page reference to its id. Exact id first, then exact name, then a
 * unique substring — the same order hindsight_read_page resolves with.
 */
function findPageRef(nodes: readonly KnowledgePageNode[] | undefined, ref: string): string | undefined {
  const pages: KnowledgePageNode[] = []
  const walk = (list: readonly KnowledgePageNode[] | undefined) => {
    for (const node of list ?? []) {
      if (node.kind === 'folder') walk(node.children)
      else pages.push(node)
    }
  }
  walk(nodes)
  const wanted = ref.trim().toLowerCase()
  const byId = pages.find(page => String(page.id).toLowerCase() === wanted)
  if (byId) return byId.id
  const byName = pages.find(page => String(page.name).trim().toLowerCase() === wanted)
  if (byName) return byName.id
  const partial = pages.filter(page => String(page.name).toLowerCase().includes(wanted))
  return partial.length === 1 ? partial[0]!.id : undefined
}

function firstOperationId(response: RetainResponse): string | undefined {
  const ids = Array.isArray(response?.operation_ids) ? response.operation_ids : []
  if (ids.length > 0) return String(ids[0])
  return response?.operation_id ? String(response.operation_id) : undefined
}

/**
 * The generic question a knowledge page answers. Deliberately not scoped in
 * prose: the page's tags (`knowledge:feature-work` + the project cwd tag) are
 * the scope, and a prose clause would only be re-stated on every page.
 */
export function initiativeSourceQuery(title: string): string {
  const name = String(title ?? '').trim()
  return `Summarize the "${name}" initiative: what is being built or changed and why, and its current state — drawn from the project's memory.`
}

/** URL/document-safe slug that keeps CJK and other unicode letters. */
export function slugify(text: string): string {
  const slug = String(text ?? '')
    .toLowerCase()
    .replace(/[^\p{L}\p{N}]+/gu, '-')
    .replace(/^-+|-+$/g, '')
  return slug.slice(0, 48) || 'page'
}

const CRON_FIELD_BOUNDS: ReadonlyArray<readonly [number, number]> = [
  [0, 59], // minute
  [0, 23], // hour
  [1, 28], // day of month
  [1, 12], // month
  [0, 6], // day of week
]

/**
 * Expand a cron template for one page: `H` (or `H(lo-hi)`) becomes a literal
 * value derived from `seed`.
 *
 * The server validates cron with a literal 5-field parser, so the official
 * coding-agents default `'H * * * *'` is rejected with a 400. Hashing on the
 * client keeps the property that matters — initiative pages created in the
 * same hour do not all refresh in the same minute — and the value is stable
 * for a given bank + page title, so re-creating a page does not move its
 * schedule around.
 */
export function expandCronHash(template: string, seed: string): string {
  const fields = String(template ?? '').trim().split(/\s+/).filter(Boolean)
  if (fields.length !== 5) {
    throw new Error(`invalid cron template ${JSON.stringify(template)}; expected 5 whitespace-separated fields`)
  }
  return fields.map((field, index) => {
    const match = /^H(?:\((\d+)-(\d+)\))?$/.exec(field)
    if (!match) return field
    const bounds = match[1] !== undefined && match[2] !== undefined
      ? [Number(match[1]), Number(match[2])] as const
      : CRON_FIELD_BOUNDS[index]!
    const [lo, hi] = bounds
    if (hi < lo) throw new Error(`invalid cron range ${JSON.stringify(field)}`)
    const digest = createHash('sha256').update(`${seed}\0${index}\0${field}`).digest()
    return String(lo + (digest.readUInt32BE(0) % (hi - lo + 1)))
  }).join(' ')
}
