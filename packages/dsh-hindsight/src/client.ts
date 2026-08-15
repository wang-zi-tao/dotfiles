/**
 * Minimal Hindsight HTTP API client.
 *
 * Endpoints mirror `hindsight-client` 0.6.x:
 *   retain:    POST /v1/default/banks/{bank_id}/memories
 *   recall:    POST /v1/default/banks/{bank_id}/memories/recall
 *   reflect:   POST /v1/default/banks/{bank_id}/reflect
 *   operation: GET  /v1/default/banks/{bank_id}/operations/{operation_id}
 *   version:   GET  /version
 */

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

export interface HindsightClientOptions {
  apiUrl: string
  apiKey?: string | null
  timeoutMs?: number
  userAgent?: string
  fetchImpl?: typeof globalThis.fetch
}

interface RequestOptions {
  method?: 'GET' | 'POST'
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

  constructor(options: HindsightClientOptions) {
    this.apiUrl = String(options.apiUrl).replace(/\/+$/, '')
    this.apiKey = options.apiKey ?? null
    this.timeoutMs = options.timeoutMs ?? 120000
    this.userAgent = options.userAgent ?? USER_AGENT
    this.fetchImpl = options.fetchImpl ?? globalThis.fetch
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
      } catch {
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
}
