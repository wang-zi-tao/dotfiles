# Architecture notes

## Why this is a host-side plugin, not an agent-preset plugin

dsh composes capabilities in two planes:

- **Host composition** — cross-session services and registries. This is what a
  profile bundle patch inserts into.
- **Agent preset** — one session's tool plugins, persona, and compaction
  policy, mounted per session and unwound with it.

A Hindsight memory bank is shared across sessions and the automatic hooks must
observe every session (`session/event` with `{ global: true }`). That requires
a host-side row. The package therefore ships a `dsh.bundle.patch` manifest and
`cordis.patch.yml` that inserts the row into the host tree.

## Services used

| Service | Why |
| --- | --- |
| `ctx.tools` | Tool registry; `register()` contributions auto-dispose with the plugin fiber |
| `ctx.commands` | Slash-command registry; `/hindsight-import` (import historical sessions) is registered here, not as a model tool |
| `ctx.systemPrompt` | `section()` adds static Hindsight guidance at tool order 135 |
| `ctx.on` / Cordis event bus | Lifecycle and turn-boundary observation |
| `globalThis.fetch` | HTTP client; no runtime npm dependencies |

## Turn pipeline

```text
session/event (turn/end, kind ∈ retainTurnKinds)
  └─ buildTurnRecord(session, turn)
       ├─ user/message   (source.kind == 'user')
       ├─ assistant/message
       └─ tool/result    (optional, includeToolResults)
  └─ per-session promise chain
       ├─ append to retain buffer
       │    └─ buffer.length >= retainEveryNTurns → POST /memories

agent/pre-step (global: true)
  └─ await next() → default decision with runtime context
  └─ extractUserQuery from payload.messages
  └─ schedule recallAndInject (fire-and-forget, non-blocking)
       ├─ gated by autoRecallOnce: only the agent's first user turn injects
       │  (recallInjectedAgents keyed by agent.id); later turns use the tool
       ├─ recallWithTimeout (fail-open, soft timeout: logs a slow-call
       │  warning and keeps waiting up to config.timeoutMs)
       └─ agent.inject(memoryMessage) → claimed at next step boundary
  └─ schedule mentalModelsAndInject (fire-and-forget, non-blocking)
       ├─ for each wanted model (user + project from session cwd)
       │    ├─ listMentalModels → find by source_query / name / user_advise id
       │    ├─ missing && mentalModelAutoCreate → createMentalModel
       │    │    └─ poll /operations/{operation_id} until completed
       │    └─ getMentalModel → content
       ├─ skip when already injected for this session (agent.id:want.key)
       └─ agent.inject(mentalModelMessage) → claimed at next step boundary
  └─ schedule knowledgePagesAndInject (fire-and-forget, non-blocking)
       ├─ skip until pageRosterRefreshEveryTurns turns have passed
       │  (rosterTurns keyed by agent.id)
       ├─ GET /knowledge-base/tree → flatten pages, trim each description
       │  to one sentence (the raw field repeats the whole source_query)
       └─ agent.inject(knowledgePagesMessage(roster))
            → names + summaries only; bodies are never pushed
```

Retention happens off the reply path. If retain fails, the buffered record is
kept and retried together with the next completed turn.

## Hindsight API surface

`src/client.ts` implements the same endpoints as `hindsight-client` 0.6.x:

When `retainAsync` is enabled the plugin polls `/operations/{operation_id}`
(within `retainDrainTimeoutMs`) to confirm the retain batch completed. Each
retained turn gets its own document id (`{sessionId}-turn-{turn}`), so turns
stay independent across sessions.

| Operation | Endpoint |
| --- | --- |
| retain | `POST /v1/default/banks/{bank_id}/memories` |
| recall | `POST /v1/default/banks/{bank_id}/memories/recall` |
| reflect | `POST /v1/default/banks/{bank_id}/reflect` |
| operation status | `GET /v1/default/banks/{bank_id}/operations/{operation_id}` |
| version | `GET /version` |
| list mental models | `GET /v1/default/banks/{bank_id}/mental-models` |
| get mental model | `GET /v1/default/banks/{bank_id}/mental-models/{id}` |
| create mental model | `POST /v1/default/banks/{bank_id}/mental-models` |
| update mental model | `PATCH /v1/default/banks/{bank_id}/mental-models/{id}` |
| knowledge-page tree | `GET /v1/default/banks/{bank_id}/knowledge-base/tree` |
| knowledge-page search | `GET /v1/default/banks/{bank_id}/knowledge-base/search?q=&limit=` |
| knowledge page | `GET /v1/default/banks/{bank_id}/knowledge-base/pages/{id}` |
| create knowledge folder | `POST /v1/default/banks/{bank_id}/knowledge-base/folders` |
| create knowledge page | `POST /v1/default/banks/{bank_id}/knowledge-base/pages` |

## Why recall is injected at agent/pre-step (once, asynchronously)

Rather than caching a recall result at `turn/end` and reading it synchronously
in a `systemPrompt.context` provider (the Hermes model), dsh-hindsight uses the
Cordis `agent/pre-step` hook. The hook:

1. calls `next()` to obtain the default pre-step decision (which includes the
   runtime context snapshot);
2. extracts the first user query from the step's claimed messages;
3. schedules a **background** Hindsight **recall** — without awaiting it, so
   the agent never waits on the memory fetch. The timeout is *soft*: after
   `recallTimeoutMs` the plugin logs a warning and keeps waiting, because a
   large bank routinely needs 10–15 s to answer;
4. when the recall returns, queues the result as a user-role message via
   `agent.inject()`, which the driver claims at the nearest later step
   boundary (an idle agent leaves it pending for the next follow-up).

Injection is **pull-first**, not per-turn. Hindsight's own 0.9.0 benchmark (61
sde-bench tasks) measured *more* corrections per task with per-prompt memory
injection (1.06 vs 0.97) and a large success drop (Claude Code 0.85 → 0.36), so
the plugin injects recall once per agent/session (`autoRecallOnce`, default
true) and leaves later retrieval to the `hindsight_recall` tool. Set it to
`false` to restore per-turn injection.

The hook is fail-open: timeouts, recall errors, and an unavailable
`agent.inject` never block a turn. Failures are not silent, though — the
recall path (and the mental-model / page-roster paths) reports through
`logFailure`, which prints the `Error.cause` chain so a `fetch failed` does not
hide the connection error underneath. Per-turn dedup (`injectedTurns` set by
`agent.id:turn`) prevents duplicate scheduling on sub-steps.

## Knowledge pages (pull-only)

Hindsight knowledge pages are server-generated, source-backed documents
(component maps, conventions, decisions). The tree endpoint returns each page's
full `source_query` in `description` — 300–400 chars per page, so a naive
roster costs thousands of tokens. The plugin therefore:

- trims each description to its first paragraph and one short sentence
  (`knowledgePageSummary`), turning five pages into a ~700-char roster;
- injects that roster at session start and again only every
  `pageRosterRefreshEveryTurns` turns (`rosterTurns` per agent), capped by
  `pageRosterMaxChars`;
- never injects page bodies. Bodies are fetched only by the three read-only
  tools: `hindsight_list_pages` (roster), `hindsight_search_pages` (page-level
  BM25+vector search, snippets), `hindsight_read_page` (one page, clipped to
  `pageReadMaxChars` and optionally to a per-call `maxChars`);
- resolves a page reference by `kp-...` id, exact name, or unique name
  substring; an ambiguous reference errors with the candidate names rather than
  guessing.

The important asymmetry: the roster is pushed (small, cheap, solves "the agent
does not know a page exists"), while bodies are pulled (large, only when the
task needs them). Search snippets are the page's opening text, so a page's
first lines must carry the conclusions — the page-generation contract writes a
TL;DR first for exactly this reason.

### Initiative pages (write side)

The server has no capture endpoint: `hindsight_capture_initiative` is the same
three-request composition as `@vectorize-io/hindsight-coding-agents` 0.8.7
(`HindsightClient.captureInitiative`): ensure the `Initiatives` folder (tree
lookup, case-insensitive, created only when missing, fail-open), create the page
(`name = title`, fixed `source_query`, `parent_id`, `knowledge:feature-work`
tier tag, `max_tokens`, trigger), then retain a marker document that links the
page: `document_id = initiative-marker-<page>[-update]`,
`context = initiative marker for [[page:<id>]]`, `metadata.relatedPageId`.

Only `context` survives into page synthesis: reflect/recall results strip
`metadata`, and tags are a set filter, not an identity channel. The 0.8.7 client
writes `relatedPageId:<id>` into tags only on a path that requires a
client-supplied page id (`kp-<32hex>`), and the 1.2.0 `CreatePageRequest` has
no `page_id` field, so that channel is unavailable here.

An update (`relates_to_page_id`) never creates a page: it writes
`initiative-marker-<page>-update` and leaves the existing body to refresh on its
own schedule. The page body is always server-generated from memories carrying
the page's tags — the agent contributes the intent marker, never the text.

The trigger is fixed to `mode: delta`, `tags_match: all_strict`,
`fact_types: [world, experience, observation]` and a per-page cron. Cron
templates (`H * * * *`, `H(lo-hi)` supported) are expanded client-side by
`expandCronHash` because the server validates a literal five-field cron and
rejects `H` with a 400; the hash input is `bank + index + field` so a page's
minute is stable across re-creations.

## Mental-model injection (auto-create + inject)

On `agent/pre-step` the plugin also ensures the curated mental models exist and
injects their current content — at most once per session:

- **user model** — `source_query = mentalModelUserQuery` (default `用户偏好`);
  auto-created with id `user_advise` when missing. Always on with
  `autoMentalModel`.
- **project model** — one per session cwd; `source_query` is
  `mentalModelProjectQueryTemplate` with `{cwd}` replaced by
  `agent.session.header.cwd` (skipped when the session has no cwd or when
  `mentalModelProjectEnabled` is false, which is the default: the knowledge
  pages already carry project knowledge, and a second per-project model
  duplicated it in every session's first turn).

The mental-models endpoints are distinct from recall: they return the
pre-synthesized standing answer (a plain database read on the server, ~ms),
not fresh retrieval. Injection is tracked per session (agent) in
`injectedMentalModels` keyed by `agent.id:want.key`, so each session receives
the settled content exactly once and later turns skip the query entirely —
there is no per-turn polling. A wanted model that is not ready yet (content
empty, e.g. auto-create still reflecting) is left unmarked and retried on a
later turn of the same session. Cross-session freshness is provided by the
server-side refresh, not by re-injection. The user requested no truncation, so
full content is injected.

Auto-created models use `mentalModelRefreshMode` (default `delta`) and
`refresh_after_consolidation: <autoRefresh>` — true for the user model, false
for a project model unless `mentalModelProjectAutoRefresh` is set. Missing
models are created
asynchronously and polled via `/operations/{operation_id}` until the
background reflect completes (`mentalModelTimeoutMs` cap, fail-open on
timeout/error). In-flight creations are deduplicated per wanted key.

### Muting stale project models

A project model created by an older version keeps
`refresh_after_consolidation: true`, so the server would run a full LLM reflect
after every consolidation forever — for content nothing reads any more, since
project models are no longer injected. `muteProjectModelAutoRefresh` runs one
sweep per process (before the first seed attempt): list the models, take the
ones whose tags start with `retainCwdTagPrefix`, fetch each (the list endpoint
returns no trigger), and `PATCH` those still auto-refreshing to
`{mode, refresh_after_consolidation: false, refresh_cron: null}`. The PATCH is
a field merge on the server: tags, content and fact types survive. Models
without a cwd tag (the user model) and knowledge-page backed models
(`project:*` / `knowledge:*` tags) are never touched. Fail-open, one-shot: a
failure logs and is retried on the next process start, never on every turn.

### Scope tags (why a model can silently read nothing)

`MentalModel.tags` is not decoration — it is the scope of the memories the
model's internal reflect may read. The server defaults `trigger.tags_match` to
`all_strict` **when the model has tags** (`any` when it has none), meaning a
memory must carry *every* model tag and untagged memories are excluded. A model
tagged with labels its memories do not carry therefore refreshes to **empty
content**, silently.

The plugin's contract is therefore:

- retain writes `cwd:<session cwd>` on every memory
  (`retainCwdTagPrefix`, default `cwd:`, empty disables);
- the project model declares the same `cwd:<cwd>` in its `tags`;
- creation pins `trigger.tags_match = 'all_strict'` explicitly rather than
  relying on the server default, so a tagged model never ORs its tag with
  untagged memories;
- the cwd is never normalised (case, trailing separator): model identity is
  derived from the same raw string, and normalising one side only would break
  the match;
- a *found* model is reconciled before injection: `reconcileMentalModel`
  PATCHes `tags` only when the wanted scope is non-empty and differs from the
  model's, leaving the stored content intact until the next scoped refresh. An
  empty wanted scope means "leave the scope alone", never "clear it". The same
  PATCH carries `{mode, refresh_after_consolidation: false, refresh_cron: null}`
  when the caller wants the model muted (project models by default), so tags and
  trigger state never disagree.

Because the patch changes only the scope, adopting this on an existing bank
requires backfilling `cwd` tags onto previously retained documents — otherwise
the re-scoped models have nothing in scope to read.

Like recall, the whole path is fire-and-forget and fail-open: the decision
is returned before any memory work, and errors are logged (with their
`cause` chain) rather than thrown.

## Configuration ownership

`resolveConfig()` deliberately merges defaults < JSON config file < Cordis row
config < environment. Environment is highest so the same profile can point at
different banks without editing YAML. `cordis.patch.yml` additionally uses
`!!js process.env.HINDSIGHT_* ?? default` expressions for the initial row, but
the plugin re-reads the environment itself so a later profile patch that
replaces the row config does not lose env overrides.
