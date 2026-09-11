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
       ├─ recallWithTimeout (fail-open, async)
       └─ agent.inject(memoryMessage) → claimed at next step boundary
  └─ schedule mentalModelsAndInject (fire-and-forget, non-blocking)
       ├─ for each wanted model (user + project from session cwd)
       │    ├─ listMentalModels → find by source_query / name / user_advise id
       │    ├─ missing && mentalModelAutoCreate → createMentalModel
       │    │    └─ poll /operations/{operation_id} until completed
       │    └─ getMentalModel → content
       ├─ skip when already injected for this session (agent.id:want.key)
       └─ agent.inject(mentalModelMessage) → claimed at next step boundary
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

## Why recall is injected at agent/pre-step (reflect, asynchronously)

Rather than caching a recall result at `turn/end` and reading it synchronously
in a `systemPrompt.context` provider (the Hermes model), dsh-hindsight uses the
Cordis `agent/pre-step` hook. The hook:

1. calls `next()` to obtain the default pre-step decision (which includes the
   runtime context snapshot);
2. extracts the first user query from the step's claimed messages;
3. schedules a **background** Hindsight **reflect** (LLM synthesis) with a
   timeout — without awaiting it, so the agent never waits on the memory fetch;
4. when the reflection returns, queues the result as a user-role message via
   `agent.inject()`, which the driver claims at the nearest later step
   boundary (an idle agent leaves it pending for the next follow-up).

This avoids a synchronous cache entirely and guarantees every step gets a fresh
reflection without blocking the turn. The hook is fail-open: timeouts, reflect
errors, and an unavailable `agent.inject` never block a turn. Per-turn dedup
(`injectedTurns` set by `agent.id:turn`) prevents duplicate reflect
scheduling on sub-steps.

## Mental-model injection (auto-create + inject)

On `agent/pre-step` the plugin also ensures two curated mental models exist
and injects their current content — at most once per session:

- **user model** — `source_query = mentalModelUserQuery` (default `用户偏好`);
  auto-created with id `user_advise` when missing;
- **project model** — one per session cwd; `source_query` is
  `mentalModelProjectQueryTemplate` with `{cwd}` replaced by
  `agent.session.header.cwd` (skipped when the session has no cwd).

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
`refresh_after_consolidation: true`, so the server keeps them current
incrementally after each consolidation. Missing models are created
asynchronously and polled via `/operations/{operation_id}` until the
background reflect completes (`mentalModelTimeoutMs` cap, fail-open on
timeout/error). In-flight creations are deduplicated per wanted key.

Like recall, the whole path is fire-and-forget and fail-open: the decision
is returned before any memory work, and errors only log.

## Configuration ownership

`resolveConfig()` deliberately merges defaults < JSON config file < Cordis row
config < environment. Environment is highest so the same profile can point at
different banks without editing YAML. `cordis.patch.yml` additionally uses
`!!js process.env.HINDSIGHT_* ?? default` expressions for the initial row, but
the plugin re-reads the environment itself so a later profile patch that
replaces the row config does not lose env overrides.
