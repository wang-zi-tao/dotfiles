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
| `ctx.systemPrompt` | `context()` injects recall as a durable user-role snapshot; `section()` adds tool guidance |
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
       └─ recall(current user query)
            └─ recallCache[session.id] = formatted results

next model step
  └─ systemPrompt.context('hindsight:recall')
       └─ returns cached recall text (empty when none)
```

Retention happens off the reply path. If retain fails, the buffered record is
kept and retried together with the next completed turn.

## Hindsight API surface

`src/client.ts` implements the same endpoints as `hindsight-client` 0.6.x:

When `retainAsync` is enabled the plugin polls `/operations/{operation_id}`
(within `retainDrainTimeoutMs`) before prefetching recall, so the next-turn
recall can observe the turn that was just retained.

| Operation | Endpoint |
| --- | --- |
| retain | `POST /v1/default/banks/{bank_id}/memories` |
| recall | `POST /v1/default/banks/{bank_id}/memories/recall` |
| reflect | `POST /v1/default/banks/{bank_id}/reflect` |
| operation status | `GET /v1/default/banks/{bank_id}/operations/{operation_id}` |
| version | `GET /version` |

## Why recall is cached instead of fetched in `systemPrompt.context`

dsh's `PromptContext.text` provider is synchronous. A network recall can
therefore not run during prompt assembly. The plugin runs recall at
`turn/end` (after the just-retained turn is visible to the API) and the
synchronous context provider only reads the cached result. This is the same
next-turn prefetch model used by the Hermes Hindsight provider.

## Configuration ownership

`resolveConfig()` deliberately merges defaults < JSON config file < Cordis row
config < environment. Environment is highest so the same profile can point at
different banks without editing YAML. `cordis.patch.yml` additionally uses
`!!js process.env.HINDSIGHT_* ?? default` expressions for the initial row, but
the plugin re-reads the environment itself so a later profile patch that
replaces the row config does not lose env overrides.
