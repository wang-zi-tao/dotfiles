# dsh-hindsight

Hindsight long-term memory plugin for [DeepSeek Harness](https://github.com/deepseek-ai/deepseek-harness)
(`dsh`). It follows the same design as the Hermes / opencode Hindsight integrations:

- **automatic retention** — completed turns are extracted from the dsh session
  event log and sent to the Hindsight bank in the background;
- **automatic recall, pull-first** — the first user turn of a session gets one
  background recall whose result is queued via `agent.inject()` for the next
  step boundary (fail-open, non-blocking, timeout-bounded). Later turns use the
  `hindsight_recall` tool: Hindsight's 0.9.x benchmarks showed that pushing
  memories into every prompt costs focus, so injection is capped by
  `autoRecallOnce` (set it to `false` for the old per-turn behaviour);
- **knowledge pages, pull-only** — a roster of the bank's curated pages (name,
  folder, one-line summary; never bodies) is injected at session start and
  refreshed every `pageRosterRefreshEveryTurns` turns. The page tools fetch
  content on demand: `hindsight_list_pages` → `hindsight_search_pages` →
  `hindsight_read_page`;
- **mental-model injection** — on each `agent/pre-step` the plugin looks up
  the user-preference mental model and a per-project mental model (keyed by the
  session cwd), auto-creates either when it is missing, and injects the current
  content via `agent.inject()` when it has changed. No truncation; refresh is
  left to the server (delta mode by default);
- **explicit tools** — `hindsight_retain`, `hindsight_recall`,
  `hindsight_reflect`, `hindsight_status`, and the three read-only page tools;
- **slash command** — `/hindsight-import` imports turns from a historical dsh
  session into the memory bank (list sessions with no arguments);
- **configurable endpoint and bank** — via the Cordis row config and
  `HINDSIGHT_*` environment variables.

The package is written in TypeScript, compiled with nixpkgs TypeScript, and has no
runtime npm dependencies. It speaks the Hindsight HTTP API directly
(client-compatible with `hindsight-client` 0.6.x).

## Build

```bash
nix build .#dsh-hindsight
```

The flake already auto-discovers packages from `packages/<name>`, so the new
directory is exposed as `.#dsh-hindsight` without editing `flake.nix`.

> The local flake is a git flake: until the new directory is committed, run
> `git add -N packages/dsh-hindsight` once so the dirty working tree is
> visible to `nix build .#dsh-hindsight`.

## Activate in a dsh profile

1. Link the built package into the target profile's `node_modules`:

   ```bash
   out=$(nix build --no-link --print-out-paths .#dsh-hindsight)
   profile="$HOME/.dsh/profiles/web"
   mkdir -p "$profile/node_modules"
   ln -sfn "$out/lib/node_modules/dsh-hindsight" \
     "$profile/node_modules/dsh-hindsight"
   ```

2. Add `"dsh-hindsight"` to the end of `dsh.profile.bundles` in
   `$profile/package.json` (after `@deepseek-ai/dsh-base`). The package
   declares `dsh.bundle.patch`, so dsh will apply its `cordis.patch.yml` and
   insert the plugin row:

   ```json
   {
     "dsh": {
       "profile": {
         "bundles": [
           "@deepseek-ai/dsh-base",
           "@deepseek-ai/dsh-web-app",
           "dsh-hindsight"
         ]
       }
     }
   }
   ```

3. Restart `dsh --profile web`.

See [ARCHITECTURE.md](ARCHITECTURE.md) for why the row is host-side.

## Configuration

Precedence, highest wins:

1. `HINDSIGHT_*` environment variables;
2. the row `config` in the profile's `cordis.patch.yml`;
3. an optional JSON file named by `configFile` / `HINDSIGHT_CONFIG`;
4. built-in defaults.

### Environment variables

| Variable | Description |
| --- | --- |
| `HINDSIGHT_API_URL` | Hindsight server URL, e.g. `http://127.0.0.1:8888` |
| `HINDSIGHT_API_KEY` | Bearer API key |
| `HINDSIGHT_BANK_ID` | Memory bank id |
| `HINDSIGHT_BUDGET` | `low`, `mid`, or `high` |
| `HINDSIGHT_TIMEOUT` | Request timeout in milliseconds |
| `HINDSIGHT_MEMORY_MODE` | `hybrid`, `context`, or `tools` |
| `HINDSIGHT_RETAIN_TAGS` | Comma-separated default retain tags |
| `HINDSIGHT_RETAIN_CWD_TAG_PREFIX` | Prefix of the per-session working-directory visibility tag (default `cwd:`); empty disables the tag |
| `HINDSIGHT_RECALL_TAGS` | Comma-separated recall filter tags |
| `HINDSIGHT_RECALL_TYPES` | Comma-separated recall fact types |
| `HINDSIGHT_AUTO_MENTAL_MODEL` | Enable/disable mental-model injection (`true`/`false`) |
| `HINDSIGHT_MENTAL_MODEL_USER_QUERY` | `source_query` for the user-preference mental model |
| `HINDSIGHT_MENTAL_MODEL_PROJECT_QUERY_TEMPLATE` | Template for the per-project model; `{cwd}` is replaced by the session cwd |
| `HINDSIGHT_MENTAL_MODEL_MAX_TOKENS` | `max_tokens` for auto-created mental models |
| `HINDSIGHT_MENTAL_MODEL_REFRESH_MODE` | Refresh mode for auto-created models: `full` or `delta` |
| `HINDSIGHT_MENTAL_MODEL_AUTO_CREATE` | Auto-create a missing mental model (`true`/`false`) |

### Row config

Override the whole row by id in `~/.dsh/profiles/web/cordis.patch.yml`.
Remember that dsh patch rows replace the entire `config`; omitted keys fall
back to the plugin defaults.

```yaml
- id: hindsight
  config:
    apiUrl: http://127.0.0.1:8888
    apiKey: sk-test
    bankId: work
    budget: mid
    timeoutMs: 120000
    memoryMode: hybrid

    autoRetain: true
    retainEveryNTurns: 1
    retainAsync: true
    retainWaitForOperations: true
    retainDrainTimeoutMs: 10000
    retainContext: conversation between a coding agent and the user
    retainTags: [source:dsh-hindsight]
    # Visibility scope written on every retained memory: `cwd:<cwd>`.
    # Empty disables it. Keep it identical to the mental-model scope below.
    retainCwdTagPrefix: 'cwd:'

    autoRecall: true
    # Only the first user turn of a session injects recall; later turns pull it
    # with hindsight_recall. Set false for per-turn injection.
    autoRecallOnce: true
    recallTimeoutMs: 6000
    recallMaxTokens: 4096
    recallMaxInputChars: 800
    recallTypes: [experience, observation, world]

    autoMentalModel: true
    mentalModelUserQuery: 用户偏好
    # Extra scope tags. The project model always gets the session's `cwd:<cwd>`
    # tag; these narrow it further. The user model is only scoped by what you
    # list here — leave it empty to keep it reading the whole bank.
    mentalModelUserTags: []
    mentalModelProjectTags: []
    mentalModelProjectQueryTemplate: |
      项目 {cwd} 的
      - 概述
      - 项目架构
      - 设计偏好
      - 相关事件
      - 相关修改
      - 重要实体
    mentalModelMaxTokens: 4096
    mentalModelRefreshMode: delta
    mentalModelAutoCreate: true
    mentalModelTimeoutMs: 120000
    mentalModelRequestTimeoutMs: 10000
    mentalModelPollIntervalMs: 3000

    # Read-only knowledge pages. Only the roster (names + one-line summaries)
    # is injected; bodies are fetched by the page tools when a task needs them.
    knowledgePagesEnabled: true
    injectPageRoster: true
    pageRosterRefreshEveryTurns: 10
    pageRosterMaxChars: 1200
    pageSearchLimit: 3
    pageReadMaxChars: 8000
```

Prefer `HINDSIGHT_API_KEY` over putting credentials in YAML.

### Config file

Set `configFile: /absolute/path/config.json` in the row config, or set
`HINDSIGHT_CONFIG`. Both camelCase and snake_case keys are accepted, so an
existing Hermes-style `config.json` works as a starting point.

## Visibility tags (`cwd` scope)

Hindsight memories and mental models both carry **tags**, and a mental model's
tags *are* the scope of the memories it can read. The semantics (Hindsight
0.10.x):

| Where | Meaning |
| --- | --- |
| `MemoryItem.tags` (retain) | Visibility scope stored on the document, its facts and its observations. `RetainRequest.document_tags` is deprecated in favour of item-level tags. |
| `RecallRequest.tags` / `tags_match` | `any`/`all` also match untagged memories; `any_strict`/`all_strict` exclude them; `exact` is set equality. |
| `MentalModel.tags` | "Tags for scoped visibility" — the refresh's internal recall/freflect only sees memories carrying them. |
| `MentalModel.trigger.tags_match` | Defaults to `all_strict` **when the model has tags**, `any` when it has none. Under `all_strict` a memory must carry *every* model tag and untagged memories are excluded. |

Consequences this plugin is built around:

- A tagged project model whose memories do **not** carry the same tag refreshes
  to **empty content** — the failure is silent, so the tag written by retain and
  the tag declared on the model must be byte-identical.
- The cwd is written **verbatim** (`cwd:C:\dotfiles-copy`), never normalised,
  because the mental-model identity (`source_query` / name) is derived from the
  same raw string.
- The session tag (`session:<id>`) is *per-call provenance*: useful for
  filtering, but it fragments consolidation because the default
  `observation_scopes: combined` consolidates one tag-set at a time.

What the plugin writes:

| Producer | Tags |
| --- | --- |
| auto-retain (`turn/end`) | `retainTags` + `cwd:<session cwd>` + `session:<session id>` |
| `hindsight_retain` tool | `retainTags` + `cwd:<calling session cwd>` + tool `tags` |
| project mental model | `cwd:<session cwd>` + `mentalModelProjectTags` |
| user mental model | `mentalModelUserTags` (empty by default — reads the whole bank) |

Existing models are **re-scoped in place**: when a wanted model is found its
tags are compared with the wanted scope and a `PATCH` is issued only on a
difference (never on an empty wanted scope — an empty list means "leave it
alone", not "clear it"). The patch changes the scope only; the stored content
is preserved until the server's next scoped refresh.

> Upgrading an existing bank: memories retained before this change carry no
> `cwd` tag, so a freshly scoped project model will read nothing until those
> documents are backfilled. `scripts/backfill-cwd-tags.mjs` does that through
> the documents API.

## Tools

| Tool | Purpose |
| --- | --- |
| `hindsight_retain` | Store one piece of information; optional `context` and `tags` |
| `hindsight_recall` | Semantic/keyword/entity-graph search over the bank |
| `hindsight_reflect` | LLM synthesis across stored memories |
| `hindsight_status` | Check the server, API version, and active bank |
| `hindsight_list_pages` | Read-only roster of curated knowledge pages (name, folder, one-line summary) |
| `hindsight_search_pages` | Read-only BM25+vector search over page bodies; returns snippets, not full pages |
| `hindsight_read_page` | Read one page by name, unique name substring or `kp-...` id; `maxChars` caps the body |

## Knowledge pages

Hindsight *knowledge pages* are curated, server-generated documents (component
maps, conventions, decisions) whose bodies are synthesized from memories and
refreshed on a schedule. They are the right source when a task needs standing
project knowledge rather than one remembered fact.

The plugin treats them as **pull, not push**. The reasoning is empirical:
Hindsight's 0.9.0 benchmark (61 sde-bench tasks) found that injecting recalled
corrections into every prompt made agents *worse* (1.06 vs 0.97 corrections per
task), and final success dropped from 0.85 to 0.36 with per-turn injection.
Page bodies are the most expensive memory in the bank — a single page can be
4 KB — so the plugin only ever:

1. injects the roster at session start (default cap 1 200 chars, ~150 tokens for
   five pages) and on the first turn after every `pageRosterRefreshEveryTurns`
   turns;
2. leaves page selection to the agent: list → search → read one page;
3. caps what a read returns (`pageReadMaxChars`, agent-overridable per call).

If the roster alone is not enough to locate a page, the agent falls back to the
memory tools rather than reading the tree, because the tree endpoint repeats the
full `source_query` of each page and costs thousands of tokens — the plugin
trims `description` to one sentence for the roster and never injects the tree.

## Slash commands

| Command | Purpose |
| --- | --- |
| `/hindsight-import` | Import turns from a historical dsh session into memory; no arguments lists importable sessions. Options: `[sessionId] [--bank <id>] [--max-turns <n>] [--turn-kinds <a,b>]` |

## Hooks

| Hook | Use |
| --- | --- |
| `ctx.tools.register` | Register the model-facing memory and page tools |
| `ctx.commands.register` | Register the `/hindsight-import` slash command |
| `agent/pre-step` (`{ global: true }`) | Schedule the one-shot background recall, the mental-model fetch, and (every N turns) the page-roster fetch; all results are queued via `agent.inject()` for the next step boundary (non-blocking) |
| `session/event` (`turn/end`, `{ global: true }`) | Buffer/retain completed turns |
| `session/disposed` (`{ global: true }`) | Best-effort flush of buffered turns and state cleanup |

Subagent sessions are skipped by default (`skipSubagents: true`).

## Notes

- With `retainDocumentId: null` (default) every retained turn gets a
  per-turn document id (`{sessionId}-turn-{turn}`), written with
  `update_mode: replace`. The record is a full snapshot of the turn, never a
  delta, so replacing keeps a re-send (a batch retried after a failure, a
  replayed or resumed session) idempotent — appending the snapshot to itself
  is refused by the server's append guard, which requires the new body to
  extend the stored one byte-for-byte. Set `retainDocumentId` to group a whole
  session into one document, where appending each turn is the point and is the
  default for that mode. `retainUpdateMode` overrides the mode in both cases.
- Auto-retain only runs for turn end reasons listed in `retainTurnKinds`
  (default `[completed]`). Tool failures and cancellations are not written as
  memories.
- Mental-model injection is fail-open and non-blocking: it looks up the user
  model (source_query `用户偏好`, id `user_advise` when auto-created) and one
  project model per session cwd (source_query from
  `mentalModelProjectQueryTemplate`). Missing models are auto-created with the
  configured `mentalModelRefreshMode`, `refresh_after_consolidation: true` and
  `tags_match: 'all_strict'` whenever they carry tags, then polled via the
  operations endpoint until the background reflect finishes.
- Project models are scoped by the session's cwd tag (see
  [Visibility tags](#visibility-tags-cwd-scope)); a model found with a different
  scope is re-scoped with a `PATCH` before it is injected.
- Injection happens **at most once per session (agent)**: the plugin records
  which mental models were already delivered to a given agent and skips both the
  query and the inject on later turns of that session. A model that is not ready
  yet (e.g. auto-create still reflecting) is not marked, so a later turn retries.
  Each new session gets the current settled content; cross-session freshness
  comes from the server-side refresh (delta mode by default). Content is not
  truncated.
