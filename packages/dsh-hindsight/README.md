# dsh-hindsight

Hindsight long-term memory plugin for [DeepSeek Harness](https://github.com/deepseek-ai/deepseek-harness)
(`dsh`). It follows the same design as the Hermes / opencode Hindsight integrations:

- **automatic retention** — completed turns are extracted from the dsh session
  event log and sent to the Hindsight bank in the background;
- **automatic recall** — on each `agent/pre-step` the Hindsight reflect API
  synthesizes relevant memories in the background and queues the result via
  `agent.inject()` for the next step boundary (fail-open, non-blocking, with a
  configurable timeout);
- **explicit tools** — `hindsight_retain`, `hindsight_recall`,
  `hindsight_reflect`, and `hindsight_status`;
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
| `HINDSIGHT_RECALL_TAGS` | Comma-separated recall filter tags |
| `HINDSIGHT_RECALL_TYPES` | Comma-separated recall fact types |

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

    autoRecall: true
    recallTimeoutMs: 6000
    recallMaxTokens: 4096
    recallMaxInputChars: 800
    recallTypes: [experience, observation, world]
```

Prefer `HINDSIGHT_API_KEY` over putting credentials in YAML.

### Config file

Set `configFile: /absolute/path/config.json` in the row config, or set
`HINDSIGHT_CONFIG`. Both camelCase and snake_case keys are accepted, so an
existing Hermes-style `config.json` works as a starting point.

## Tools

| Tool | Purpose |
| --- | --- |
| `hindsight_retain` | Store one piece of information; optional `context` and `tags` |
| `hindsight_recall` | Semantic/keyword/entity-graph search over the bank |
| `hindsight_reflect` | LLM synthesis across stored memories |
| `hindsight_status` | Check the server, API version, and active bank |

## Slash commands

| Command | Purpose |
| --- | --- |
| `/hindsight-import` | Import turns from a historical dsh session into memory; no arguments lists importable sessions. Options: `[sessionId] [--bank <id>] [--max-turns <n>] [--turn-kinds <a,b>]` |

## Hooks

| Hook | Use |
| --- | --- |
| `ctx.tools.register` | Register the four model-facing tools |
| `ctx.commands.register` | Register the `/hindsight-import` slash command |
| `agent/pre-step` (`{ global: true }`) | Schedule a background reflect and queue the synthesized memory via `agent.inject()` for the next step boundary (non-blocking) |
| `session/event` (`turn/end`, `{ global: true }`) | Buffer/retain completed turns |
| `session/disposed` (`{ global: true }`) | Best-effort flush of buffered turns and state cleanup |

Subagent sessions are skipped by default (`skipSubagents: true`).

## Notes

- With `retainDocumentId: null` (default) every retained turn gets a
  per-turn document id (`{sessionId}-turn-{turn}`), so older Hindsight
  servers never overwrite prior turns. Set `retainDocumentId` plus
  `retainUpdateMode: append` when grouping a whole session into one document
  is desired.
- Auto-retain only runs for turn end reasons listed in `retainTurnKinds`
  (default `[completed]`). Tool failures and cancellations are not written as
  memories.
