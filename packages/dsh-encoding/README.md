# dsh-encoding

Post-write source encoding repair for DeepSeek Harness.

The harness file tools leave two invisible marks on C/C++ and CMake sources, and
both of them cost real money in a WPS-sized tree:

| what the tool does | why it happens | what it costs |
| --- | --- | --- |
| the UTF-8 BOM disappears | `TextDecoder` strips U+FEFF, and the decoded text is what gets written back | a non-ASCII source without a BOM raises **C4819**, and `/WX` turns that into **C2220** |
| `edit` re-applyies the file's *majority* terminator to the *whole* file | `restoreLineEndings` normalizes once it has chosen a style | one small edit flips every minority-terminator line, so the diff is the whole file |
| a stray bare CR survives | nothing normalizes lone CRs | **C4335** ("Mac file format"), `/WX`, **C2220** |

This plugin repairs all three after the write lands, without touching a single
line the write did not change.

## Install

```powershell
cd C:\dotfiles-copy\packages\dsh-encoding
npm install --include=dev --no-audit --no-fund   # --include=dev: this machine sets omit=dev
npm run build
```

Then add the bundle to a DSH profile's `package.json`:

```jsonc
{
  "dependencies": { "dsh-encoding": "file:C:/dotfiles-copy/packages/dsh-encoding" },
  "dsh": { "profile": { "bundles": [ /* ... */ "dsh-encoding" ] } }
}
```

Restart the DSH session. The plugin has no model-facing surface — it registers
no tool, no command and no prompt section — so nothing about it is visible to
the model, and nothing needs to be.

## Behaviour

### When it runs

| stage | event | what it does |
| --- | --- | --- |
| before the write | `tools/pre-execute` | if the target matches `include`/`exclude`, snapshot its raw bytes |
| after the write | `tools/post-execute` | recompute the file and rewrite it when the bytes differ |

Both hooks register with `{ global: true }`, so a write made by a subagent is
repaired too.

The snapshot is not optional. The tool result's own `before`/`after` text is
LF-normalized and BOM-stripped *by construction*, so the two facts the repair
needs — the original per-line terminators and the original BOM state — exist
only in the bytes on disk before the write.

The post-execute stage computes the repaired bytes and compares them with the
file before writing, so a conforming file is left completely alone (no mtime
churn, no watcher churn).

### BOM policy (`bom`, default `non-ascii`)

Measured on `D:\branch-master\wpsmain\Coding` (non-ASCII is tested on the
*decoded* text, with the BOM excluded — the BOM bytes are themselves > 0x7F, so
counting raw bytes gets this wrong):

| sample | BOM + non-ASCII | BOM + ASCII only | no BOM + non-ASCII | no BOM + ASCII only |
| --- | --- | --- | --- | --- |
| `*.cpp`/`*.h` family (2999) | 315 | 2578 | **0** | 106 |
| `CMakeLists.txt` (2411) | 100 | 373 | 262 | 1676 |
| `*.cmake` (114) | 5 | 3 | 50 | 56 |

Two separate facts fall out of that, and they are not the same fact:

- **The hard constraint, and where it applies: non-ASCII ⟹ BOM for C/C++.**
  In the C/C++ family there is not a single file with non-ASCII and no BOM, and
  the compiler enforces exactly that direction (C4819 → `/WX` → C2220). CMake is
  not compiled by MSVC, so it carries no such constraint at all — which is why
  262 `CMakeLists.txt` files with non-ASCII sit there without a BOM and
  configure perfectly well.
- **The soft convention: C/C++ carries a BOM anyway.** 96.5% of the family has
  one, so 2578 of those BOMs sit on pure-ASCII files. A pure-ASCII source
  compiles identically with or without a BOM, in any codepage.

`non-ascii` implements the **hard** rule and is **additive**: it adds the BOM a
non-ASCII file is missing, and never strips a BOM that is already there —
stripping could only churn a file that compiles today. As a consequence a
brand-new pure-ASCII C/C++ file gets no BOM; the 106 existing files in that shape
say that is a tolerated state, but if you would rather match the tree's dominant
style, set `bom: always` (and accept a 3-byte diff on the ASCII-only minority).

Other values: `always`, `never`, `preserve` (mirror the pre-write state).

### CMake is exempt from it

CMake is not compiled by MSVC, so there is no C4819 to dodge and no reason for
the plugin to put a BOM on a CMake file. `overrides` carries that difference:

```yaml
bom: non-ascii            # the global default: C/C++
overrides:
  - match:                # the CMake family
      - '**/CMakeLists.txt'
      - '**/*.cmake'
      - '**/*.cmake.in'
    bom: preserve         # never ADD a BOM to CMake
```

`preserve` is the narrow reading of "CMake does not need a BOM": the plugin
never *adds* one, and never strips one the file already had either, because
removing it is a gratuitous diff on a file that configures fine today. Switch
that one word to `never` if the intent is to converge the tree on BOM-free
CMake — the 473 `CMakeLists.txt` files that carry a BOM today would then lose
it the next time the model edits them.

An override may set `bom`, `eol`, or both. Overrides from the row config are
consulted **before** the built-in ones, so whatever `cordis.patch.yml` says
wins, and adding an unrelated override does not silently drop the CMake rule.
Matching is first-match-wins, field by field.

### Line-ending policy (`eol`, default `crlf`)

Newly written lines — the ones the write actually introduced or changed — get
**CRLF**. Every other line keeps its own pre-write terminator, decided in two
tiers:

1. **Positional.** The common prefix and suffix of the pre-write and post-write
   files are byte-identical by line body, so they map 1:1 and keep their exact
   terminators. This is what makes "the model re-emitted the whole file" cost
   **zero** diff: in an all-CRLF file rewritten with LF text, every line is in
   the prefix and nothing moves.
2. **Content.** Inside the changed region, a line whose body matches a
   pre-write line keeps the terminator that body used most often. Only text with
   no precedent in the original — genuinely new or edited lines — takes
   `crlf`.

A bare CR (`fixLoneCr`, default `true`) is never preserved, not even on an
otherwise untouched line: it is the C4335 trigger, and preserving it would trade
a diff for a build break. A missing final terminator is never invented.

Other values: `lf`, `preserve` (majority terminator of the pre-write file).

### What it refuses to touch

- UTF-16 content (either BOM).
- Anything with a NUL byte in the first 8 KiB.
- Text that is not valid UTF-8. Decoding lossily and re-encoding would rewrite
  the invalid bytes into U+FFFD, so a file that cannot round-trip is left alone.
- Anything the `include`/`exclude` sets reject; the default exclude set covers
  `.git`, `.vs`, `.idea`, `node_modules` and `CMakeFiles`.

The plugin never throws into the tool pipeline: every hook body is wrapped, and
the pipeline decision it was handed is always returned unchanged.

## Config

Row config in `cordis.patch.yml`; built-in defaults < row config, and a later
profile patch replaces the whole row config by id.

| key | default | meaning |
| --- | --- | --- |
| `enabled` | `true` | master switch |
| `tools` | `[write, edit]` | tools whose writes are repaired |
| `include` | C/C++ + CMake globs | which files the repair applies to |
| `exclude` | VCS / `node_modules` / `CMakeFiles` | wins over `include` |
| `bom` | `non-ascii` | `non-ascii` \| `always` \| `never` \| `preserve` |
| `eol` | `crlf` | `crlf` \| `lf` \| `preserve` |
| `overrides` | CMake → `bom: preserve` | per-file-type patches, first match wins |
| `preserveUntouchedEol` | `true` | keep each untouched line's own terminator |
| `fixLoneCr` | `true` | rewrite bare CRs to the target terminator |
| `maxSnapshotEntries` | `64` | pre-write snapshots held at once |
| `snapshotTtlMs` | `300000` | how long a snapshot stays usable |
| `log` | `summary` | `off` \| `summary` \| `debug` |
| `dryRun` | `false` | compute and log, never write |

Glob syntax: `*` (within a segment), `?`, `**` (any depth, `**/` also matches
zero segments), `{a,b}`. A pattern with no `/` matches the basename at any
depth. Character classes are intentionally unsupported.

## Verification

- `npm run check` — 66 unit + integration tests, including the full
  pre-execute → tool write → post-execute round trip.
- End-to-end against 1499 real files under `D:\branch-master\wpsmain\Coding`:
  re-emitting every file unchanged through the harness's own decode/write
  pipeline (BOM stripped, text LF-normalized, and on the `edit` path the whole
  file converted to its majority terminator) and running the repair restored
  **1498 of 1499 byte-for-byte** on both the `write` and the `edit` path.
- The single exception is the plugin working rather than failing:
  `Coding/api_bundle/api/etapi/CMakeLists.txt` contains a bare CR, so its line
  endings were rewritten to CRLF instead of restored. Everything else came back
  exactly as it went in — including
  `Coding/api_bundle/api/jsapi/CMakeLists.txt`, which has non-ASCII and no BOM
  and which the plugin now leaves completely alone thanks to the CMake
  exemption.

### Line-ending census of the same sample

| style | files |
| --- | --- |
| CRLF throughout | 1464 |
| LF throughout | 30 |
| genuinely mixed | 5 |
| contains a bare CR | 1 |

So the tree is far more uniform than it feels; the practical effect of the
plugin on the 97.7% CRLF majority is simply "the CRLF the file already had comes
back", and the lone-CR file is a real C4335 waiting to happen.

### Backfilling existing files

The plugin only repairs files as they are written. To sweep a tree that already
contains non-ASCII sources with no BOM, set `dryRun: true` first, then call
`repairBytes` over whatever list you want to fix. Files the plugin never
revisits are unaffected, which is deliberate: a silent mass rewrite of a source
tree is not something a plugin should decide on its own.
