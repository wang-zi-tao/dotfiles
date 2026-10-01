#!/usr/bin/env node
/**
 * prune-deps.mjs — shrink a built pnpm workspace down to its production closure.
 *
 * Why: a pnpm workspace install materialises *everything* the monorepo declares
 * — every project's devDependencies (typescript, vitest, playwright, oxlint,
 * electron-winstaller, …), the toolchain, browser-only libraries that are only
 * ever bundled into a front-end build, and repos that are not part of the
 * shipped product at all (the optional Codex / Claude Code subagent providers).
 * None of that is reachable from the process the package actually runs, but all
 * of it lands in the store output because the tree is copied wholesale.
 *
 * What it does, in order:
 *   1. Reads every workspace manifest (packages/<group>/<name>, vendor/*,
 *      apps/*, native/system/packages/*, benchmarks) and walks the *production*
 *      dependency graph (dependencies + optionalDependencies + peerDependencies;
 *      devDependencies are deliberately not followed), resolving each name the
 *      way Node does (nearest node_modules, walking up).
 *   2. Deletes every node_modules/.pnpm entry the walk did not reach, then drops
 *      the now-dangling symlinks that pointed at them (including .bin shims).
 *   3. Deletes the workspace packages whose *name* is excluded (--exclude), e.g.
 *      the optional subagent providers, so neither they nor their SDKs ship.
 *   4. Removes build caches (*.tsbuildinfo) and native payloads built for a
 *      platform other than --keep-arch (e.g. node-pty's win32/darwin prebuilds,
 *      committed darwin/arm64 addons under native/).
 *   5. Optionally strips tests/, source maps, and repo docs (--strip-*).
 *
 * Usage:
 *   node prune-deps.mjs <repoRoot> [--exclude NAME]... [--keep-arch linux-x64]
 *        [--strip-tests] [--strip-maps] [--strip-docs]
 *
 * Exit code is non-zero only on a real error; the walk is tolerant of missing
 * manifests, broken symlinks and unreadable files (it is post-processing a
 * tree that a package manager already produced).
 */
import fs from "node:fs";
import path from "node:path";
import process from "node:process";

const opts = {
  exclude: new Set(),
  keep: [],
  keepArch: null,
  stripTests: false,
  stripMaps: false,
  stripDocs: false,
};

const positional = [];
const argv = process.argv.slice(2);
for (let i = 0; i < argv.length; i += 1) {
  const arg = argv[i];
  const [flag, inline] = arg.startsWith("--") && arg.includes("=") ? arg.split(/=(.*)/s, 2) : [arg, null];
  const value = () => inline ?? argv[++i];
  switch (flag) {
    case "--exclude":
      opts.exclude.add(value());
      break;
    case "--keep":
      opts.keep.push(value());
      break;
    case "--keep-arch":
      opts.keepArch = value();
      break;
    case "--strip-tests":
      opts.stripTests = true;
      break;
    case "--strip-maps":
      opts.stripMaps = true;
      break;
    case "--strip-docs":
      opts.stripDocs = true;
      break;
    default:
      positional.push(arg);
  }
}

const root = path.resolve(positional[0] ?? process.cwd());
const NM = path.join(root, "node_modules");
const STORE = path.join(NM, ".pnpm");

if (!fs.existsSync(root) || !fs.existsSync(STORE)) {
  // Nothing pnpm-shaped here: the recipe changed upstream. Do not fail the build.
  console.log(`prune-deps: no pnpm store under ${root}; nothing to prune`);
  process.exit(0);
}

const readJson = (file) => {
  try {
    return JSON.parse(fs.readFileSync(file, "utf8"));
  } catch {
    return null;
  }
};

/** Remove a file/dir, fixing up read-only (store-derived) permissions first. */
function forceRemove(target) {
  let st;
  try {
    st = fs.lstatSync(target);
  } catch {
    return 0;
  }
  if (st.isSymbolicLink()) {
    tryChmodParent(target);
    fs.unlinkSync(target);
    return 1;
  }
  if (st.isDirectory()) {
    let n = 0;
    for (const entry of fs.readdirSync(target)) n += forceRemove(path.join(target, entry));
    try {
      fs.chmodSync(target, 0o755);
    } catch {
      /* best effort */
    }
    tryChmodParent(target);
    fs.rmdirSync(target);
    return n;
  }
  try {
    fs.chmodSync(target, 0o644);
  } catch {
    /* best effort */
  }
  tryChmodParent(target);
  fs.unlinkSync(target);
  return 1;
}

/**
 * Removing an entry needs write permission on the *directory*, not the entry:
 * a tree copied out of the store has 0555 directories, so unlink/rmdir would
 * fail with EACCES even after chmod-ing the file itself. Make the parent
 * writable (and the grandparents, for nested removals) before failing.
 */
function tryChmodParent(target) {
  try {
    const parent = path.dirname(target);
    fs.chmodSync(parent, 0o755);
    const grandparent = path.dirname(parent);
    if (grandparent !== parent) fs.chmodSync(grandparent, 0o755);
  } catch {
    /* best effort */
  }
}

const SKIP_DIRS = new Set(["node_modules", "tests", "test", "fixtures", "__fixtures__", "lib", "dist", "src", ".git"]);

/** Every directory in the workspace that is itself a package. */
function collectManifests() {
  const found = new Set();
  const roots = ["packages", "vendor", "apps", "native/system/packages", "benchmarks"]
    .map((rel) => path.join(root, rel))
    .filter((dir) => fs.existsSync(dir));
  const walk = (dir, depth) => {
    if (depth > 3) return;
    let entries;
    try {
      entries = fs.readdirSync(dir, { withFileTypes: true });
    } catch {
      return;
    }
    if (entries.some((e) => e.isFile() && e.name === "package.json")) {
      found.add(dir);
      return;
    }
    for (const entry of entries) {
      if (!entry.isDirectory() || entry.name.startsWith(".") || SKIP_DIRS.has(entry.name)) continue;
      walk(path.join(dir, entry.name), depth + 1);
    }
  };
  for (const dir of roots) walk(dir, 0);
  return [...found].sort();
}

/** Node-style resolution: nearest node_modules, then walk up. */
function resolveDep(name, from) {
  let dir = from;
  for (;;) {
    const candidate = path.join(dir, "node_modules", name);
    if (fs.existsSync(candidate)) {
      try {
        return fs.realpathSync(candidate);
      } catch {
        return null;
      }
    }
    const up = path.dirname(dir);
    if (up === dir) return null;
    dir = up;
  }
}

function dirSize(target) {
  let total = 0;
  const stack = [target];
  while (stack.length) {
    const current = stack.pop();
    let entries;
    try {
      entries = fs.readdirSync(current, { withFileTypes: true });
    } catch {
      continue;
    }
    for (const entry of entries) {
      const child = path.join(current, entry.name);
      if (entry.isDirectory()) stack.push(child);
      else {
        try {
          total += fs.lstatSync(child).size;
        } catch {
          /* ignore */
        }
      }
    }
  }
  return total;
}

const mb = (bytes) => `${(bytes / 1024 / 1024).toFixed(1)} MB`;

// ---------------------------------------------------------------- closure ---
const manifests = collectManifests();
const excludedDirs = [];
const queue = [];
for (const dir of manifests) {
  const name = readJson(path.join(dir, "package.json"))?.name;
  if (name && opts.exclude.has(name)) {
    excludedDirs.push(dir);
    continue;
  }
  try {
    queue.push(fs.realpathSync(dir));
  } catch {
    /* ignore */
  }
}

const seen = new Set();
const needed = new Set();
const drainQueue = () => {
  while (queue.length) {
    const dir = queue.pop();
    if (seen.has(dir)) continue;
    seen.add(dir);
    const rel = path.relative(NM, dir);
    if (rel === ".pnpm" || rel.startsWith(`.pnpm${path.sep}`)) {
      needed.add(rel.split(path.sep)[1]);
    }
    const manifest = readJson(path.join(dir, "package.json"));
    if (!manifest) continue;
    const deps = {
      ...(manifest.peerDependencies ?? {}),
      ...(manifest.optionalDependencies ?? {}),
      ...(manifest.dependencies ?? {}),
    };
    for (const name of Object.keys(deps)) {
      if (opts.exclude.has(name)) continue;
      const target = resolveDep(name, dir);
      if (target) queue.push(target);
    }
  }
};
drainQueue();

// --keep <name>: packages that must survive even though nothing in the
// production closure reaches them. This is the escape hatch for a toolchain that
// a *running* process invokes by bare specifier while upstream declares it as a
// devDependency of the package that shells into it (e.g. the vendored HMR plugin
// depends on esbuild at build time but bundles client code with it at runtime).
// Keeping a name also keeps that package's own production dependencies.
const matchesKeep = (name) =>
  opts.keep.some((pattern) => name === pattern || name.startsWith(`${pattern}/`) || name.startsWith(`${pattern}-`));

/** The package a `.pnpm/<key>` entry holds: its real (non-symlink) directory. */
const packageOfEntry = (entry) => {
  const nm = path.join(STORE, entry, "node_modules");
  let names;
  try {
    names = fs.readdirSync(nm);
  } catch {
    return null;
  }
  const candidates = [];
  for (const name of names) {
    const child = path.join(nm, name);
    try {
      if (!fs.lstatSync(child).isDirectory() || fs.lstatSync(child).isSymbolicLink()) continue;
    } catch {
      continue;
    }
    if (name.startsWith("@")) {
      // A scope directory is real; the package inside it is a symlink to another
      // store entry, which is a *dependency*, not this entry's own package.
      for (const scoped of fs.readdirSync(child)) {
        const scopedPath = path.join(child, scoped);
        try {
          if (fs.lstatSync(scopedPath).isSymbolicLink()) continue;
        } catch {
          continue;
        }
        candidates.push(scopedPath);
      }
    } else {
      candidates.push(child);
    }
  }
  for (const dir of candidates) {
    const manifest = readJson(path.join(dir, "package.json"));
    if (manifest?.name) return { dir, name: manifest.name };
  }
  return null;
};

if (opts.keep.length) {
  for (;;) {
    const pending = fs
      .readdirSync(STORE)
      .filter((entry) => isPackageEntry(entry) && !needed.has(entry))
      .map((entry) => ({ entry, pkg: packageOfEntry(entry) }))
      .filter(({ pkg }) => pkg && matchesKeep(pkg.name));
    if (!pending.length) break;
    for (const { entry, pkg } of pending) {
      console.log(`prune-deps: --keep ${pkg.name} (unreachable from the closure, retained on request)`);
      needed.add(entry);
      try {
        queue.push(fs.realpathSync(pkg.dir));
      } catch {
        /* ignore */
      }
    }
    drainQueue();
  }
}

// ---------------------------------------------------------------- pruning ---
// Only real package entries may be pruned. `.pnpm/node_modules` is pnpm's
// hoisted-link directory (Node resolution for packages that rely on it — e.g.
// node-addon-native-custom-loader resolving its platform addon — goes through
// it) and `.pnpm/lock.yaml` is the store's own lockfile copy; neither is an
// entry, and deleting either breaks resolution at runtime.
// (Declared as a function so it is hoisted: the --keep pass below runs first.)
function isPackageEntry(name) {
  if (name.startsWith(".") || name === "node_modules") return false;
  try {
    return fs.statSync(path.join(STORE, name)).isDirectory();
  } catch {
    return false;
  }
}
const storeEntries = fs.readdirSync(STORE);
const dropped = storeEntries.filter((entry) => isPackageEntry(entry) && !needed.has(entry));
const droppedBytes = dropped.reduce((sum, entry) => sum + dirSize(path.join(STORE, entry)), 0);

for (const entry of dropped) forceRemove(path.join(STORE, entry));

let excludedCount = 0;
for (const dir of excludedDirs) {
  excludedCount += forceRemove(dir);
  console.log(`prune-deps: dropped workspace package ${path.relative(root, dir)}`);
}

console.log(
  `prune-deps: .pnpm entries kept ${needed.size}/${storeEntries.length} → freed ${mb(droppedBytes)}`,
);

// Dangling links: every node_modules subtree in the repo (the .pnpm removals
// above invalidate the hoisted symlinks and .bin shims that pointed at dev-only
// tools, both at the root and inside each workspace package).
let dangling = 0;
const sweepNodeModules = (dir, insideNodeModules) => {
  const here = insideNodeModules || path.basename(dir) === "node_modules";
  let entries;
  try {
    entries = fs.readdirSync(dir, { withFileTypes: true });
  } catch {
    return;
  }
  for (const entry of entries) {
    const child = path.join(dir, entry.name);
    if (entry.isSymbolicLink()) {
      if (here && !fs.existsSync(child)) {
        try {
          forceRemove(child);
          dangling += 1;
        } catch {
          /* ignore */
        }
      }
      continue;
    }
    if (entry.isDirectory() && entry.name !== ".git") sweepNodeModules(child, here);
  }
};
sweepNodeModules(root, false);
console.log(`prune-deps: removed ${dangling} dangling symlinks (dev-only links, .bin shims)`);

// ------------------------------------------------------------ build cache ---
let cacheBytes = 0;
const stripTsbuildinfo = (dir) => {
  let entries;
  try {
    entries = fs.readdirSync(dir, { withFileTypes: true });
  } catch {
    return;
  }
  for (const entry of entries) {
    const child = path.join(dir, entry.name);
    if (entry.isDirectory()) stripTsbuildinfo(child);
    else if (entry.name.endsWith(".tsbuildinfo")) {
      let size = 0;
      try {
        size = fs.lstatSync(child).size;
      } catch {
        /* ignore */
      }
      try {
        forceRemove(child);
      } catch {
        continue;
      }
      cacheBytes += size;
    }
  }
};
stripTsbuildinfo(root);
console.log(`prune-deps: removed *.tsbuildinfo build caches → freed ${mb(cacheBytes)}`);

// ------------------------------------------------ foreign-arch native code ---
const FOREIGN = ["win32", "windows", "darwin", "macos", "freebsd", "openbsd", "android", "sunos", "ios"];
const NATIVE_EXT = new Set([".node", ".dll", ".dylib", ".exe", ".so", ".pdb", ".lib", ".a", ".wasm"]);
const keepArch = opts.keepArch;
let foreignBytes = 0;
let foreignFiles = 0;
const trimmedDirs = new Set();

const isForeign = (rel) => {
  const segments = rel.split(path.sep).map((s) => s.toLowerCase());
  if (keepArch && segments.includes(keepArch.toLowerCase())) return false;
  // A linux payload is never addressed under a foreign token unless the token is
  // part of the package's own (target) name, e.g. `sharp-win32-x64` — in which
  // case the path simply is not the one we run.
  return segments.some((s) => FOREIGN.some((f) => s === f || s.startsWith(`${f}-`) || s.startsWith(`${f}_`)));
};

const stripForeign = (dir, pathFromRoot) => {
  let entries;
  try {
    entries = fs.readdirSync(dir, { withFileTypes: true });
  } catch {
    return;
  }
  for (const entry of entries) {
    const child = path.join(dir, entry.name);
    const rel = path.join(pathFromRoot, entry.name);
    if (entry.isDirectory()) {
      stripForeign(child, rel);
      continue;
    }
    if (entry.isSymbolicLink()) continue;
    if (NATIVE_EXT.has(path.extname(entry.name).toLowerCase()) && isForeign(rel)) {
      const size = (() => {
        try {
          return fs.lstatSync(child).size;
        } catch {
          return 0;
        }
      })();
      if (forceRemove(child)) {
        foreignBytes += size;
        foreignFiles += 1;
        trimmedDirs.add(path.dirname(rel));
      }
    }
  }
};
stripForeign(root, "");
// The emptied payload directories are dead weight too; only prune ones whose own
// path still carries the foreign token, so unrelated empty dirs survive.
for (const rel of [...trimmedDirs].sort((a, b) => b.length - a.length)) {
  let dir = path.join(root, rel);
  while (rel !== "" && dir.startsWith(root)) {
    const name = path.basename(dir);
    if (!isForeign(name) && !trimmedDirs.has(path.relative(root, dir))) break;
    let empty = true;
    try {
      empty = fs.readdirSync(dir).length === 0;
    } catch {
      empty = false;
    }
    if (!empty) break;
    try {
      forceRemove(dir);
    } catch {
      break;
    }
    dir = path.dirname(dir);
  }
}
console.log(
  `prune-deps: removed ${foreignFiles} non-${keepArch ?? "host"} native payloads → freed ${mb(foreignBytes)}`,
);

// ----------------------------------------------------------- optional extras --
const walkTree = (dir, visitDir, visitFile) => {
  let entries;
  try {
    entries = fs.readdirSync(dir, { withFileTypes: true });
  } catch {
    return;
  }
  for (const entry of entries) {
    const child = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      if (entry.name === ".git") continue;
      const descend = visitDir?.(child);
      if (descend !== false) walkTree(child, visitDir, visitFile);
    } else if (!entry.isSymbolicLink()) {
      visitFile?.(child);
    }
  }
};

const fileSize = (file) => {
  try {
    return fs.lstatSync(file).size;
  } catch {
    return 0;
  }
};

/** Delete directories whose basename is in `names`, anywhere except node_modules. */
const stripDirs = (names, label) => {
  const wanted = new Set(names);
  const nativeRoot = path.join(root, "native") + path.sep;
  let bytes = 0;
  let count = 0;
  walkTree(
    root,
    (dir) => {
      if (path.basename(dir) === "node_modules") return false;
      // native/ owns its own tests and fixtures beside the addon it tests; leave it alone.
      if (dir.startsWith(nativeRoot)) return false;
      if (!wanted.has(path.basename(dir))) return true;
      bytes += dirSize(dir);
      count += 1;
      forceRemove(dir);
      return false;
    },
    null,
  );
  console.log(`prune-deps: stripped ${label} → freed ${mb(bytes)} (${count} dirs)`);
};

const stripFileSuffixes = (suffixes, label) => {
  let bytes = 0;
  let count = 0;
  walkTree(root, null, (file) => {
    if (!suffixes.some((suffix) => file.endsWith(suffix))) return;
    const size = fileSize(file);
    try {
      forceRemove(file);
    } catch {
      return;
    }
    bytes += size;
    count += 1;
  });
  console.log(`prune-deps: stripped ${label} → freed ${mb(bytes)} (${count} files)`);
};

if (opts.stripTests) stripDirs(["tests", "test", "__tests__"], "tests/");
if (opts.stripMaps) stripFileSuffixes([".map"], "source maps");
if (opts.stripDocs) {
  const docs = ["docs", "website", "snapshots", "benchmarks"]
    .map((rel) => path.join(root, rel))
    .filter((dir) => fs.existsSync(dir));
  const bytes = docs.reduce((sum, dir) => sum + dirSize(dir), 0);
  for (const dir of docs) forceRemove(dir);
  console.log(`prune-deps: stripped ${docs.map((d) => path.relative(root, d)).join(", ")} → freed ${mb(bytes)}`);
}

console.log(
  `prune-deps: excluded ${excludedCount} files under ${excludedDirs.length} provider package(s): ${[...opts.exclude].join(", ") || "-"}`,
);
