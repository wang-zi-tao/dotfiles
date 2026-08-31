/**
 * LSP root-directory discovery.
 *
 * The LSP root is not necessarily the session working directory, especially
 * in a mixed-language tree or when a query targets a file deep inside a
 * sub-project (the WPS case: `D:\branch-master\wpsmain` holds
 * `compile_commands.json`, but a C++ file can sit many directories down).
 *
 * Algorithm, from a start file path (or the session cwd fallback):
 *   1. walk upward, directory by directory;
 *   2. the first directory containing any of the server's `rootMarkers` wins;
 *   3. if no marker is found, the nearest `.git`/`.hg` ancestor is the
 *      fallback root;
 *   4. otherwise the start directory itself is returned.
 *
 * Results are cached per `serverId + startDir`; the cache is invalidated by
 * the `/lsp restart` command (which clears the registry's instance cache and
 * this map together).
 */
import { existsSync } from 'node:fs';
import { dirname, join, parse } from 'node:path';
function isWindowsDriveRoot(dir) {
    const parsed = parse(dir);
    return parsed.root === dir;
}
/**
 * Find the first existing marker among `markers` directly inside `dir`.
 */
function findMarker(dir, markers) {
    for (const marker of markers) {
        const candidate = join(dir, marker);
        if (existsSync(candidate))
            return candidate;
    }
    return null;
}
/**
 * Walk up from `startDir` looking for a directory containing any root marker,
 * or the nearest VCS ancestor. Returns the resolved root directory (absolute).
 */
export function findRoot(startDir, spec) {
    let current = startDir;
    let vcsRoot = null;
    for (;;) {
        if (spec.rootMarkers.length > 0) {
            const marker = findMarker(current, spec.rootMarkers);
            if (marker)
                return current;
        }
        if (vcsRoot === null) {
            if (existsSync(join(current, '.git')))
                vcsRoot = current;
            else if (existsSync(join(current, '.hg')))
                vcsRoot = current;
        }
        const parent = dirname(current);
        if (parent === current)
            break; // reached the filesystem root
        current = parent;
    }
    // No project marker: prefer the VCS ancestor, else the start directory.
    return vcsRoot ?? startDir;
}
/**
 * A root cache keyed by `serverId + startDir`.
 */
export class RootResolver {
    cache = new Map();
    resolve(startDir, spec) {
        const key = `${spec.id}\u0000${startDir}`;
        const cached = this.cache.get(key);
        if (cached)
            return cached;
        const root = findRoot(startDir, spec);
        this.cache.set(key, root);
        return root;
    }
    clear() {
        this.cache.clear();
    }
}
