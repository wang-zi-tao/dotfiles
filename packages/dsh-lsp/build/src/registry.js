/**
 * Server registry: extension→server routing and instance lifecycle.
 *
 * Extension ownership is exclusive (enforced at config load), so a query
 * selects its server deterministically by file extension and never asks the
 * model to choose a provider. Each server has at most one live client;
 * `stop`/`restart` rebuild the client, and an unexpectedly exited client is
 * dropped so the next query lazily respawns it.
 */
import { extname, isAbsolute, resolve } from 'node:path';
import { LspClient } from './client.js';
import { RootResolver } from './root.js';
function extensionOf(filePath) {
    const ext = extname(filePath);
    return ext.startsWith('.') ? ext.slice(1).toLowerCase() : ext.toLowerCase();
}
export class ServerRegistry {
    config;
    subprocess;
    logger;
    roots = new RootResolver();
    byExtension = new Map();
    byId = new Map();
    clients = new Map();
    constructor(config, subprocess, logger) {
        this.config = config;
        this.subprocess = subprocess;
        this.logger = logger;
        for (const spec of config.servers) {
            this.byId.set(spec.id, spec);
            for (const ext of spec.extensions) {
                this.byExtension.set(ext, spec);
            }
        }
    }
    get servers() {
        return this.config.servers;
    }
    serverForFile(filePath) {
        const ext = extensionOf(filePath);
        const spec = this.byExtension.get(ext);
        if (!spec) {
            throw new Error(`no LSP server for extension '.${ext}' (file: ${filePath})`);
        }
        return spec;
    }
    startDirFor(filePath, cwd) {
        if (isAbsolute(filePath))
            return filePath;
        const base = cwd ? resolve(cwd) : process.cwd();
        return resolve(base, filePath);
    }
    async getClient(spec, startDir, signal) {
        let client = this.clients.get(spec.id);
        if (client && client.state === 'running')
            return client;
        if (client && client.state === 'starting') {
            // A concurrent query is mid-start; wait briefly for it to settle.
            await new Promise(r => setTimeout(r, 50));
            client = this.clients.get(spec.id);
            if (client && client.state === 'running')
                return client;
        }
        const root = this.roots.resolve(startDir, spec);
        const fresh = new LspClient(spec, this.subprocess, this.logger, {}, this.config.logDir);
        this.clients.set(spec.id, fresh);
        await fresh.start(root, signal);
        return fresh;
    }
    /** Resolve the server + client for a file, starting it lazily if needed. */
    async resolve(filePath, cwd, signal) {
        const spec = this.serverForFile(filePath);
        const startDir = this.startDirFor(filePath, cwd);
        const client = await this.getClient(spec, startDir, signal);
        return { spec, client, root: client.root ?? startDir };
    }
    /** Resolve a server by id for `/lsp start <id>` (no file to route by). */
    serverById(id) {
        return this.byId.get(id);
    }
    async startById(id, cwd) {
        const spec = this.byId.get(id);
        if (!spec)
            throw new Error(`unknown server id '${id}'`);
        const existing = this.clients.get(id);
        if (existing && existing.state === 'running')
            return;
        const startDir = cwd ? resolve(cwd) : process.cwd();
        const root = this.roots.resolve(startDir, spec);
        const fresh = new LspClient(spec, this.subprocess, this.logger, {}, this.config.logDir);
        this.clients.set(id, fresh);
        await fresh.start(root);
    }
    async stopById(id) {
        const client = this.clients.get(id);
        if (client) {
            await client.stop();
            this.clients.delete(id);
        }
    }
    async stopAll() {
        const ids = [...this.clients.keys()];
        await Promise.all(ids.map(id => this.stopById(id)));
    }
    /** Rebuild every running client (used by `/lsp restart`). */
    async restartAll(cwd) {
        const ids = [...this.clients.keys()];
        for (const id of ids) {
            await this.stopById(id);
        }
        this.roots.clear();
    }
    status() {
        const out = [];
        for (const spec of this.config.servers) {
            const client = this.clients.get(spec.id);
            out.push({
                id: spec.id,
                languageId: spec.languageId,
                extensions: [...spec.extensions],
                state: client ? client.state : 'stopped',
                root: client ? client.root : null,
                pid: client ? client.pid : null,
                error: client ? client.error : null,
            });
        }
        return out;
    }
}
