/**
 * Neovim RPC client wrapper.
 *
 * Wraps the `neovim` npm package's msgpack-rpc client with the small surface
 * the tools need: notifications (nvim → client), `command` / `commandOutput`
 * passthrough, Lua execution and expression evaluation via `vim.json.encode`,
 * and a coroutine-backed blocking `luaAsyncEval` driven by the `async_task_finish`
 * notification (the `run_async` bridge in the user's Neovim configuration).
 *
 * Ported from the OpenCode `neovim-api` plugin; the original `getNvimSocket`
 * resolution lives in `config.ts` (`resolveSocket`) so the address is a
 * Cordis row config concern. The singleton cache / retry policy lives in
 * `index.ts`, which owns the host-side lifecycle.
 */
import { attach } from 'neovim';
import { createConnection } from 'node:net';
/**
 * Probe whether anything is listening on the RPC address before attaching.
 *
 * The `neovim` package never installs an `error` listener on its transport
 * socket, so attaching to a dead address raises an uncaught `ENOENT` /
 * `ECONNREFUSED` and leaves `channelId()` pending forever. Probe first, fail
 * fast with a plain `false`, and only attach to a live peer.
 */
export function probeSocket(socket, timeoutMs = 5000) {
    return new Promise((resolve) => {
        const probe = createConnection(socket);
        let timer;
        const done = (ok) => {
            if (timer)
                clearTimeout(timer);
            probe.destroy();
            resolve(ok);
        };
        timer = setTimeout(() => done(false), timeoutMs);
        probe.once('connect', () => done(true));
        probe.once('error', () => done(false));
    });
}
export class Neovim {
    client;
    notifyCallbacks;
    asyncTaskCallbacks;
    nextTaskId = 0;
    _disconnected = false;
    constructor(client) {
        this.client = client;
        this.notifyCallbacks = new Map();
        this.asyncTaskCallbacks = new Map();
        // The underlying package leaves socket errors unhandled; absorb them here
        // (a mid-flight reset surfaces as `disconnect` / a rejected request).
        const transport = client.transport;
        const onSocketError = () => {
            this._disconnected = true;
        };
        transport?.reader?.on('error', onSocketError);
        transport?.writer?.on('error', onSocketError);
        this.client.on('disconnect', () => {
            this._disconnected = true;
            // Settle every in-flight blocking task so no tool call hangs forever
            // on a socket that is gone. The host reconnects on the next tool call.
            for (const [, reject] of this.asyncTaskCallbacks.values()) {
                reject(new Error('nvim disconnected'));
            }
            this.asyncTaskCallbacks.clear();
        });
        this.client.subscribe('async_task_finish');
        this.client.on('notification', (method, args) => {
            if (method === 'async_task_finish') {
                const { succ, ret, error, task_id } = args[0];
                if (this.asyncTaskCallbacks.has(task_id)) {
                    const [resolve, reject] = this.asyncTaskCallbacks.get(task_id);
                    if (succ) {
                        resolve(ret);
                    }
                    else {
                        reject(error);
                    }
                    this.asyncTaskCallbacks.delete(task_id);
                }
            }
            else {
                const emitter = this.notifyCallbacks.get(method);
                if (emitter) {
                    for (const callback of emitter) {
                        callback(args[0]);
                    }
                }
            }
        });
    }
    get disconnected() {
        return this._disconnected;
    }
    async subscribe(method, callback) {
        let emitter = this.notifyCallbacks.get(method);
        if (!emitter) {
            emitter = [];
            this.notifyCallbacks.set(method, emitter);
            await this.client.subscribe(method);
        }
        emitter.push(callback);
    }
    command(command) {
        return this.client.commandOutput(command);
    }
    parseReturn(ret) {
        try {
            return JSON.parse(ret);
        }
        catch (e) {
            throw new Error('invalid JSON: "' + ret + '", error: ' + e);
        }
    }
    encodeLuaArgs(command, args) {
        if (args) {
            const argsExpr = [];
            for (const [key, value] of Object.entries(args)) {
                argsExpr.push(`${key}=${JSON.stringify(value)};`);
            }
            command = `( function() ${argsExpr.join('')} return ${command} end )()`;
        }
        return command;
    }
    async lua(command, args) {
        const script = `lua ${this.encodeLuaArgs(command, args)}`;
        await this.client.command(script);
    }
    async luaEval(command, args) {
        const script = `lua print(vim.json.encode(${this.encodeLuaArgs(command, args)}))`;
        const ret = await this.client.commandOutput(script);
        return this.parseReturn(ret);
    }
    luaAsyncEval(command, luaModule, args) {
        const task_id = this.nextTaskId;
        this.nextTaskId += 1;
        return this.client.channelId.then((channel_id) => {
            const script = `lua require("${luaModule}").run_async(function() return ` +
                `${this.encodeLuaArgs(command, args)} end, ${channel_id}, ${task_id})`;
            return new Promise((resolve, reject) => {
                this.asyncTaskCallbacks.set(task_id, [resolve, reject]);
                this.client.commandOutput(script).catch((error) => {
                    if (this.asyncTaskCallbacks.delete(task_id))
                        reject(error);
                });
            });
        });
    }
    eval(command) {
        return this.client.eval(command);
    }
    async channelId() {
        return await this.client.channelId;
    }
    /** Close the RPC connection and release the underlying socket. */
    close() {
        return this.client.close();
    }
}
/**
 * Attach one client to the given RPC address. Synchronous failures (bad
 * arguments) return null; a peer that is simply not listening surfaces later
 * as the client's `disconnect` event / a rejected `channelId()`.
 */
export function tryConnectNvim(socket) {
    try {
        return new Neovim(attach({ socket }));
    }
    catch {
        return null;
    }
}
