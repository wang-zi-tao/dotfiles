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

import { attach, NeovimClient } from 'neovim'
import { createConnection } from 'node:net'

export type LuaArgs = Record<string, unknown>

/**
 * Probe whether anything is listening on the RPC address before attaching.
 *
 * The `neovim` package never installs an `error` listener on its transport
 * socket, so attaching to a dead address raises an uncaught `ENOENT` /
 * `ECONNREFUSED` and leaves `channelId()` pending forever. Probe first, fail
 * fast with a plain `false`, and only attach to a live peer.
 */
export function probeSocket(socket: string, timeoutMs = 5000): Promise<boolean> {
  return new Promise((resolve) => {
    const probe = createConnection(socket)
    let timer: NodeJS.Timeout | undefined
    const done = (ok: boolean) => {
      if (timer) clearTimeout(timer)
      probe.destroy()
      resolve(ok)
    }
    timer = setTimeout(() => done(false), timeoutMs)
    probe.once('connect', () => done(true))
    probe.once('error', () => done(false))
  })
}

export class Neovim {
  private client: NeovimClient
  private notifyCallbacks: Map<string, Array<(value: any) => void>>
  private asyncTaskCallbacks: Map<number, [(value: any) => void, (error: any) => void]>
  private nextTaskId = 0
  private _disconnected = false

  constructor(client: NeovimClient) {
    this.client = client
    this.notifyCallbacks = new Map()
    this.asyncTaskCallbacks = new Map()

    // The underlying package leaves socket errors unhandled; absorb them here
    // (a mid-flight reset surfaces as `disconnect` / a rejected request).
    const transport = (client as unknown as {
      transport?: { reader?: NodeJS.EventEmitter; writer?: NodeJS.EventEmitter }
    }).transport
    const onSocketError = () => {
      this._disconnected = true
    }
    transport?.reader?.on('error', onSocketError)
    transport?.writer?.on('error', onSocketError)

    this.client.on('disconnect', () => {
      this._disconnected = true
      // Settle every in-flight blocking task so no tool call hangs forever
      // on a socket that is gone. The host reconnects on the next tool call.
      for (const [, reject] of this.asyncTaskCallbacks.values()) {
        reject(new Error('nvim disconnected'))
      }
      this.asyncTaskCallbacks.clear()
    })

    this.client.subscribe('async_task_finish')
    this.client.on('notification', (method: string, args: any[]) => {
      if (method === 'async_task_finish') {
        const { succ, ret, error, task_id } = args[0]
        if (this.asyncTaskCallbacks.has(task_id)) {
          const [resolve, reject] = this.asyncTaskCallbacks.get(task_id)!
          if (succ) {
            resolve(ret)
          } else {
            reject(error)
          }
          this.asyncTaskCallbacks.delete(task_id)
        }
      } else {
        const emitter = this.notifyCallbacks.get(method)
        if (emitter) {
          for (const callback of emitter) {
            callback(args[0])
          }
        }
      }
    })
  }

  get disconnected(): boolean {
    return this._disconnected
  }

  async subscribe(method: string, callback: (...args: any[]) => void): Promise<void> {
    let emitter = this.notifyCallbacks.get(method)
    if (!emitter) {
      emitter = []
      this.notifyCallbacks.set(method, emitter)
      await this.client.subscribe(method)
    }
    emitter.push(callback)
  }

  command(command: string): Promise<unknown> {
    return this.client.commandOutput(command)
  }

  parseReturn(ret: string): unknown {
    try {
      return JSON.parse(ret)
    } catch (e) {
      throw new Error('invalid JSON: "' + ret + '", error: ' + e)
    }
  }

  /**
   * Serialize a JSON value (tool argument) into a Lua expression that
   * reconstructs it, safe to inline into a `lua` command line. Handles null,
   * booleans, finite numbers, strings, arrays and plain objects; nested
   * structures recurse. Throws on values JSON cannot represent.
   */
  jsonToLuaCode(json: any): string {
    if (json === null || json === undefined) return 'nil'
    switch (typeof json) {
      case 'boolean':
        return json ? 'true' : 'false'
      case 'number': {
        if (!Number.isFinite(json)) {
          throw new Error('cannot encode non-finite number as Lua: ' + json)
        }
        return String(json)
      }
      case 'string':
        return this.luaStringLiteral(json)
      case 'object': {
        if (Array.isArray(json)) {
          const items = json.map((item) => this.jsonToLuaCode(item))
          return '{' + items.join(',') + '}'
        }
        const fields: string[] = []
        for (const [key, value] of Object.entries(json)) {
          fields.push(`[${this.luaStringLiteral(key)}]=${this.jsonToLuaCode(value)}`)
        }
        return '{' + fields.join(',') + '}'
      }
      default:
        throw new Error('cannot encode value of type ' + typeof json + ' as Lua')
    }
  }

  /** Quote a string as a double-quoted Lua literal, escaping control chars. */
  private luaStringLiteral(value: string): string {
    let out = '"'
    for (const char of value) {
      const code = char.codePointAt(0)!
      switch (char) {
        case '\\':
          out += '\\\\'
          break
        case '"':
          out += '\\"'
          break
        case '\n':
          out += '\\n'
          break
        case '\r':
          out += '\\r'
          break
        case '\t':
          out += '\\t'
          break
        default:
          if (code < 0x20 || code === 0x7f) {
            out += '\\' + code.toString().padStart(3, '0')
          } else {
            out += char
          }
      }
    }
    return out + '"'
  }

  private encodeLuaArgs(command: string, args?: LuaArgs): string {
    if (args) {
      const argsExpr: string[] = []
      for (const [key, value] of Object.entries(args)) {
        argsExpr.push(`${key}=${this.jsonToLuaCode(value)};`)
      }
      command = `( function() ${argsExpr.join('')} return ${command} end )()`
    }
    return command
  }

  async lua(command: string, args?: LuaArgs): Promise<void> {
    const script = `lua ${this.encodeLuaArgs(command, args)}`
    await this.client.command(script)
  }

  async luaEval(command: string, args?: LuaArgs): Promise<unknown> {
    const script = `lua print(vim.json.encode(${this.encodeLuaArgs(command, args)}))`
    const ret = await this.client.commandOutput(script)
    return this.parseReturn(ret)
  }

  luaAsyncEval(command: string, luaModule: string, args?: LuaArgs): Promise<unknown> {
    const task_id = this.nextTaskId
    this.nextTaskId += 1

    return this.client.channelId.then((channel_id) => {
      const script =
        `require("${luaModule}").run_async(function() return ` +
        `${this.encodeLuaArgs(command, args)} end, ${channel_id}, ${task_id})`
      return new Promise((resolve, reject) => {
        this.asyncTaskCallbacks.set(task_id, [resolve, reject])
        this.client.executeLua(script).catch((error) => {
          if (this.asyncTaskCallbacks.delete(task_id)) reject(error)
        })
      })
    })
  }

  eval(command: string): Promise<unknown> {
    return this.client.eval(command)
  }

  async channelId(): Promise<number> {
    return await this.client.channelId
  }

  /** Close the RPC connection and release the underlying socket. */
  close(): Promise<void> {
    return this.client.close()
  }
}

/**
 * Attach one client to the given RPC address. Synchronous failures (bad
 * arguments) return null; a peer that is simply not listening surfaces later
 * as the client's `disconnect` event / a rejected `channelId()`.
 */
export function tryConnectNvim(socket: string): Neovim | null {
  try {
    return new Neovim(attach({ socket }))
  } catch {
    return null
  }
}
