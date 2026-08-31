/**
 * Configuration resolution for dsh-neovim.
 *
 * Precedence (highest wins):
 *   1. the Cordis row `config` (what `cordis.patch.yml` supplied)
 *   2. environment / platform defaults (the Neovim RPC address)
 *
 * Validation is fail-loud at load for the static fields: a non-string
 * `luaModule` (or an empty one) aborts the mount, surfaced by the mount audit
 * as an invalid row config. The socket address is validated lazily at connect
 * time because Neovim may legitimately not be running when the host starts.
 */
export const DEFAULT_LUA_MODULE = 'core.agent';
export function resolveConfig(raw = {}) {
    const luaModule = raw.luaModule === undefined || raw.luaModule === null
        ? DEFAULT_LUA_MODULE
        : String(raw.luaModule).trim();
    if (luaModule === '') {
        throw new Error(`dsh-neovim: luaModule must not be empty`);
    }
    const socket = raw.socket === undefined || raw.socket === null
        ? ''
        : String(raw.socket).trim();
    return { socket, luaModule };
}
/**
 * Resolve the Neovim RPC address: explicit config wins, then the environment,
 * then the platform default.
 */
export function resolveSocket(config) {
    if (config.socket)
        return config.socket;
    if (process.env.NVIM_LISTEN_ADDRESS)
        return process.env.NVIM_LISTEN_ADDRESS;
    if (process.env.NVIM)
        return process.env.NVIM;
    if (process.platform === 'win32')
        return '\\\\.\\pipe\\nvim';
    const runtime = process.env.XDG_RUNTIME_DIR || '/tmp';
    return `${runtime}/nvim.${process.env.USER || 'user'}`;
}
