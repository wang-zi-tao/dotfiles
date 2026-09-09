--- Neovim 配置热加载机制
---
--- 为 core 配置模块（core.agent / core.opt / core.cmd / ...）提供免重启热加载：
---   * 手动命令 :LuaReload <module> / :LuaReloadAll [path]
---   * 自动监听：fs_event 监听安装目录 lua/core（可设 vim.g.lua_hotreload=false 关闭）
---
--- 配置文件在 dotfiles-copy 仓库中修改后会被外部进程复制到安装目录，
--- 不触发 BufWritePost，因此用 fs_event 目录监听（参考 core.utils.watch_file 模式）
--- 而非 BufWritePost 事件。
---
--- 状态管理：有状态模块（如 core.agent）应通过 init_module(factory) 创建并托管状态表，
--- 重载时状态表本体不变、仅函数值更新，旧闭包与新函数共享同一张状态表。
---
--- 副作用清理钩子由各模块自身注册（见 register_hook）：模块在自身文件内调用
--- require("core.hotreload").register_hook("core.cmd", { before = function() ... end })
--- 重载该模块前 hotreload 会调用 hook.before 清理副作用（如用户命令/autocmd），
--- 重载成功后调用 hook.after（如有）。
---
--- 已知限制：
---   * 已建立的 dap 订阅监听器回调仍是旧函数闭包，重载 core.agent 后需重新
---     dap_subscribe（重连 dsh 会话）才会使用新的监听回调逻辑；但所有 dap_* 命令类调用立即生效。
---   * core.database 顶层建立 sqlite 连接且被 core.utils 等以 local 缓存引用，
---     重载会重建连接而旧引用不更新，故不进入默认重载列表；手动 M.reload("core.database") 风险自负。
---   * 不处理插件（lazy.nvim / core.plugins.*）的热加载。

--- 持久状态锚点：重载 hotreload 自身时不丢失 states/hooks/fs_event 句柄。
local _G_KEY = "__lua_hotreload_state"
local _P = rawget(_G, _G_KEY)
if _P == nil then
  _P = {
    states = {},         -- module -> 托管状态表（init_module 创建，重载不换）
    hooks = {},          -- module -> HotReloadHooks（各模块 register_hook 注册）
    watch = nil,         -- uv_fs_event 句柄（监听 lua/core 目录）
    watch_dir = nil,     -- 监听的目录绝对路径
    reloading = nil,     -- 重载中的模块名，供 init_module 精确命中
  }
  rawset(_G, _G_KEY, _P)
end

local M = {}
local states = _P.states
local hooks = _P.hooks

--- 默认重载列表（按 core/init.lua 的加载顺序；不含 core.database / core.plugins.*）
local DEFAULT_MODULES = {
  "core.opt",
  "core.map",
  "core.env",
  "core.theme",
  "core.cmd",
  "core.auto",
  "core.agent",
}

--- 从文件路径推断模块名（如 C:/x/lua/core/agent.lua -> core.agent）
---@param path string
---@return string|nil
function M.path_to_module(path)
  if type(path) ~= "string" or path == "" then
    return nil
  end
  local norm = vim.fn.fnamemodify(path, ":p:gs?\\?/?")
  local rel = norm:match("[/\\]lua[/\\](.+)%.lua$")
  if not rel then
    return nil
  end
  return rel:gsub("[/\\]", ".")
end

--- 解析模块文件绝对路径
---@param module string
---@return string|nil
function M.module_path(module)
  local lua_dir = vim.fn.stdpath("config") .. "/lua"
  local path = package.searchpath(module, lua_dir .. "/?.lua")
  if path then
    return vim.fn.fnamemodify(path, ":p")
  end
  -- 兜底：runtimepath 中查找
  local rtp_patterns = vim.tbl_map(function(p)
    return p .. "/lua/?.lua"
  end, vim.split(vim.o.runtimepath, ","))
  path = package.searchpath(module, table.concat(rtp_patterns, ";"))
  if path then
    return vim.fn.fnamemodify(path, ":p")
  end
  return nil
end

--- 注册模块重载钩子（由各模块在自身文件内调用，覆盖式注册）
---@class HotReloadHooks
---@field before? fun(old: table|nil) 重载前清理副作用
---@field after?  fun(new: table)     重载成功后处理
---@param module string
---@param hook HotReloadHooks
function M.register_hook(module, hook)
  hooks[module] = hook
end

--- 创建并托管模块状态表。
--- 模块顶层调用：local M = require("core.hotreload").init_module(function() return { ... } end)
--- 返回的表即模块最终 return 的 M，也就是 require 的返回值（package.loaded[mod]）。
--- 首次加载时 factory() 建表并记录；之后（含重载）直接返回同一张状态表：
--- 表本体不变、仅函数值更新，所以 require 返回值在重载前后是同一个对象，
--- 插件/调用方缓存的模块引用不会失效，且下次调用即走新函数逻辑。
---@generic T
---@param factory? fun(): T
---@return T
function M.init_module(factory)
  local mod = _P.reloading
  if not mod then
    local info = debug.getinfo(2, "S")
    local source = info and info.source or ""
    if source:sub(1, 1) == "@" then
      mod = M.path_to_module(source:sub(2))
    end
  end
  if mod and states[mod] then
    return states[mod]
  end
  local state = factory and factory() or {}
  if mod then
    states[mod] = state
  end
  return state
end

--- 重载单个模块
---@param module string
---@return boolean
function M.reload(module)
  if type(module) ~= "string" or module == "" then
    vim.notify("LuaReload: 模块名不能为空", vim.log.levels.ERROR)
    return false
  end
  local path = M.module_path(module)
  if not path or vim.fn.filereadable(path) ~= 1 then
    vim.notify("LuaReload: 找不到模块文件 " .. module, vim.log.levels.ERROR)
    return false
  end

  local old = package.loaded[module]
  local hook = hooks[module]
  if hook and hook.before then
    pcall(hook.before, old)
  end

  package.loaded[module] = nil
  _P.reloading = module
  local ok, err = pcall(require, module)
  _P.reloading = nil
  if not ok then
    -- 失败回滚：恢复旧模块，既有调用继续用旧代码
    package.loaded[module] = old
    vim.notify("LuaReload: 重载失败 " .. module .. ": " .. tostring(err), vim.log.levels.ERROR)
    return false
  end

  local new = package.loaded[module]
  if hook and hook.after then
    pcall(hook.after, new)
  end
  vim.notify("已重载 " .. module, vim.log.levels.INFO)
  return true
end

--- 重载全部默认模块；newpath 非空时视为单文件路径，只重载对应模块
---@param newpath? string
---@return boolean
function M.reloadall(newpath)
  if newpath and newpath ~= "" then
    local module = M.path_to_module(newpath)
    if not module then
      vim.notify("LuaReloadAll: 无法从路径推断模块名 " .. newpath, vim.log.levels.ERROR)
      return false
    end
    return M.reload(module)
  end

  local failed = {}
  for _, module in ipairs(DEFAULT_MODULES) do
    if not M.reload(module) then
      table.insert(failed, module)
    end
  end
  if #failed > 0 then
    vim.notify("LuaReloadAll: 失败模块: " .. table.concat(failed, ", "), vim.log.levels.ERROR)
    return false
  end
  return true
end

--- fs_event 目录变化回调（参考 core.utils.watch_file 模式：debounce stop/start）。
--- fname 可能是 basename（如 "agent.lua"）或完整路径，先拼目录再推断模块。
---@param err string
---@param fname string
---@param status table
local function on_dir_change(err, fname, status)
  -- debounce：stop/start 重置监听，避免复制过程中的事件风暴
  local w = _P.watch
  if w and not w:is_closing() then
    w:stop()
    w:start(_P.watch_dir, {}, vim.schedule_wrap(on_dir_change))
  end
  if err then
    return
  end

  local module
  if fname and fname ~= "" then
    module = M.path_to_module(fname)
    if not module then
      module = M.path_to_module(_P.watch_dir .. "/" .. fname)
    end
  end
  if not module then
    return
  end
  if module == "core.hotreload" or vim.tbl_contains(DEFAULT_MODULES, module) or hooks[module] then
    M.reload(module)
  end
end

--- 启动自动监听：fs_event 监听安装目录 lua/core
function M.start_watch()
  local dir = vim.fn.stdpath("config") .. "/lua/core"
  if vim.fn.isdirectory(dir) ~= 1 then
    return
  end
  -- 重建句柄（重载 hotreload 自身时旧句柄绑定旧回调，须重建以使用新逻辑）
  if _P.watch and not _P.watch:is_closing() then
    _P.watch:stop()
    _P.watch:close()
  end
  local w = vim.uv.new_fs_event()
  _P.watch = w
  _P.watch_dir = dir
  w:start(dir, {}, vim.schedule_wrap(on_dir_change))
end

--- 停止自动监听
function M.stop_watch()
  if _P.watch and not _P.watch:is_closing() then
    _P.watch:stop()
    _P.watch:close()
    _P.watch = nil
    _P.watch_dir = nil
  end
end

--- 命令注册（幂等，支持重载 hotreload 自身）
pcall(vim.api.nvim_del_user_command, "LuaReload")
pcall(vim.api.nvim_del_user_command, "LuaReloadAll")
vim.api.nvim_create_user_command("LuaReload", function(opts)
  M.reload(opts.args)
end, {
  nargs = 1,
  desc = "Reload module",
})
vim.api.nvim_create_user_command("LuaReloadAll", function(args)
  M.reloadall(args.args)
end, {
  nargs = "?",
  desc = "Reload all modules",
})

-- if vim.g.lua_hotreload ~= false then
--     M.start_watch()
-- end

return M
