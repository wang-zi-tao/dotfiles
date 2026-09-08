local M = {}
---@type table<string, {exports: Object, unload?: fun(Object)}>
M.reloadable_module = {}

---@generic T
---@param module string
---@param init nil | fun(): T
---@param unload nil | fun(T)
---@return T
function M.module_export(module, init, unload)
  local module_obj = M.reloadable_module[module]
  if module_obj then
    return module_obj.exports
  else
    module_obj = {
      exports = init and init() or {},
      unload = unload,
    }
    M.reloadable_module[module] = module_obj
    return module_obj.exports
  end
end

function M.reload(module)
  local module_obj = M.reloadable_module[module]
  if module_obj and module_obj.unload then
    module_obj.unload(module_obj.exports)
  end
  M.reloadable_module[module] = nil
  package.loaded[module] = nil
  return require(module)
end

---@param newpath string|nil
function M.reloadall(newpath)
  for module in pairs(M.reloadable_module) do
    package.loaded[module] = nil
  end
  if newpath then
    vim.opt.rtp:remove(newpath)
    vim.opt.rtp:append(newpath)
  end
  for module in pairs(M.reloadable_module) do
    require(module)
  end
end

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

return M
