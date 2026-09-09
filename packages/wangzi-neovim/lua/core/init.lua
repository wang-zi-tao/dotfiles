local function load(module)
    local success, error = pcall(require, module)
    if not success then
        require("notify")("Failed to load " .. module .. ": " .. error, vim.log.levels.ERROR)
    end
end

vim.g.lua_hotreload = false

load("core.opt")
load("core.map")
load("core.env")
load("core.auto")
load("core.cmd")
load("core.theme")
load("core.database")
load("core.agent")
load("core.plugins.wps")
load("core.hotreload")
