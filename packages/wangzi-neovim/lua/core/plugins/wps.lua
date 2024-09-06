-- @class Wps
local wps = { path = nil, loaded = false }

local function config()
    local Path = require("plenary.path")
    local Job = require("plenary.job")
    wps.path = Path:new(vim.fn.finddir('Coding/..', vim.fn.expand('%:p:h') .. ';'))

    vim.api.nvim_create_user_command("CdWps", function()
        local path = Path:new(wps.path)
        vim.cmd.cd(tostring(wps.path))
    end, {})

    vim.api.nvim_create_user_command("CdQt", function()
        local path = Path:new(wps.path)
        vim.cmd.cd(tostring(wps.path:joinpath("../3rdparty/qt5/source/qtbase/")))
    end, {})

    vim.api.nvim_create_user_command("CdDebug", function()
        local path = Path:new(wps.path)
        vim.cmd.cd(tostring(wps.path:joinpath("../debug")))
    end, {})

    vim.api.nvim_create_user_command("CdOhosTemplate", function()
        local path = Path:new(wps.path)
        vim.cmd.cd(tostring(wps.path:joinpath("../ohos_apptemplate/")))
    end, {})

    vim.api.nvim_create_user_command("CdRelease", function()
        local path = Path:new(wps.path)
        vim.cmd.cd(tostring(wps.path:joinpath("../release")))
    end, {})

    local function debug(module)
        Job:new({
            command = "C:/Users/wps/AppData/Local/Microsoft/WindowsApps/WinDbgX.exe",
            args = { "-o", tostring(wps.path:joinpath("../debug/WPSOffice/office6/", module .. ".exe")) }
        }):start()
    end

    vim.api.nvim_create_user_command("DebugWps", function()
        debug("wps")
    end, {})

    vim.api.nvim_create_user_command("DebugEt", function()
        debug("et")
    end, {})

    vim.api.nvim_create_user_command("DebugWpp", function()
        debug("wpp")
    end, {})

    vim.api.nvim_create_user_command("DebugPdf", function()
        debug("wpspdf")
    end, {})

    vim.api.nvim_create_user_command("WinDbg", function()
        Job:new({
            command = "C:/Users/wps/AppData/Local/Microsoft/WindowsApps/WinDbgX.exe",
            args = {}
        }):start()
    end, {})
end

local M = {}
setmetatable(M, {
    __index = function(o, k)
        if not wps.loaded then
            vim.cmd.Lazy("load", "wps")
            wps.loaded = true
        end
        return wps[k]
    end
})

return {
    {
        dir = vim.fn.stdpath("config") .. "/lua/core/plugins",
        name = "wps",
        event = "VeryLazy",
        config = config,
    }
}
