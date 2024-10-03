-- @class Wps
local wps = { path = nil, loaded = false }

local function config()
    local utils = require("core.utils")
    local Path = require("plenary.path")
    local Job = require("plenary.job")
    wps.path = Path:new(vim.fn.finddir('Coding/..', vim.fn.expand('%:p:h') .. ';'))

    vim.api.nvim_create_user_command("CdWps", function()
        vim.cmd.cd(tostring(wps.path))
    end, {})

    vim.api.nvim_create_user_command("CdQt", function()
        vim.cmd.cd(tostring(wps.path:joinpath("../3rdparty/qt5/source/qtbase/")))
    end, {})

    vim.api.nvim_create_user_command("CdDebug", function()
        vim.cmd.cd(tostring(wps.path:joinpath("../debug")))
    end, {})

    vim.api.nvim_create_user_command("CdOhosTemplate", function()
        vim.cmd.cd(tostring(wps.path:joinpath("../ohos_apptemplate/")))
    end, {})

    vim.api.nvim_create_user_command("CdRelease", function()
        vim.cmd.cd(tostring(wps.path:joinpath("../release")))
    end, {})

    local function debug(module)
        Job:new({
            command = "C:/Users/wps/AppData/Local/Microsoft/WindowsApps/WinDbgX.exe",
            args = {
                -- "-WF", tostring(wps.path:joinpath("../windbg.debugtarget")),
                "-o",
                tostring(wps.path:joinpath("../debug/WPSOffice/office6/", module .. ".exe"))
            },
            skip_validation = true
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

    vim.api.nvim_create_user_command("Msbuild", function(opts)
        utils.argOrCachedInput(opts.args, "target", "target", "", "", function(target)
            if target == "" then
                require("toggleterm").exec("msbuild ../debug/WPSOffice.sln -m:32")
            else
                require("toggleterm").exec("msbuild ../debug/WPSOffice.sln -m:32 -t:" .. target)
            end
        end)
    end, { nargs = "?" })

    vim.api.nvim_create_user_command("KrepoBuild", function(opts)
        utils.argOrCachedInput(opts.args, "target", "target", "", "", function(target)
            if target == "" then
                require("toggleterm").exec("krepo build --no-redirect --verbose -t wps/" .. target)
            else
                require("toggleterm").exec("krepo build --no-redirect --verbose/")
            end
        end)
    end, { nargs = "?" })
    vim.api.nvim_create_user_command("KrepoCr", function(opts)
        require("plenary.job"):new({ command = "krepo", args = { "cr" } }):start()
    end, { nargs = 0 })
    vim.api.nvim_create_user_command("KrepoPush", function(opts)
        require("plenary.job"):new({ command = "krepo", args = { "push" } }):start()
    end, { nargs = 0 })
    vim.api.nvim_create_user_command("KrepoSync", function(opts)
        require("plenary.job"):new({
            command = "krepo",
            args = { "sync", "--with-sdk", "--stash" },
        }):start()
    end, { nargs = 0 })
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
