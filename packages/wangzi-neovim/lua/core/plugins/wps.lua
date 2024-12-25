-- @class Wps
local wps = { path = nil, qt_path = nil, ohos_qt_path = nil, loaded = false }

local function config()
    local utils = require("core.utils")
    local Path = require("plenary.path")
    local Job = require("plenary.job")
    wps.path = Path:new(vim.fn.finddir('Coding/..', vim.fn.expand('%:p:h') .. ';'))
    wps.qt_path = wps.path:joinpath("../3rdparty/qt5/source/qtbase/")
    wps.ohos_qt_path = wps.path:joinpath("../3rdparty/qt5/source/qtbase/")

    local coding_dir = wps.path:joinpath("Coding/")
    if coding_dir.exist() then
        require("cmake-tools").select_cwd(tostring(coding_dir))
    end

    vim.api.nvim_create_user_command("CdWps", function()
        vim.cmd.cd(tostring(wps.path))
    end, {})

    vim.api.nvim_create_user_command("CdQt", function()
        vim.cmd.cd(tostring(wps.path:joinpath("../3rdparty/qt5/source/qtbase/")))
    end, {})

    vim.api.nvim_create_user_command("RgQt", function()
        require("trailblazer").new_trail_mark()
        vim.cmd("Telescope live_grep search_dirs=" .. tostring(wps.qt_path))
    end, {})

    vim.api.nvim_create_user_command("RgQtOhos", function()
        require("trailblazer").new_trail_mark()
        vim.cmd("Telescope live_grep search_dirs=" .. tostring(wps.ohos_qt_path))
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
            skip_validation = true,
            detach = true,
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
        utils.argOrCachedInput(opts.args, "target", "target", "", nil, function(target)
            if target == "" then
                require("toggleterm").exec("msbuild ../debug/WPSOffice.sln -m:32")
            else
                require("toggleterm").exec("msbuild ../debug/WPSOffice.sln -m:32 -t:" .. target)
            end
        end)
    end, { nargs = "?" })

    vim.api.nvim_create_user_command("KrepoBuild", function(opts)
        utils.argOrCachedInput(opts.args, "target", "target", "", nil, function(target)
            if target == "" then
                require("toggleterm").exec("krepo build --no-redirect --verbose")
            else
                require("toggleterm").exec("krepo build --no-redirect --verbose -t wps/" .. target)
            end
        end)
    end, { nargs = "?" })

    vim.api.nvim_create_user_command("KrepoCr", function(opts)
        Job:new({ command = "krepo", args = { "cr" } }):start()
    end, { nargs = 0 })

    vim.api.nvim_create_user_command("KrepoPush", function(opts)
        Job:new({ command = "krepo", args = { "push" } }):start()
    end, { nargs = 0 })

    vim.api.nvim_create_user_command("KrepoSync", function(opts)
        Job:new({
            command = "krepo",
            args = { "sync", "--with-sdk", "--stash" },
        }):start()
    end, { nargs = 0 })

    vim.api.nvim_create_user_command("KrepoSync", function(opts)
        Job:new({
            command = "../debug/WPSOffice/auth_plugins/version_common_cn.bat",
        }):start()
    end, { nargs = 0 })

    vim.api.nvim_create_user_command("NotesTree", function(opts)
        vim.cmd [[neotree dir=C:\Users\wps\Documents\Obsidian-work]]
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
