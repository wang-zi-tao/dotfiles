local awful = require("awful")
local filesystem = require("gears.filesystem")
local config_dir = filesystem.get_configuration_dir()

local function run_once_pgrep(cmd)
    local findme = cmd
    local firstspace = cmd:find(" ")
    if firstspace then
        findme = cmd:sub(0, firstspace - 1)
    end
    awful.spawn.easy_async_with_shell(string.format("pgrep -u $USER -x %s > /dev/null || (%s)", findme, cmd), function()

    end)
end

local function run_once_ps(findme, cmd)
    awful.spawn.easy_async_with_shell(string.format("procs '%s'|wc -l", findme), function(stdout)
        if tonumber(stdout) ~= 2 then
            awful.spawn(cmd, false)
        end
    end)
end

local function run_once_grep(command)
    awful.spawn.easy_async_with_shell(string.format("ps aux | grep '%s' | grep -v 'grep'", command), function(stdout)
        if stdout == "" or stdout == nil then
            awful.spawn(command, false)
        end
    end)
end

