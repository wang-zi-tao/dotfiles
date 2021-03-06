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
  awful.spawn.easy_async_with_shell(string.format("ps -C %s|wc -l", findme), function(stdout)
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

run_once_pgrep("picom --dbus --experimental-backend")
run_once_pgrep("mpDris2")
run_once_pgrep("ibus-daemon -x -r -R")
awful.spawn("gpaste-client start")
awful.spawn("xhost +")
awful.spawn([[sh -c "
  if command -v nvidia-smi ; then 
    xpra shadow $DISPLAY --bind-tcp=$(hostname).wg1:$((${DISPLAY:1}+6000)) --video-encoders=nvenc
  else 
    xpra shadow $DISPLAY --bind-tcp=$(hostname).wg1:$((${DISPLAY:1}+6000))
  fi
"]])
awful.spawn([[sh -c "
  if command -v nvidia-smi ; then 
    xpra start :$((${DISPLAY:1}+1000)) --bind-tcp=$(hostname).wg1:$((${DISPLAY:1}+7000)) --video-encoders=nvenc
  else 
    xpra start :$((${DISPLAY:1}+1000)) --bind-tcp=$(hostname).wg1:$((${DISPLAY:1}+7000))
  fi
"]])
