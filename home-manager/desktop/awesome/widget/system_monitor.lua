local wibox = require("wibox")
local gears = require("gears")
local awful = require("awful")
local beautiful = require("beautiful")
local watch = awful.widget.watch
local dpi = beautiful.xresources.apply_dpi

local M = {}
local function process(id, icon, icon_color, colors)
  return wibox.widget({
    {
      {
        text = icon,
        align = "center",
        valign = "center",
        font = beautiful.font_name .. "Normal 9",
        widget = wibox.widget.textbox,
      },
      fg = icon_color,
      widget = wibox.container.background,
    },
    id = id,
    min_value = 0,
    max_value = 100,
    value = 100,
    thickness = 3,
    forced_height = 36,
    forced_width = 36,
    rounded_edge = true,
    bg = beautiful.background,
    border_width = 0,
    colors = colors,
    widget = wibox.container.arcchart,
  })
end
function M.panel(screen)
  local cpu_usage = process("cpu_usage", "", beautiful.foreground1, { beautiful.foreground })
  local ram_usage = process("ram_usage", "ﲮ", beautiful.foreground1, { beautiful.foreground })
  local hdd_usage = process("hdd_usage", "", beautiful.foreground1, { beautiful.foreground })
  local temp_status = process("temp_status", "", beautiful.foreground1, { beautiful.foreground })
  local slider = {
    cpu_usage,
    ram_usage,
    hdd_usage,
    temp_status,
    spacing = 1,
    layout = wibox.layout.fixed.horizontal,
  }
  local total_prev = 0
  local idle_prev = 0
  watch([[bash -c " cat /proc/stat | grep '^cpu ' "]], 1, function(_, stdout)
    local user, nice, system, idle, iowait, irq, softirq, steal, guest, guest_nice = stdout:match(
      "(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s"
    )
    local total = user + nice + system + idle + iowait + irq + softirq + steal
    local diff_idle = idle - idle_prev
    local diff_total = total - total_prev
    local diff_usage = (1000 * (diff_total - diff_idle) / diff_total + 5) / 10
    cpu_usage.value = diff_usage
    total_prev = total
    idle_prev = idle
    collectgarbage("collect")
  end)
  watch([[bash -c "LANG='' free | grep -z Mem.*Swap.*"]], 10, function(_, stdout)
    local total, used, free, shared, buff_cache, available, total_swap, used_swap, free_swap = stdout:match(
      "(%d+)%s*(%d+)%s*(%d+)%s*(%d+)%s*(%d+)%s*(%d+)%s*Swap:%s*(%d+)%s*(%d+)%s*(%d+)"
    )
    ram_usage.value = (used / total * 100)
    collectgarbage("collect")
  end)
  watch([[bash -c "df -h / |grep '^/' | awk '{print $5}'"]], 10, function(_, stdout)
    local space_consumed = stdout:match("(%d+)")
    hdd_usage.value = (tonumber(space_consumed))
    collectgarbage("collect")
  end)
  local max_temp = 80
  awful.spawn.easy_async_with_shell(
    [[
  temp_path=null
  for i in /sys/class/hwmon/hwmon*/temp*_input;
  do
  	temp_path="$(echo "$(<$(dirname $i)/name): $(cat ${i%_*}_label 2>/dev/null ||
  		echo $(basename ${i%_*})) $(readlink -f $i)");"
  
  	label="$(echo $temp_path | awk '{print $2}')"
  
  	if [ "$label" = "Package" ];
  	then
  		echo ${temp_path} | awk '{print $5}' | tr -d ';\n'
  		exit;
  	fi
  done
  ]],
    function(stdout)
      local temp_path = stdout:gsub("%\n", "")
      if temp_path == "" or not temp_path then
        temp_path = "/sys/class/thermal/thermal_zone0/temp"
      end
      watch([[ sh -c "cat ]] .. temp_path .. [[" ]], 10, function(_, stdout)
        local temp = stdout:match("(%d+)")
        temp_status.value = ((temp / 1000) / max_temp * 100)
        collectgarbage("collect")
      end)
    end
  )
  return slider
end
return M
