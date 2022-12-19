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
                image = icon,
                halign = "center",
                valign = "center",
                forced_width = 24,
                forced_height = 24,
                widget = wibox.widget.imagebox,
            },
            fg = icon_color,
            widget = wibox.container.background,
        },
        id = id,
        min_value = 0,
        max_value = 100,
        value = 100,
        thickness = 3,
        forced_height = 40,
        forced_width = 40,
        bg = beautiful.background,
        border_width = 0,
        start_angle = 0.5 * math.pi,
        colors = colors,
        widget = wibox.container.arcchart,
    })
end

local cpu_usage_on_panel = process("cpu_usage", [[<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
   <path fill="#cccccc" d="M19,16C19,17.72 18.37,19.3 17.34,20.5C17.75,20.89 18,21.41 18,22H6C6,21.41 6.25,20.89 6.66,20.5C5.63,19.3 5,17.72 5,16H3C3,14.75 3.57,13.64 4.46,12.91L4.47,12.89C6,11.81 7,10 7,8V7A5,5 0 0,1 12,2A5,5 0 0,1 17,7V8C17,10 18,11.81 19.53,12.89L19.54,12.91C20.43,13.64 21,14.75 21,16H19M16,16A4,4 0 0,0 12,12A4,4 0 0,0 8,16A4,4 0 0,0 12,20A4,4 0 0,0 16,16M10,9L12,10.5L14,9L12,7.5L10,9M10,5A1,1 0 0,0 9,6A1,1 0 0,0 10,7A1,1 0 0,0 11,6A1,1 0 0,0 10,5M14,5A1,1 0 0,0 13,6A1,1 0 0,0 14,7A1,1 0 0,0 15,6A1,1 0 0,0 14,5Z" />
</svg>]], beautiful.foreground1, {
    beautiful.blue,
    beautiful.yellow,
    beautiful.background3,
    beautiful.orange,
    beautiful.yellow,
    beautiful.red,
    beautiful.red,
})
local ram_usage_on_panel = process("ram_usage", [[<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
   <path fill="#cccccc" d="M17,17H7V7H17M21,11V9H19V7C19,5.89 18.1,5 17,5H15V3H13V5H11V3H9V5H7C5.89,5 5,5.89 5,7V9H3V11H5V13H3V15H5V17A2,2 0 0,0 7,19H9V21H11V19H13V21H15V19H17A2,2 0 0,0 19,17V15H21V13H19V11M13,13H11V11H13M15,9H9V15H15V9Z" />
</svg>]], beautiful.foreground1, {
    beautiful.blue,
    beautiful.orange,
    beautiful.background3,
})
local hdd_usage_on_panel = process("hdd_usage", [[<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
   <path fill="#cccccc" d="M6,2H18A2,2 0 0,1 20,4V20A2,2 0 0,1 18,22H6A2,2 0 0,1 4,20V4A2,2 0 0,1 6,2M12,4A6,6 0 0,0 6,10C6,13.31 8.69,16 12.1,16L11.22,13.77C10.95,13.29 11.11,12.68 11.59,12.4L12.45,11.9C12.93,11.63 13.54,11.79 13.82,12.27L15.74,14.69C17.12,13.59 18,11.9 18,10A6,6 0 0,0 12,4M12,9A1,1 0 0,1 13,10A1,1 0 0,1 12,11A1,1 0 0,1 11,10A1,1 0 0,1 12,9M7,18A1,1 0 0,0 6,19A1,1 0 0,0 7,20A1,1 0 0,0 8,19A1,1 0 0,0 7,18M12.09,13.27L14.58,19.58L17.17,18.08L12.95,12.77L12.09,13.27Z" />
</svg>]], beautiful.foreground1, { beautiful.foreground })
local temp_status_on_panel = process("temp_status", [[<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
   <path fill="#cccccc" d="M17 3H21V5H17V3M17 7H21V9H17V7M17 11H21V13H17.75L17 12.1V11M21 15V17H19C19 16.31 18.9 15.63 18.71 15H21M7 3V5H3V3H7M7 7V9H3V7H7M7 11V12.1L6.25 13H3V11H7M3 15H5.29C5.1 15.63 5 16.31 5 17H3V15M15 13V5C15 3.34 13.66 2 12 2S9 3.34 9 5V13C6.79 14.66 6.34 17.79 8 20S12.79 22.66 15 21 17.66 16.21 16 14C15.72 13.62 15.38 13.28 15 13M12 4C12.55 4 13 4.45 13 5V8H11V5C11 4.45 11.45 4 12 4Z" />
</svg>]], beautiful.foreground1, { beautiful.foreground })
function M.panel(screen)
    local slider = {
        cpu_usage_on_panel,
        ram_usage_on_panel,
        hdd_usage_on_panel,
        temp_status_on_panel,
        spacing = 1,
        layout = wibox.layout.fixed.horizontal,
    }
    local prev = {}
    local data = {}
    local function diff(key)
        return data[key] - (prev[key] or 0)
    end

    watch([[bash -c " cat /proc/stat | grep '^cpu ' "]], 1, function(_, stdout)
        data.user, data.nice, data.system, data.idle, data.iowait, data.irq, data.softirq, data.steal, data.guest,
            data.guest_nice = stdout:match(
            "(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s"
        )
        data.total = data.user + data.nice + data.system + data.idle + data.iowait + data.irq + data.softirq 
        cpu_usage_on_panel.max_value = diff("total")
        cpu_usage_on_panel.values = {
            diff("user") - diff("guest"),
            diff("nice"),
            diff("system"),
            diff("iowait"),
            diff("guest"),
            diff("irq"),
            diff("softirq"),
        }
        prev, data = data, {}
        collectgarbage("collect")
    end)
    watch([[bash -c "LANG='' free | grep -z Mem.*Swap.*"]], 10, function(_, stdout)
        local total, used, free, shared, buff_cache, available, total_swap, used_swap, free_swap = stdout:match(
            "(%d+)%s*(%d+)%s*(%d+)%s*(%d+)%s*(%d+)%s*(%d+)%s*Swap:%s*(%d+)%s*(%d+)%s*(%d+)"
        )
        ram_usage_on_panel.max_value = total
        ram_usage_on_panel.values = {
            used,
            shared,
            total - free - used - shared,
        }
        collectgarbage("collect")
    end)
    watch([[bash -c "df -h / |grep '^/' | awk '{print $5}'"]], 10, function(_, stdout)
        local space_consumed = stdout:match("(%d+)")
        hdd_usage_on_panel.value = (tonumber(space_consumed))
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
  ]]     ,
        function(stdout)
            local temp_path = stdout:gsub("%\n", "")
            if temp_path == "" or not temp_path then
                temp_path = "/sys/class/thermal/thermal_zone0/temp"
            end
            watch([[ sh -c "cat ]] .. temp_path .. [[" ]], 10, function(_, stdout)
                local temp = stdout:match("(%d+)")
                temp_status_on_panel.value = ((temp / 1000) / max_temp * 100)
                collectgarbage("collect")
            end)
        end
    )
    return slider
end

return M
