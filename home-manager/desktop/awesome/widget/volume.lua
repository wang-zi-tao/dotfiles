local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local watch = awful.widget.watch

local M = {}
function M.panel(screen)
  local volume_icon = wibox.widget({
    text = "",
    align = "center",
    valign = "center",
    font = beautiful.font_name .. "Bold 11",
    widget = wibox.widget.textbox,
  })
  local volume = 0

  local volume_progressbar = wibox.widget({
    max_value = 1,
    value = 0.5,
    forced_width = 48,
    paddings = 0,
    border_width = 1,
    color = beautiful.orange,
    background_color = beautiful.background1,
    border_color = beautiful.background1,
    bar_border_width = 0,
    clip = true,
    shape = gears.shape.rounded_bar,
    bar_shape = gears.shape.rectangle,
    widget = wibox.widget.progressbar,
    buttons = gears.table.join(
      awful.button({}, 5, function(t)
        volume = volume - 5
        if volume < 0 then
          volume = 0
        end
        awful.spawn("amixer set Master " .. volume .. "%", false)
        awesome.emit_signal("signal::volume", volume, volume == 0)
      end),
      awful.button({}, 4, function(t)
        volume = volume + 5
        if volume > 100 then
          volume = 100
        end
        awful.spawn("amixer set Master " .. volume .. "%", false)
        awesome.emit_signal("signal::volume", volume, volume == 0)
      end)
    ),
  })
  local volume_box = wibox.widget({
    volume_progressbar,
    volume_icon,
    layout = wibox.layout.stack,
  })
  awesome.connect_signal("signal::volume", function(vol, muted)
    volume = vol
    volume_progressbar.value = vol / 100
    if muted then
      volume_icon.text = ""
    else
      volume_icon.text = "" .. vol .. "%"
    end
  end)
  local volume_old
  local muted_old
  watch("pamixer --get-volume", 1, function(_, stdout)
    local volume_int = tonumber(stdout)
    if volume_int ~= volume_old or muted_int ~= muted_old then
      awesome.emit_signal("signal::volume", volume_int, muted_int)
      volume_old = volume_int
      muted_old = muted_int
    end
  end)
  return volume_box
end
return M
