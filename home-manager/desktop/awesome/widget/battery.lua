local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local util = require("widget.util")
local battery_widget = require("module.battery_widget")

local M = {}
function M.panel(screen)
  -- Create the battery widget:
  local my_battery_widget = battery_widget({
    screen = screen,
    use_display_device = true,
    widget_template = wibox.widget.textbox,
  })

  -- When UPower updates the battery status, the widget is notified
  -- and calls a signal you need to connect to:
  my_battery_widget:connect_signal("upower::update", function(widget, device)
    local icon = "  "
    if device.percentage < 20 then
      icon = "  "
    elseif device.percentage < 40 then
      icon = "  "
    elseif device.percentage < 60 then
      icon = "  "
    elseif device.percentage < 80 then
      icon = "  "
    else
      icon = "  "
    end
    widget.text = string.format(icon .. "%3d", device.percentage) .. "%"
  end)
  return my_battery_widget
end

return M
