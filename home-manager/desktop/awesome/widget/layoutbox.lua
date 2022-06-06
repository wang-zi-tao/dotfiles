local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local util = require("widget.util")
return function(s)
  local layoutbox_buttons = gears.table.join(
    -- Left click
    awful.button({}, 1, function(c)
      awful.layout.inc(1)
    end),

    -- Right click
    awful.button({}, 3, function(c)
      awful.layout.inc(-1)
    end),

    -- Scrolling
    awful.button({}, 4, function()
      awful.layout.inc(-1)
    end),
    awful.button({}, 5, function()
      awful.layout.inc(1)
    end)
  )
  local mylayoutbox = awful.widget.layoutbox:new(s)
  mylayoutbox:buttons(layoutbox_buttons)
  return {
    mylayoutbox,
    margins = 2,
    widget = wibox.container.margin,
  }
end
