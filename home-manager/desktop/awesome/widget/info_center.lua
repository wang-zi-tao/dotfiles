local gears = require("gears")
local gfs = require("gears.filesystem")
local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local wibox = require("wibox")
local awful = require("awful")
local beautiful = require("beautiful")
local util = require("widget.util")

local info_center
awful.screen.connect_for_each_screen(function(s)
  local info_center_width = dpi(1000)
  local info_center_height = dpi(450)

  info_center = awful.popup({
    type = "dock",
    screen = s,
    width = dpi(info_center_width),
    maximum_width = dpi(info_center_width),
    maximum_height = dpi(info_center_height),
    bg = beautiful.transparent,
    ontop = true,
    visible = false,
    widget = {},
  })

  -- Day
  local day = wibox.widget({
    font = beautiful.font_name .. "Bold 28",
    format = "<span foreground='" .. beautiful.foreground .. "'>%A %m-%d</span>",
    align = "center",
    valign = "center",
    widget = wibox.widget.textclock,
  })

  -- Calendar
  s.calendar = require("widget.calendar")

  info_center:connect_signal("property::visible", function()
    if info_center.visible then
      s.calendar.date = os.date("*t")
    end
  end)

  -- Weather
  s.weather = require("widget.weather")()

  s.player = require("widget.playerctl")()
  s.notifys = require("widget.notify")
  -- Wallpaper
  local wallpaper_box = wibox.widget({
    {
      {
        image = beautiful.wallpaper,
        horizontal_fit_policy = "fit",
        vertical_fit_policy = "fit",
        -- clip_shape = helpers.rrect(dpi(5)),
        shape = util.rounded_shape(4),
        widget = wibox.widget.imagebox,
      },
      forced_width = dpi(270),
      forced_height = dpi(140),
      shape = util.rounded_shape(16),
      widget = wibox.container.background,
    },
    margins = dpi(10),
    widget = wibox.container.margin,
  })

  local info_center_widget = wibox.widget({
    {
      {
        layout = wibox.layout.fixed.horizontal,
        {
          layout = wibox.layout.fixed.vertical,
          util.big_block1(s.weather),
          util.big_block1(s.calendar),
        },
        {
          layout = wibox.layout.fixed.vertical,
          util.big_block1(day),
          wallpaper_box,
          util.big_block1(s.player),
        },
        {
          layout = wibox.layout.fixed.vertical,
          s.notifys,
        },
      },
      margins = dpi(10),
      widget = wibox.container.margin,
    },
    bg = beautiful.wibar_bg,
    shape = util.rounded_shape(4),
    widget = wibox.container.background,
  })
  info_center_widget:connect_signal("mouse::leave", function(widget)
    info_center:toggle()
  end)
  info_center:setup(util.big_block(info_center_widget))

  awful.placement.top(info_center, {
    honor_workarea = true,
    parent = s,
    margins = {
      top = dpi(40),
    },
  })

  -- Make toogle button
  local info_center_show = function()
    info_center.visible = true
    info_center:emit_signal("opened")
  end

  local info_center_hide = function()
    info_center.visible = false
    info_center:emit_signal("closed")
  end

  function info_center:toggle()
    if self.opened then
      info_center_hide()
    else
      info_center_show()
    end
    self.opened = not self.opened
  end
end)
return info_center
