local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local util = require("widget.util")
local apps = require("apps")
local launcher = require("widget.launcher")

local info_center = require("widget.info_center")

awful.screen.connect_for_each_screen(function(s)
  s.mywibar = awful.wibar({
    type = "dock",
    stretch = true,
    visible = true,
    height = dpi(36),
    ontop = false,
    width = s.geometry.width - dpi(16),
    screen = s,
    position = "top",
    y = dpi(3),
    bg = "#00000000",
  })
  local mylayoutbox = require("widget.layoutbox")(s)
  local mytaglist = require("widget.workspaces")(s)
  local mybatterybox = require("widget.battery").panel(s)
  local tray = require("widget.tray")()
  local system_monitor = require("widget.system_monitor").panel(s)
  local volume_widget = require("widget.volume").panel(s)
  local mytasklist = require("widget.tasklist")(s)
  s.mywibar:setup({
    {
      util.block({
        util.button({ text = " ", widget = wibox.widget.textbox }, function()
          launcher:toggle()
        end),
        {
          mytaglist,
          util.button(mylayoutbox),
          mytasklist,
          spacing = 8,
          layout = wibox.layout.fixed.horizontal,
        },
        spacing = 4,
        layout = wibox.layout.fixed.horizontal,
      }),
      util.block(util.button({
        font = beautiful.font_name .. "Bold 12",
        format = " %B%e日  %H:%M:%S  %A ",
        align = "center",
        valign = "center",
        refresh = 1,
        widget = wibox.widget.textclock,
      }, function()
        info_center:toggle()
      end)),
      util.block({
        tray,
        volume_widget,
        util.block1(system_monitor),
        mybatterybox,
        util.button(util.symbol(""), function()
          awesome.emit_signal("control_center::toggle")
        end),
        spacing = 8,
        layout = wibox.layout.fixed.horizontal,
      }),
      expand = "none",
      spacing = 10,
      layout = wibox.layout.align.horizontal,
    },
    -- dg = beautiful.background,
    layout = wibox.container.background,
  })
end)
