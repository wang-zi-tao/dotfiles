local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local util = require("widget.util")
local apps = require("apps")
local bling = require("module.bling")
bling.widget.task_preview.enable {
  x = 20, -- The x-coord of the popup
  y = 20, -- The y-coord of the popup
  height = 600, -- The height of the popup
  width = 800, -- The width of the popup
  placement_fn = function(c) -- Place the widget using awful.placement (this overrides x & y)
    awful.placement.top_left(c, {
      margins = {
        top = 40,
        left = 40,
      }
    })
  end,
  -- Your widget will automatically conform to the given size due to a constraint container.
  widget_structure = {
    {
      {
        {
          id = 'icon_role',
          widget = awful.widget.clienticon, -- The client icon
        },
        {
          id = 'name_role', -- The client name / title
          ellipsize = "end",
          font = beautiful.font_name .. "Bold 11",
          widget = wibox.widget.textbox,
        },
        layout = wibox.layout.flex.horizontal
      },
      margins = 8,
      widget = wibox.container.margin,
    },
    {
      id = 'image_role', -- The client preview
      resize = true,
      valign = 'center',
      halign = 'center',
      forced_width = 32,
      forced_height = 32,
      awful.widget.clienticon,
    },
    layout = wibox.layout.fixed.vertical
  }
}
return function(s)
  s.mytasklist = awful.widget.tasklist {
    screen          = s,
    -- filter          = awful.widget.tasklist.filter.currenttags,
    filter          = awful.widget.tasklist.filter.alltags,
    -- buttons         = tasklist_buttons,
    layout          = {
      spacing = 1,
      layout  = wibox.layout.fixed.horizontal
    },
    -- Notice that there is *NO* wibox.wibox prefix, it is a template,
    -- not a widget instance.
    widget_template = {
      {
        awful.widget.clienticon,
        margins = 2,
        widget  = wibox.container.margin
      },
      id              = "background_role",
      bg              = beautiful.background2,
      shape           = util.rounded_shape(8),
      create_callback = function(self, c, _, _)
        self:connect_signal("button::press", function()
          awesome.emit_signal("bling::task_preview::visibility", s,
            false, c)
          if not c.active then
            c:activate({
              context = "through_dock",
              switch_to_tag = true,
            })
          else
            c.minimized = true
          end
        end)
        self:connect_signal('mouse::enter', function()
          awesome.emit_signal("bling::task_preview::visibility", s,
            true, c)
        end)
        self:connect_signal('mouse::leave', function()
          awesome.emit_signal("bling::task_preview::visibility", s,
            false, c)
        end)
      end,
      layout          = wibox.container.background
    },
  }
  return s.mytasklist
end
