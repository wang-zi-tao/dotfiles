local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local util = require("widget.util")
local bling = require("module.bling")
local workspace_names = { " ", " ", "", " ", "", " ", " ", " ", "" }
bling.widget.tag_preview.enable({
  show_client_content = true, -- Whether or not to show the client content x = 30, -- The x-coord of the popup
  y = 30, -- The y-coord of the popup
  scale = 0.5, -- The scale of the previews compared to the screen
  honor_padding = true, -- Honor padding when creating widget size
  honor_workarea = true, -- Honor work area when creating widget size
  placement_fn = function(c) -- Place the widget using awful.placement (this overrides x & y)
    awful.placement.top_left(c, {
      margins = {
        top = 40,
        left = 40,
      },
    })
  end,
  background_widget = wibox.widget({ -- Set a background image (like a wallpaper) for the widget
    image = beautiful.wallpaper,
    horizontal_fit_policy = "full",
    vertical_fit_policy = "full",
    widget = wibox.widget.imagebox,
  }),
})
return function(s)
  return awful.widget.taglist({
    screen          = s,
    filter          = function(tag)
      return (awful.widget.taglist.filter.noempty(tag))
    end,
    style           = {
      shape = util.rounded_shape(2),
    },
    layout          = {
      spacing = 1,
      layout = wibox.layout.flex.horizontal,
    },
    widget_template = {
      {
        {

          id = "index_role",
          align = "center",
          valign = "center",
          forced_width = 24,
          widget = wibox.widget.textbox,
        },
        top = 2,
        left = 6,
        right = 6,
        bottom = 2,
        widget = wibox.container.margin,
      },
      id = "background_role",
      widget = wibox.container.background,
      -- Add support for hover colors and an index label
      create_callback = function(self, tag, index, objects) --luacheck: no unused args
        self:get_children_by_id("index_role")[1].text = workspace_names[tag.index]
        self:connect_signal("mouse::enter", function()
          awesome.emit_signal("bling::tag_preview::update", tag)
          awesome.emit_signal("bling::tag_preview::visibility", s, true)
        end)
        self:connect_signal("mouse::leave", function()
          awesome.emit_signal("bling::tag_preview::visibility", s, false)
        end)
      end,
      pdate_callback = function(self, tag, index, objects) --luacheck: no unused args
        self:get_children_by_id("index_role")[1].text = workspace_names[tag.index]
      end,
    },
    buttons         = gears.table.join(
      awful.button({}, 1, function(t)
        awesome.emit_signal("bling::tag_preview::visibility", s, false)
        t:view_only()
      end),
      awful.button({ mod }, 1, function(t)
        awesome.emit_signal("bling::tag_preview::visibility", s, false)
        if client.focus then
          client.focus:move_to_tag(t)
        end
      end),
      awful.button({}, 3, awful.tag.viewtoggle),
      awful.button({ mod }, 3, function(t)
        awesome.emit_signal("bling::tag_preview::visibility", s, false)
        if client.focus then
          client.focus:toggle_tag(t)
        end
      end),
      awful.button({}, 4, function(t)
        awesome.emit_signal("bling::tag_preview::visibility", s, false)
        awful.tag.viewnext(t.screen)
      end),
      awful.button({}, 5, function(t)
        awesome.emit_signal("bling::tag_preview::visibility", s, false)
        awful.tag.viewprev(t.screen)
      end)
    ),
  })
end
