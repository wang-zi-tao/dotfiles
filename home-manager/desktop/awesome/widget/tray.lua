local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local util = require("widget.util")
local react = require("react")
return react({
  default_states = { expand = false },
  render = function(self)
    if self.state.expand then
      return util.block1({
        { widget = wibox.widget.systray },
        util.button({
          text = " ",
          widget = wibox.widget.textbox,
        }, function()
          self:set_state({ expand = false })
        end),
        spacing = 8,
        layout = wibox.layout.fixed.horizontal,
      })
    else
      return {
        util.button({
          text = " ",
          widget = wibox.widget.textbox,
        }, function()
          self:set_state({ expand = true })
        end),
        spacing = 8,
        layout = wibox.layout.fixed.horizontal,
      }
    end
  end,
})
