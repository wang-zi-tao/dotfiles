local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local util = require("widget.util")
local watch = awful.widget.watch
local react = require("react")

local react_demo = react({
    render = function(self)
        return {
            font = beautiful.font_name .. "Bold 8",
            text = self.state.out,
            widget = wibox.widget.textbox,
        }
    end,
    default_states = { out = 0 },
    init = function(self)
        gears.timer({
            timeout = 1,
            call_now = true,
            autostart = true,
            callback = function()
                self:set_state({ out = self.state.out + 1 })
            end,
        })
    end,
})
return react_demo
