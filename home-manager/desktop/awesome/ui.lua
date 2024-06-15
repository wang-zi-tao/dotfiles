local bling = require("module.bling")
local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
require("awful.autofocus")

local machi = require("module.layout-machi")
beautiful.layout_machi = machi.get_icon()

local mstab = bling.layout.mstab
local centered = bling.layout.centered
local horizontal = bling.layout.horizontal
local equal = bling.layout.equalarea
local deck = bling.layout.deck
machi.editor.nested_layouts = {
    ["0"] = deck,
    ["1"] = awful.layout.suit.spiral,
    ["2"] = awful.layout.suit.fair,
    ["3"] = awful.layout.suit.fair.horizontal,
    ["4"] = mstab,
}
local l = awful.layout.suit
machi.default_layout = l.corner.nw
tag.connect_signal("request::default_layouts", function()
    awful.layout.append_default_layouts({
        -- l.corner.nw,
        -- l.tile,
        -- l.fair,
        -- l.spiral,
        -- l.max,
        -- l.magnifier,
        -- l.floating,
        -- centered,
        -- deck,
        -- horizontal,
        equal,
        machi.layout.create({
            name_func = function(t)
                return t.name .. "_default"
            end,
            default_cmd = "w11",
            new_placement_cb = machi.layout.placement.empty,
        }),
        -- mstab,
        l.max.fullscreen,
    })
end)

local names = { "1", "2", "3", "4", "5", "6", "7", "8", "9" }
screen.connect_signal("request::desktop_decoration", function(s)
    awful.tag(names, s, awful.layout.layouts[1])
end)

awful.screen.connect_for_each_screen(function(s)
    gears.wallpaper.maximized(gears.surface.load_uncached(beautiful.wallpaper), s, false, nil)
end)

naughty.connect_signal("request::display_error", function(message, startup)
    naughty.notification({
        urgency = "critical",
        title = "Oops, an error happened" .. (startup and " during startup!" or "!"),
        message = message,
    })
end)
client.connect_signal("request::manage", function(c)
    -- Add missing icon to client
    if not c.icon then
        local icon = gears.surface(beautiful.awesome_icon)
        c.icon = icon._native
        icon:finish()
    end

    -- Set the windows at the slave,
    if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)
client.connect_signal("mouse::enter", function(c)
    c:activate({ context = "mouse_enter", raise = false })
end)
-- bclient.connect_signal("focus", function(c)
--   -- c.border_color = beautiful.blue
-- end)
bling.module.flash_focus.enable()
