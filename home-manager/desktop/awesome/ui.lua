local bling = require("module.bling")
local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
require("awful.autofocus")
local dpi = xresources.apply_dpi

local l = awful.layout.suit
awful.layout.layouts = {
  l.corner.nw,
  l.tile,
  l.fair,
  l.spiral,
  l.max,
  l.max.fullscreen,
  l.magnifier,
  l.floating,
}

local names = { "\0xf269", "\0xf120", "\0xe7a8", "\0xf48a", "\0xf126", "\0xfb04", "\0xf313", "\0xf308", "\0xf872" }
-- local names = { "1 firefox", "2", "3", "4", "5", "6", "7", "8", "9" }
screen.connect_signal("request::desktop_decoration", function(s)
  awful.tag(names, s, awful.layout.layouts[1])
end)

awful.screen.connect_for_each_screen(function(s)
  gears.wallpaper.maximized(gears.surface.load_uncached(beautiful.wallpaper), s, false, nil)
end)

-- List of apps to start once on start-up
local autostart_app = {
  -- Compositor
  "killall picom; picom --dbus --experimental-backend",
  -- Playertctl support for mpd
  "mpDris2",
}

for _, command in ipairs(autostart_app) do
  awful.spawn.easy_async_with_shell(string.format("ps aux | grep '%s' | grep -v 'grep'", command), function(stdout)
    if stdout == "" or stdout == nil then
      awful.spawn(command, false)
    end
  end)
end

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
