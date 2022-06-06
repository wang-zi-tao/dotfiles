local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local util = require("widget.util")
local playerctl = require("module.bling").signal.playerctl.cli()
local naughty = require("naughty")

return require("react")({
  default_states = {
    playing = true,
    art = "",
    name = "",
    title = "",
    artist = "",
  },
  init = function(self)
    playerctl:connect_signal("metadata", function(_, title, artist, album_path, album, new, player_name)
      if new == true then
        naughty.notify({ title = title, text = artist, image = album_path })
      end
      self:set_state({
        art = album_path,
        name = player_name,
        title = title,
        artist = artist,
      })
    end)
    playerctl:connect_signal("playback_status", function(_, playing)
      self.set_state({ playing = playing })
    end)
  end,
  render = function(self)
    return {
      layout = wibox.layout.align.horizontal,
      {
        layout = wibox.layout.fixed.horizontal,
        expand = "none",
        spacing = dpi(10),
        {
          {
            image = self.state.art and gears.surface.load_uncached(self.state.art),
            resize = true,
            forced_height = dpi(80),
            forced_width = dpi(80),
            widget = wibox.widget.imagebox,
          },
          bg = beautiful.background,
          widget = wibox.container.background,
        },
        {
          layout = wibox.layout.align.vertical,
          expand = "none",
          {
            layout = wibox.layout.align.vertical,
            expand = "none",
            nil,
            {
              layout = wibox.layout.fixed.vertical,
              -- name_widget,
              {
                text = self.state.title,
                font = beautiful.font_name .. "Bold 12",
                align = "center",
                valign = "center",
                widget = wibox.widget.textbox,
              },
              {
                text = self.state.artist,
                font = beautiful.font_name .. "Bold 10",
                align = "center",
                valign = "center",
                widget = wibox.widget.textbox,
              },
            },
            nil,
          },
          nil,
          {
            layout = wibox.layout.flex.horizontal,
            spacing = dpi(16),
            {
              util.big_button(util.symbol("ﭣ"), function()
                playerctl:previous()
              end),
              forced_width = dpi(36),
              forced_height = dpi(36),
              bg = beautiful.transparent,
              shape = gears.shape.circle,
              widget = wibox.container.background,
            },
            {
              util.big_button(util.symbol(self.state.playing and "" or "契"), function()
                playerctl:play_pause()
              end),
              forced_width = dpi(36),
              forced_height = dpi(36),
              bg = beautiful.transparent,
              shape = gears.shape.circle,
              widget = wibox.container.background,
            },
            {
              util.big_button(util.symbol("ﭡ"), function()
                playerctl:next()
              end),
              forced_width = dpi(36),
              forced_height = dpi(36),
              bg = beautiful.transparent,
              shape = gears.shape.circle,
              widget = wibox.container.background,
            },
          },
        },
      },
    }
  end,
})
