local gears = require("gears")
local gfs = require("gears.filesystem")
local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local wibox = require("wibox")
local awful = require("awful")
local beautiful = require("beautiful")
local util = require("widget.util")
local react = require("react")
local config = require("config")
local watch = awful.widget.watch
local function button(symbol_enabled, symbol_disabled, text_enabled, text_disabled, enabled, callback)
  return {
    {
      {
        text = enabled and symbol_enabled or symbol_disabled,
        align = "center",
        valign = "center",
        forced_width = 48,
        forced_height = 48,
        font = beautiful.font_name .. "Bold 16",
        widget = wibox.widget.textbox,
      },
      fg = enabled and beautiful.foreground or beautiful.foreground1,
      shape = gears.shape.rounded_bar,
      bg = enabled and beautiful.blue or beautiful.background,
      widget = wibox.container.background,
    },
    {
      text = enabled and text_enabled or text_disabled,
      font = beautiful.font_name .. "Normal 12",
      widget = wibox.widget.textbox,
    },
    spacing = 8,
    layout = wibox.layout.fixed.horizontal,
    buttons = gears.table.join(awful.button({}, 1, callback)),
  }
end

local function slider(symbol, value, callback)
  local slider_widget = wibox.widget({
    bar_shape = gears.shape.rounded_rect,
    bar_height = 16,
    bar_color = beautiful.background2,
    handle_width = 32,
    handle_color = beautiful.blue,
    handle_shape = gears.shape.circle,
    handle_border_color = beautiful.foreground,
    handle_border_width = 4,
    minimum = 0,
    maximum = 100,
    forced_height = 40,
    value = value,
    widget = wibox.widget.slider,
  })
  slider_widget:connect_signal("property::value", function(_, new_value)
    callback(new_value)
  end)
  return util.big_block1({
    {
      {
        text = symbol,
        align = "center",
        valign = "center",
        forced_width = 48,
        forced_height = 48,
        font = beautiful.font_name .. "Bold 16",
        widget = wibox.widget.textbox,
      },
      fg = beautiful.foreground,
      shape = gears.shape.rounded_bar,
      bg = beautiful.background,
      widget = wibox.container.background,
    },
    {
      slider_widget,
      left = dpi(16),
      right = dpi(16),
      widget = wibox.container.margin,
    },
    spacing = 16,
    layout = wibox.layout.fixed.horizontal,
  })
end

local control_center_react = react({
  default_states = {
    volume = 0,
    brightness = 100,
    old_layout = false,
    wifi = false,
    bluetooth = {},
    bluetooth_enable = true,
    fly_mode = false,
    cursor = {},
    remote_hosts = {},
    remote_hosts_cli_enable = false,
    remote_hosts_cli = {},
    remote_hosts_shell_enable = false,
    remote_hosts_shell = {},
    remote_hosts_gui_enable = false,
    remote_hosts_gui = {},
  },
  init = function(self)
    awesome.connect_signal("signal::volume", function(volume_int, muted)
      self:set_state({ volume = volume_int })
    end)
    watch("brightnessctl -m", 16, function(_, stdout)
      local split = gears.string.split(stdout, ",")
      local brightness = 100 * split[3] / split[5]
      if brightness ~= self.state.brightness then
        self.set_state({ brightness = brightness })
      end
    end)
    awful.spawn.easy_async_with_shell([[LANG='' nmcli monitor]], function(stdout)
      local split = gears.string.split(stdout, " ")
      for _, device in pairs(config.wifi_devices) do
        if split[1] == device .. ":" then
          if split[2] == "disconnected" then
            self:set_state({ wifi = false })
          elseif split[2] == "using" and split[3] == "connection" then
            self:set_state({ wifi = split[4] })
          end
        end
      end
    end)
  end,
  render = function(self)
    return {
      slider("", self.state.volume, function(v)
        awful.spawn("amixer set Master " .. v .. "%", false)
      end),
      slider("", self.state.brightness, function(v)
        awful.spawn("brightness set " .. v .. "%", false)
        self:set_state({ brightness = v })
      end),
      {
        util.big_block1({
          button("直", "睊", "wifi " .. (self.state.wifi or "on"), "wifi off", self.state.wifi ~= nil, function()
            if self.state.wifi then
              for _, device in pairs(config.wifi_devices) do
                awful.spawn("nmcli device disconnect " .. device)
              end
            else
              for _, device in pairs(config.wifi_devices) do
                awful.spawn("nmcli device connect " .. device)
              end
            end
          end),
          button("", "", "bluetooth on", "bluetooth off", self.state.bluetooth_enable, function() end),
          button("", "舘", "float on", "float off", self.state.old_layout, function()
            if self.state.old_layout then
              awful.layout.set(self.state.old_layout)
              self:set_state({ old_layout = false })
            else
              local layout = awful.layout.get(self.props.screen)
              awful.layout.set(awful.layout.suit.floating)
              self:set_state({ old_layout = layout })
            end
          end),
          spacing = 8,
          layout = wibox.layout.fixed.vertical,
        }),
        util.big_block1({
          button("ﲳ", "", "remote cli on", "remote cli off", self.state.remote_hosts_cli_enable, function()
            self:set_state({ remote_hosts_cli_enable = not self.state.remote_hosts_cli_enable })
          end),
          button("ﲳ", "", "remote shell on", "remote shell off", self.state.remote_hosts_shell_enable, function()
            self:set_state({ remote_hosts_shell_enable = not self.state.remote_hosts_shell_enable })
          end),
          button("", "", "remote gui on", "remote gui off", self.state.remote_hosts_gui_enable, function()
            self:set_state({ remote_hosts_gui_enable = not self.state.remote_hosts_gui_enable })
          end),
          spacing = 8,
          layout = wibox.layout.fixed.vertical,
        }),
        spacing = 8,
        layout = wibox.layout.fixed.horizontal,
      },
      layout = wibox.layout.fixed.vertical,
    }
  end,
})

awful.screen.connect_for_each_screen(function(s)
  local width = dpi(400)
  local height = dpi(600)
  local control_center_widget = wibox.widget(util.big_block({
    control_center_react({ screen = s }),
    widget = wibox.container.margin,
  }))
  local control_center = awful.popup({
    type = "dock",
    screen = s,
    width = dpi(width),
    maximum_width = dpi(width),
    maximum_height = dpi(height),
    bg = beautiful.transparent,
    ontop = true,
    visible = false,
    widget = control_center_widget,
  })
  awful.placement.top_right(control_center, {
    parent = s,
    positions = "bottom_left",
    margins = {
      top = dpi(40),
      right = dpi(20),
    },
  })
  control_center_widget:connect_signal("mouse::leave", function(widget)
    control_center.visible = false
    control_center:emit_signal("closed")
  end)
  awesome.connect_signal("control_center::toggle", function()
    if not control_center.visible then
      control_center.visible = true
      control_center:emit_signal("opened")
    else
      control_center.visible = false
      control_center:emit_signal("closed")
    end
  end)
end)
