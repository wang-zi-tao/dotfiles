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
local function button(symbol_enabled, symbol_disabled, text_enabled, text_disabled, enabled, callback)
  return util.big_block1({
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
  })
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
  slider_widget:connect_signal("button::release", callback)
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
    float = false,
    wifi = "",
    bluetooth = {},
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
  init = function(self) end,
  render = function(self)
    return {
      {
        slider("", self.state.volume, function() end),
        slider("", self.state.brightness, function() end),
        {
          button("直", "睊", "wifi on", "wifi off", self.state.wifi ~= nil, function() end),
          button("", "", "bluetooth on", "bluetooth off", self.state.bluetooth_enable, function() end),
          button("", "舘", "float on", "float off", self.state.float, function() end),
          button("ﲳ", "", "remote cli on", "remote cli off", self.state.remote_hosts_cli_enable, function()
            self:set_state({ remote_hosts_cli_enable = not self.state.remote_hosts_cli_enable })
          end),
          button("ﲳ", "", "remote shell on", "remote shell off", self.state.remote_hosts_shell_enable, function()
            self:set_state({ remote_hosts_shell_enable = not self.state.remote_hosts_shell_enable })
          end),
          button("", "", "remote gui on", "remote gui off", self.state.remote_hosts_gui_enable, function()
            self:set_state({ remote_hosts_gui_enable = not self.state.remote_hosts_gui_enable })
          end),
          forced_num_cols = 2,
          expand = true,
          layout = wibox.layout.grid,
        },
        layout = wibox.layout.fixed.vertical,
      },
      layout = wibox.layout.fixed.horizontal,
    }
  end,
})

awful.screen.connect_for_each_screen(function(s)
  local width = dpi(400)
  local height = dpi(600)
  local control_center_widget = wibox.widget(util.big_block({
    control_center_react(),
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
