local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local util = require("widget.util")
local launcher = require("module.bling.widget.app_launcher")({
  shape = util.rounded_shape(16),
  prompt_shape = util.rounded_shape(16),
  app_shape = util.rounded_shape(16),
  background = beautiful.background,
  app_normal_color = beautiful.background1,
  prompt_margins = dpi(8),
  apps_per_column = 8,
  apps_per_raw = 4,
  app_content_padding = dpi(8),
  app_content_spacing = dpi(8),
  save_history = true,
  app_selected_color = beautiful.blue,
  app_selected_hover_color = beautiful.blue,
  app_width = dpi(128),
  app_height = dpi(128),
  prompt_text = "",
  prompt_height = dpi(64);
  prompt_paddings = dpi(8);
})
return launcher
