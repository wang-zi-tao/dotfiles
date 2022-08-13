local gears = require("gears")
local gfs = require("gears.filesystem")
local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local wibox = require("wibox")

local themes_path = gfs.get_themes_dir()
local theme = dofile(themes_path .. "default/theme.lua")

theme.layoutlist_shape_selected = function(cr, w, h)
  gears.shape.rounded_rect(cr, w, h, 4)
end
theme.layoutlist_bg_selected = theme.lighter_bg

theme.font_name = "Iosevka "
theme.icon_font_name = "Iosevka "
theme.font = theme.font_name .. "Medium 15"
theme.title_font = theme.font_name .. "Medium 15"
theme.icon_font = "Material Icons "
theme.icon_theme = "Papirus-Dark"

theme.awesome_icon = theme_assets.awesome_icon(dpi(30), theme.xbackground, theme.xforeground)

theme.xbackground = "#181c14"
theme.xforeground = "#D9D7D6"
theme.xcolor0 = "#1C252C"
theme.xcolor1 = "#DF5B61"
theme.xcolor2 = "#78B892"
theme.xcolor3 = "#DE8F78"
theme.xcolor4 = "#6791C9"
theme.xcolor5 = "#BC83E3"
theme.xcolor6 = "#67AFC1"
theme.xcolor7 = "#D9D7D6"
theme.xcolor8 = "#484E5B"
theme.xcolor9 = "#F16269"
theme.xcolor10 = "#8CD7AA"
theme.xcolor11 = "#E9967E"
theme.xcolor12 = "#79AAEB"
theme.xcolor13 = "#C488EC"
theme.xcolor14 = "#7ACFE4"
theme.xcolor15 = "#E5E5E5"
theme.darker_bg = "#181c14"
theme.lighter_bg = "#162026"
theme.transparent = "#00000000"

theme.foreground = "#ffffff"
theme.foreground1 = "#D8DEE9"
theme.foreground2 = "#C8CED9"
theme.background = "#10171e"
-- theme.background1 = "#131a21"
-- theme.background2 = "#1a222a"
theme.background1 = "#1a222a"
theme.background2 = "#2d2a33"
theme.black = "#1C252C"
theme.red = "#DF5B61"
theme.green = "#78B892"
theme.orange = "#DE8F78"
theme.blue = "#6791C9"
theme.purple = "#BC83E3"
theme.sky = "#67AFC1"
theme.white = "#D9D7D6"
theme.gray = "#484E5B"

theme.taglist_fg_focus = theme.xcolor12
theme.fg_normal = theme.foreground

theme.titlebar_bg = theme.darker_bg

theme.useless_gap = dpi(4)

theme.border_width = dpi(0)
theme.oof_border_width = dpi(0)
theme.border_color_marked = theme.titlebar_bg
theme.border_color_active = theme.titlebar_bg
theme.border_color_normal = theme.titlebar_bg
theme.border_color_new = theme.titlebar_bg
theme.border_color_urgent = theme.titlebar_bg
theme.border_color_floating = theme.titlebar_bg
theme.border_color_maximized = theme.titlebar_bg
theme.border_color_fullscreen = theme.titlebar_bg

theme.border_radius = dpi(6)

local taglist_square_size = dpi(0)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(taglist_square_size, theme.fg_normal)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(taglist_square_size, theme.fg_normal)
theme.taglist_fg_focus = theme.blue
theme.taglist_bg_focus = theme.blue
theme.taglist_bg_occupied = theme.background1
theme.taglist_fg_urgent = theme.blue
theme.taglist_bg_urgent = theme.background1
theme.taglist_bg_empty = theme.background1

theme.wallpaper = ".config/awesome/resources/share/backgrounds/大鱼海棠8.jpg"
theme.lockscreen_wallpaper = ".config/awesome/resources/share/backgrounds/locked_wallpaper.png"
-- theme.pfp = gears.surface.load_uncached(".config/eww/images/profile.jpg")

theme.systray_icon_spacing = 4
theme.bg_systray = theme.background2

--[[ Bling theme variables template
This file has all theme variables of the bling module.
Every variable has a small comment on what it does.
You might just want to copy that whole part into your theme.lua and start adjusting from there.
--]]

-- window swallowing
theme.dont_swallow_classname_list = { "firefox", "Gimp" } -- list of class names that should not be swallowed
theme.dont_swallow_filter_activated = true -- whether the filter above should be active

-- flash focus
theme.flash_focus_start_opacity = 0.6 -- the starting opacity
theme.flash_focus_step = 0.01 -- the step of animation

-- playerctl signal
theme.playerctl_backend = "playerctl_cli" -- backend to use
theme.playerctl_ignore = {} -- list of players to be ignored
theme.playerctl_player = {} -- list of players to be used in priority order
theme.playerctl_update_on_activity = true -- whether to prioritize the most recently active players or not
theme.playerctl_position_update_interval = 1 -- the update interval for fetching the position from playerctl

-- tabbed
theme.tabbed_spawn_in_tab = false -- whether a new client should spawn into the focused tabbing container

-- tabbar general
theme.tabbar_ontop = false
theme.tabbar_radius = 0 -- border radius of the tabbar
theme.tabbar_style = "default" -- style of the tabbar ("default", "boxes" or "modern")
theme.tabbar_font = "Sans 11" -- font of the tabbar
theme.tabbar_size = 40 -- size of the tabbar
theme.tabbar_position = "top" -- position of the tabbar
theme.tabbar_bg_normal = theme.background -- background color of the focused client on the tabbar
theme.tabbar_fg_normal = theme.foreground -- foreground color of the focused client on the tabbar
theme.tabbar_bg_focus = theme.background1 -- background color of unfocused clients on the tabbar
theme.tabbar_fg_focus = theme.blue -- foreground color of unfocused clients on the tabbar
theme.tabbar_bg_focus_inactive = nil -- background color of the focused client on the tabbar when inactive
theme.tabbar_fg_focus_inactive = nil -- foreground color of the focused client on the tabbar when inactive
theme.tabbar_bg_normal_inactive = nil -- background color of unfocused clients on the tabbar when inactive
theme.tabbar_fg_normal_inactive = nil -- foreground color of unfocused clients on the tabbar when inactive
theme.tabbar_disable = false -- disable the tab bar entirely

-- mstab
theme.mstab_bar_disable = false -- disable the tabbar
theme.mstab_bar_ontop = false -- whether you want to allow the bar to be ontop of clients
theme.mstab_dont_resize_slaves = false -- whether the tabbed stack windows should be smaller than the
-- currently focused stack window (set it to true if you use
-- transparent terminals. False if you use shadows on solid ones
theme.mstab_bar_padding = "default" -- how much padding there should be between clients and your tabbar
-- by default it will adjust based on your useless gaps.
-- If you want a custom value. Set it to the number of pixels (int)
theme.mstab_border_radius = 0 -- border radius of the tabbar
theme.mstab_bar_height = 40 -- height of the tabbar
theme.mstab_tabbar_position = "top" -- position of the tabbar (mstab currently does not support left,right)
theme.mstab_tabbar_style = "default" -- style of the tabbar ("default", "boxes" or "modern")
-- defaults to the tabbar_style so only change if you want a
-- different style for mstab and tabbed

-- the following variables are currently only for the "modern" tabbar style
theme.tabbar_color_close = "#f9929b" -- chnges the color of the close button
theme.tabbar_color_min = "#fbdf90" -- chnges the color of the minimize button
theme.tabbar_color_float = "#ccaced" -- chnges the color of the float button

-- tag preview widget
theme.tag_preview_widget_border_radius = 16 -- Border radius of the widget (With AA)
theme.tag_preview_client_border_radius = 16 -- Border radius of each client in the widget (With AA)
theme.tag_preview_client_opacity = 1 -- Opacity of each client
theme.tag_preview_client_bg = theme.background -- The bg color of each client
theme.tag_preview_client_border_color = theme.xcolor12 -- The border color of each client
theme.tag_preview_client_border_width = 2 -- The border width of each client
theme.tag_preview_widget_bg = theme.background -- The bg color of the widget
theme.tag_preview_widget_border_color = theme.background -- The border color of the widget
theme.tag_preview_widget_border_width = 3 -- The border width of the widget
theme.tag_preview_widget_margin = 2 -- The margin of the widget

-- task preview widget
theme.task_preview_widget_border_radius = 0 -- Border radius of the widget (With AA)
theme.task_preview_widget_bg = theme.background -- The bg color of the widget
theme.task_preview_widget_border_color = theme.foreground2 -- The border color of the widget
theme.task_preview_widget_border_width = 3 -- The border width of the widget
theme.task_preview_widget_margin = 0 -- The margin of the widget

-- tabbed misc widget(s)
theme.bling_tabbed_misc_titlebar_indicator = {
  layout_spacing = dpi(4),
  icon_size = dpi(20),
  icon_margin = dpi(4),
  bg_color_focus = theme.background2,
  bg_color = theme.background,
  icon_shape = function(cr, w, h)
    gears.shape.rounded_rect(cr, w, h, 0)
  end,
  layout = wibox.layout.fixed.horizontal,
}

theme.progressbar_bg = theme.background2
theme.progressbar_fg = theme.foreground
-- theme.progressbar_shape = gears.shape.rounded_bar
theme.progressbar_border_color = theme.background2
theme.progressbar_border_width = 0
theme.progressbar_bar_shape = gears.shape.rounded_bar
theme.progressbar_bar_border_width = 1
theme.progressbar_bar_border_color = theme.foreground
theme.progressbar_margins = 1
theme.progressbar_paddings = 1

theme.machi_switcher_border_color = theme.darker_bg
-- theme.machi_switcher_border_opacity = 0.25
theme.machi_editor_border_color = theme.darker_bg
-- theme.machi_editor_border_opacity = 0.25
-- theme.machi_editor_active_opacity = 0.25
theme.fade_duration = 250

-- mstab
theme.mstab_bar_disable = false -- disable the tabbar
theme.mstab_bar_ontop = false -- whether you want to allow the bar to be ontop of clients
theme.mstab_dont_resize_slaves = false -- whether the tabbed stack windows should be smaller than the
-- currently focused stack window (set it to true if you use
-- transparent terminals. False if you use shadows on solid ones
theme.mstab_bar_padding = "default" -- how much padding there should be between clients and your tabbar
-- by default it will adjust based on your useless gaps.
-- If you want a custom value. Set it to the number of pixels (int)
theme.mstab_border_radius = 12 -- border radius of the tabbar
theme.mstab_bar_height = 24 -- height of the tabbar
theme.mstab_tabbar_position = "top" -- position of the tabbar (mstab currently does not support left,right)
theme.mstab_tabbar_style = "default" -- style of the tabbar ("default", "boxes" or "modern")
-- defaults to the tabbar_style so only change if you want a
-- different style for mstab and tabbed

theme.tasklist_shape = function(cr, w, h)
  gears.shape.rounded_rect(cr, w, h, dpi(2))
end
theme.tasklist_bg_focus = theme.blue
theme.tasklist_bg_normal = theme.background1
theme.tasklist_bg_urgent = theme.orange
theme.tasklist_bg_minimize = theme.gray

theme.task_preview_widget_border_radius = 16 -- Border radius of the widget (With AA)
theme.task_preview_widget_bg = theme.background -- The bg color of the widget
theme.task_preview_widget_border_color = theme.background -- The border color of the widget
theme.task_preview_widget_border_width = 3 -- The border width of the widget
theme.task_preview_widget_margin = 8 -- The margin of the widget

return theme
