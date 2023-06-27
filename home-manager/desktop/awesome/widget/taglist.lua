local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local util = require("widget.util")
local bling = require("module.bling")
local icon_black_list = { rdesktop = 1, xz_helper = 1, qq = 1 }
local window_class_black_list = { xz_helper = 1 }
local workspace_names = { " ", " ", "", " ", "", "", " ", " ", "" }
bling.widget.tag_preview.enable({
    show_client_content = true, -- Whether or not to show the client content x = 30, -- The x-coord of the popup
    y = 30, -- The y-coord of the popup
    scale = 0.3, -- The scale of the previews compared to the screen
    honor_padding = true, -- Honor padding when creating widget size
    honor_workarea = true, -- Honor work area when creating widget size
    placement_fn = function(c) -- Place the widget using awful.placement (this overrides x & y)
        awful.placement.top_left(c, {
            margins = {
                top = 40,
                left = 8,
            },
        })
    end,
    background_widget = wibox.widget({ -- Set a background image (like a wallpaper) for the widget
        image = beautiful.lockscreen_wallpaper,
        horizontal_fit_policy = "full",
        vertical_fit_policy = "full",
        widget = wibox.widget.imagebox,
    }),
})
bling.widget.task_preview.enable {
    x = 20, -- The x-coord of the popup
    y = 20, -- The y-coord of the popup
    height = 450, -- The height of the popup
    width = 600, -- The width of the popup
    placement_fn = function(c) -- Place the widget using awful.placement (this overrides x & y)
        awful.placement.top_left(c, {
            margins = {
                top = 40,
                left = 2,
            }
        })
    end,
    -- Your widget will automatically conform to the given size due to a constraint container.
    structure = {
        {
            {
                {
                    {
                        id = "icon_role",
                        resize = true,
                        forced_height = dpi(28),
                        forced_width = dpi(28),
                        widget = wibox.widget.imagebox,
                    },
                    {
                        {
                            id = "name_role",
                            align = "center",
                            ellipsize = "end",
                            widget = wibox.widget.textbox,
                        },
                        left = dpi(4),
                        right = dpi(4),
                        widget = wibox.container.margin,
                    },
                    layout = wibox.layout.align.horizontal,
                },
                {
                    {
                        {
                            id = "image_role",
                            resize = true,
                            clip_shape = util.rounded_shape(16),
                            widget = wibox.widget.imagebox,
                        },
                        valign = "center",
                        halign = "center",
                        widget = wibox.container.place,
                    },
                    top = 8 * 0.25,
                    widget = wibox.container.margin,
                },
                fill_space = true,
                layout = wibox.layout.fixed.vertical,
            },
            margins = 8,
            widget = wibox.container.margin,
        },
        bg = beautiful.background,
        shape_border_width = 3,
        shape_border_color = beautiful.blue,
        shape = util.rounded_shape(16),
        widget = wibox.container.background,
    }
}
return function(s)
    return awful.widget.taglist({
        screen          = s,
        filter          = function(tag)
            return (awful.widget.taglist.filter.noempty(tag))
        end,
        style           = {
            shape = gears.shape.rounded_bar,
            shape_border_width = 2,
            shape_border_color = beautiful.blue,
        },
        layout          = {
            spacing = 8,
            layout = wibox.layout.fixed.horizontal,
        },
        widget_template = {
            {
                {
                    {
                        {
                            id = "index_role",
                            align = "center",
                            valign = "center",
                            forced_width = 24,
                            widget = wibox.widget.textbox,
                        },
                        right = 2,
                        left = 8,
                        widget = wibox.container.margin,
                    },
                    {
                        {
                            {
                                id     = "inner",
                                layout = wibox.layout.fixed.horizontal,
                            },
                            -- left = 2,
                            -- -- right = 2,
                            -- bottom = 1,
                            -- top = 1,
                            widget = wibox.container.margin,
                        },
                        bg = beautiful.background,
                        shape = gears.shape.rounded_bar,
                        widget = wibox.container.background,
                    },
                    widget = wibox.layout.fixed.horizontal,
                },
                left = 0,
                right = 4,
                top = 2,
                bottom = 2,
                widget = wibox.container.margin,
            },
            id = "background_role",
            widget = wibox.container.background,
            -- Add support for hover colors and an index label
            create_callback = function(self, tag, index, objects) --luacheck: no unused args
                self:get_children_by_id("index_role")[1].text = workspace_names[tag.index]
                self:get_children_by_id("inner")[1]:setup {
                    awful.widget.tasklist {
                        screen          = s,
                        filter          = function(c)
                            return window_class_black_list[c.class] == nil;
                        end,
                        source          = function()
                            return tag:clients()
                        end,
                        style           = {
                            shape     = gears.shape.rounded_bar,
                            -- shape_border_width_focus = 1,
                            -- shape_border_color_focus = beautiful.blue,
                            bg_focus  = beautiful.background,
                            bg_normal = beautiful.background1,
                        },
                        layout          = {
                            spacing = 1,
                            layout  = wibox.layout.fixed.horizontal
                        },
                        -- Notice that there is *NO* wibox.wibox prefix, it is a template,
                        -- not a widget instance.
                        widget_template = {
                            {
                                {
                                    id = "clienticon",
                                    widget = awful.widget.clienticon,
                                },
                                margins = 2,
                                widget  = wibox.container.margin
                            },
                            id              = "background_role",
                            bg              = beautiful.background2,
                            forced_width    = 25,
                            forced_height   = 25,
                            create_callback = function(self, c, _, _)
                                if (icon_black_list[c.class]) then
                                    local clienticon = self:get_children_by_id("clienticon")[1]
                                    clienticon.visible = false
                                end
                                self:connect_signal("button::press", function()
                                    awesome.emit_signal("bling::task_preview::visibility", s, false, c)
                                    if not c.active then
                                        c:activate({
                                            context = "through_dock",
                                            switch_to_tag = true,
                                        })
                                    else
                                        c.minimized = true
                                    end
                                end)
                                self:connect_signal('mouse::enter', function()
                                    awesome.emit_signal("bling::tag_preview::visibility", s, false)
                                    awesome.emit_signal("bling::task_preview::visibility", s, true, c)
                                end)
                                self:connect_signal('mouse::leave', function()
                                    awesome.emit_signal("bling::task_preview::visibility", s, false, c)
                                end)
                            end,
                            layout          = wibox.container.background
                        },
                    },
                    layout = wibox.layout.fixed.horizontal,
                }
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
                awesome.emit_signal("tag::next::nonempty")
            end),
            awful.button({}, 5, function(t)
                awesome.emit_signal("bling::tag_preview::visibility", s, false)
                awesome.emit_signal("tag::last::nonempty")
            end)
        ),
    })
end
