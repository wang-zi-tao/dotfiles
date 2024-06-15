local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local util = require("widget.util")
local apps = require("apps")
local bling = require("module.bling")
bling.widget.task_preview.enable({
    x = 20, -- The x-coord of the popup
    y = 20, -- The y-coord of the popup
    height = 450, -- The height of the popup
    width = 600, -- The width of the popup
    placement_fn = function(c) -- Place the widget using awful.placement (this overrides x & y)
        awful.placement.top_left(c, {
            margins = {
                top = 40,
                left = 8,
            },
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
    },
})
return function(s)
    local tasklist = awful.widget.tasklist({
        screen = s,
        -- filter          = awful.widget.tasklist.filter.currenttags,
        filter = awful.widget.tasklist.filter.alltags,
        -- buttons         = tasklist_buttons,
        layout = {
            spacing = 1,
            layout = wibox.layout.fixed.horizontal,
        },
        -- Notice that there is *NO* wibox.wibox prefix, it is a template,
        -- not a widget instance.
        widget_template = {
            {
                awful.widget.clienticon,
                margins = 2,
                widget = wibox.container.margin,
            },
            id = "background_role",
            bg = beautiful.background2,
            shape = util.rounded_shape(8),
            create_callback = function(self, c, _, _)
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
                self:connect_signal("mouse::enter", function()
                    awesome.emit_signal("bling::task_preview::visibility", s, true, c)
                end)
                self:connect_signal("mouse::leave", function()
                    awesome.emit_signal("bling::task_preview::visibility", s, false, c)
                end)
            end,
            layout = wibox.container.background,
        },
    })
    return tasklist
end
