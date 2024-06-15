local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local xresources = require("beautiful.xresources")
local beautiful = require("beautiful")
local filesystem = gears.filesystem
local icon_dir = filesystem.get_configuration_dir() .. "/icons/"
local dpi = xresources.apply_dpi
local M = {}
function M.rounded_shape(r)
    return function(cr, w, h)
        gears.shape.rounded_rect(cr, w, h, dpi(r))
    end
end

function M.colorize_text(txt, fg)
    return "<span foreground='" .. fg .. "'>" .. txt .. "</span>"
end

function M.rounded_box(inner, bg, r, margins)
    return {
        {
            inner,
            margins = margins,
            widget = wibox.container.margin,
        },
        bg = bg or beautiful.background,
        shape = M.rounded_shape(r or 16),
        widget = wibox.container.background,
    }
end

function M.big_block(inner, bg, r)
    return {
        {
            { inner, widget = wibox.container.margin },
            bg = bg or beautiful.background,
            shape = M.rounded_shape(r or 16),
            widget = wibox.container.background,
        },
        margins = 0,
        widget = wibox.container.margin,
    }
end

function M.big_block1(inner, bg)
    return {
        {
            { inner, margins = 8, widget = wibox.container.margin },
            bg = bg or beautiful.background1,
            shape = M.rounded_shape(16),
            widget = wibox.container.background,
        },
        margins = 8,
        widget = wibox.container.margin,
    }
end

function M.block(inner)
    return {
        {
            { inner, left = 8, right = 8, widget = wibox.container.margin },
            bg = beautiful.background,
            shape = M.rounded_shape(8),
            widget = wibox.container.background,
        },
        top = 7,
        left = 8,
        right = 8,
        widget = wibox.container.margin,
    }
end

function M.block1(inner)
    return {
        {
            inner,
            margins = 2,
            widget = wibox.container.margin,
        },
        bg = beautiful.background1,
        shape = M.rounded_shape(3),
        widget = wibox.container.background,
    }
end

function M.svg(icon, args)
    args = args or {}
    return {
        image = icon_dir .. icon,
        resize = true,
        halign = "center",
        valign = "center",
        forced_width = args.width or 24,
        forced_height = args.height or 24,
        widget = wibox.widget.imagebox,
    }
end

function M.symbol(symbol, args)
    args = args or {}
    return {
        text = symbol,
        align = "center",
        valign = "center",
        font = args.font,
        widget = wibox.widget.textbox,
    }
end

function M.big_button(inner, callback, arg)
    arg = arg or {}
    local widgets = wibox.widget({
        {
            inner,
            margins = 4,
            widget = wibox.container.margin,
        },
        shape = arg.shape or M.rounded_shape(3),
        bg = arg.bg_default,
        widget = wibox.container.background,
    })
    widgets:connect_signal("mouse::enter", function(widget)
        widget.bg = arg.bg or beautiful.background0
    end)
    widgets:connect_signal("mouse::leave", function(widget)
        widget.bg = arg.bg_default or "#00000000"
    end)
    if callback then
        widgets:connect_signal("mouse::press", callback)
        widgets:buttons(gears.table.join(awful.button({}, 1, nil, callback)))
    end
    return widgets
end

function M.button(inner, callback, bg)
    bg = bg or wibox.container.background1
    local widgets = wibox.widget({
        {
            inner,
            left = 8,
            right = 8,
            widget = wibox.container.margin,
        },
        shape = M.rounded_shape(8),
        bg = bg,
        widget = wibox.container.background,
    })
    widgets:connect_signal("mouse::enter", function(widget)
        widget.bg = beautiful.blue
    end)
    widgets:connect_signal("mouse::leave", function(widget)
        widget.bg = bg
    end)
    if callback then
        widgets:connect_signal("mouse::press", callback)
        widgets:buttons(gears.table.join(awful.button({}, 1, nil, callback)))
    end
    return widgets
end

function M.map(list, f, table)
    table = table or {}
    for k, v in pairs(list) do
        print(k, v)
        table[k] = f(v, k)
    end
    return table
end

return M
