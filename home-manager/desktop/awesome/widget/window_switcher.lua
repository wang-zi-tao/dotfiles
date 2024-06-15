local util = require("widget.util")
local cairo = require("lgi").cairo
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local window_switcher_box
local window_switcher_minimized_clients = {}
local window_switcher_first_client
local window_switcher_grabber
local history_index = 3
local function get_num_clients()
    local minimized_clients_in_tag = 0
    local matcher = function(c)
        return awful.rules.match(c, {
            minimized = true,
            skip_taskbar = false,
            hidden = false,
            first_tag = awful.screen.focused().selected_tag,
        })
    end
    for c in awful.client.iterate(matcher) do
        minimized_clients_in_tag = minimized_clients_in_tag + 1
    end
    return minimized_clients_in_tag + #awful.screen.focused().clients
end

local function hide()
    window_switcher_box.visible = false
    if client.focus then
        local window_switcher_last_client = client.focus
        awful.client.focus.history.add(window_switcher_last_client)
        if window_switcher_first_client and window_switcher_first_client.valid then
            window_switcher_first_client:raise()
            window_switcher_last_client:raise()
        end
    end
    local s = awful.screen.focused()
    for _, c in pairs(window_switcher_minimized_clients) do
        if c and c.valid and not (client.focus and client.focus == c) then
            c.minimized = true
        end
    end
    window_switcher_minimized_clients = {}
    awful.client.focus.history.enable_tracking()
    awful.keygrabber.stop(window_switcher_grabber)
    collectgarbage("collect")
end

local keyboard_keys = {
    Escape = function()
        hide()
    end,

    h = function()
        if client.focus then
            client.focus.minimized = true
        end
    end,
    H = function()
        if awful.client.restore() then
            client.focus = awful.client.restore()
        end
    end,
    q = function()
        if client.focus then
            client.focus:kill()
        end
    end,

    Tab = function()
        local c = awful.client.focus.history.list[history_index]
        if c then
            client.focus = c
            local t = client.focus and client.focus.first_tag or nil
            if t then
                t:view_only()
            end
            c:raise()
        end
        if history_index >= #awful.client.focus.history.list then
            history_index = 1
        else
            history_index = history_index + 1
        end
    end,

    Left = function()
        awful.client.focus.byidx(1)
    end,
    Next = function()
        awful.client.focus.byidx(-1)
    end,
}
for i = 1, 9 do
    keyboard_keys[tostring(i)] = function()
        local c = awful.client.focus.history.list[i]
        if c then
            client.focus = c
            local t = client.focus and client.focus.first_tag or nil
            if t then
                t:view_only()
            end
            c:raise()
            hide()
        end
    end
end

local function show()
    history_index = 3
    window_switcher_first_client = client.focus
    window_switcher_box.visible = true
    awful.client.focus.history.disable_tracking()
    local c = awful.client.focus.history.list[2]
    if c then
        client.focus = c
        local t = client.focus and client.focus.first_tag or nil
        if t then
            t:view_only()
        end
        c:raise()
    end
    window_switcher_grabber = awful.keygrabber.run(function(_, key, event)
        if event == "release" then
            if key:match("Super") or key:match("Alt") or key:match("Control") then
                hide()
            end
            return
        end
        if keyboard_keys[key] then
            keyboard_keys[key]()
        else
            hide()
        end
    end)
end

local window_switcher_react = require("react")({
    init = function(self)
        awesome.connect_signal("signal::window_switcher::open", function()
            if window_switcher_box.visible then
                hide()
                self:set_state({ enabled = false })
            else
                self:set_state({ enabled = true })
                show()
            end
        end)
    end,
    default_states = { enabled = false },
    render = function(self)
        if self.state.enabled then
            return {
                awful.widget.tasklist({
                    source = function()
                        return awful.client.focus.history.list
                    end,
                    screen = awful.screen.focused(),
                    filter = awful.widget.tasklist.filter.alltags,
                    layout = {
                        forced_num_cols = 4,
                        -- min_cols_size = 400,
                        -- min_rows_size = 300,
                        orientation = "vertical",
                        homogeneous = false,
                        vertical_homogeneous = false,
                        horizontal_homogeneous = false,
                        -- expand = true,
                        horizontal_spacing = 8,
                        vertical_spacing = 8,
                        layout = wibox.layout.grid.horizontal,
                    },
                    style = {
                        bg_normal = beautiful.background,
                    },
                    widget_template = {
                        widget = wibox.container.background,
                        id = "bg_role",
                        create_callback = function(self, c, _, __)
                            local content = nil
                            if c.active then
                                content = gears.surface(c.content)
                            elseif c.prev_content then
                                content = gears.surface(c.prev_content)
                            end
                            local img = nil
                            if content ~= nil then
                                local cr = cairo.Context(content)
                                local x, y, w, h = cr:clip_extents()
                                img = cairo.ImageSurface.create(cairo.Format.ARGB32, w - x, h - y)
                                cr = cairo.Context(img)
                                cr:set_source_surface(content, 0, 0)
                                cr.operator = cairo.Operator.SOURCE
                                cr:paint()
                            end
                            self:get_children_by_id("thumbnail")[1].image = img
                        end,
                        shape = util.rounded_shape(16),
                        {
                            {
                                {
                                    {
                                        {
                                            {
                                                {
                                                    awful.widget.clienticon,
                                                    forced_width = 32,
                                                    forced_height = 32,
                                                    valign = "center",
                                                    widget = wibox.container.place,
                                                },
                                                {
                                                    valign = "center",
                                                    id = "text_role",
                                                    ellipsize = "end",
                                                    font = beautiful.font_name .. "Bold 10",
                                                    forced_height = 32,
                                                    forced_width = 380,
                                                    widget = wibox.widget.textbox,
                                                },
                                                spacing = 8,
                                                layout = wibox.layout.fixed.horizontal,
                                            },
                                            {
                                                {
                                                    halign = "center",
                                                    valign = "center",
                                                    resize = true,
                                                    id = "thumbnail",
                                                    clip_shape = util.rounded_shape(16),
                                                    widget = wibox.widget.imagebox,
                                                },
                                                forced_width = 443,
                                                forced_height = 239,
                                                layout = wibox.layout.fixed.vertical,
                                            },
                                            spacing = 4,
                                            layout = wibox.layout.fixed.vertical,
                                        },
                                        margins = 4,
                                        widget = wibox.container.margin,
                                    },
                                    bg = beautiful.background,
                                    shape = util.rounded_shape(16),
                                    widget = wibox.container.background,
                                },
                                margins = 4,
                                widget = wibox.container.margin,
                            },
                            bg = beautiful.background,
                            shape = util.rounded_shape(20),
                            id = "background_role",
                            widget = wibox.container.background,
                        },
                    },
                    buttons = gears.table.join(
                        awful.button({
                            modifiers = { "Any" },
                            button = 1,
                            on_press = function(c)
                                hide()
                                self:set_state({ enabled = false })
                                if not c.active then
                                    c:activate({
                                        context = "through_windows_switcher",
                                        switch_to_tag = true,
                                    })
                                end
                            end,
                        }),
                        awful.button({
                            modifiers = { "Any" },
                            button = 2,
                            on_press = function(c)
                                c:kill()
                            end,
                        }),
                        awful.button({
                            modifiers = { "Any" },
                            button = 4,
                            on_press = function()
                                awful.client.focus.byidx(-1)
                            end,
                        }),
                        awful.button({
                            modifiers = { "Any" },
                            button = 5,
                            on_press = function()
                                awful.client.focus.byidx(1)
                            end,
                        })
                    ),
                }),
                layout = wibox.layout.fixed.vertical,
            }
        else
            return { layout = wibox.layout.fixed.vertical }
        end
    end,
})
window_switcher_box = awful.popup({
    bg = beautiful.transparent,
    type = "dock",
    visible = false,
    ontop = true,
    placement = awful.placement.centered,
    screen = awful.screen.focused(),
    -- bgimage = beautiful.wallpaper,
    widget = {
        window_switcher_react(),
        shape = util.rounded_shape(16),
        -- bgimage = beautiful.wallpaper,
        -- bg = beautiful.background,
        -- forced_width = 1714,
        -- forced_height = 926,
        widget = wibox.container.background,
    },
})
