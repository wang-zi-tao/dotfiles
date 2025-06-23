local awful = require("awful")
local apps = require("apps")
local machi = require("module.layout-machi")
local launcher = require("widget.launcher")
local root = require("root")
mod = "Mod4"
alt = "Mod1"
ctrl = "Control"
shift = "Shift"

awful.keyboard.append_global_keybindings({
    awful.key({ mod }, "r", function()
        -- lock_screen = require("widget.lockscreen")
        -- if lock_screen.visible then
        awesome.restart()
        -- end
    end, { description = "reload awesome", group = "WM" }),

    awful.key({ mod }, " ", function()
        awful.layout.inc(1)
    end, { description = "next layout", group = "workspace" }),

    awful.key({ mod, shift }, " ", function()
        awful.layout.inc(-1)
    end, { description = "previous layout", group = "workspace" }),

    awful.key({ mod }, "Return", function()
        for _, tag in pairs(awful.screen.focused().selected_tags) do
            awful.spawn(
                apps.default.terminal
                .. " --title=alacritty-workspace-"
                .. tag.index
                .. " -e zellij attach -c workspace-"
                .. tag.index
            )
            break
        end
    end, { description = "open terminal", group = "app" }),

    awful.key({ mod, ctrl }, "Return", function()
        awful.spawn(apps.default.terminal .. " -e zsh")
    end, { description = "open terminal", group = "app" }),

    awful.key({ mod }, "b", function()
        awful.spawn(apps.default.broswer)
    end, { description = "open broswer", group = "app" }),

    awful.key({ mod }, "d", function()
        launcher:toggle()
    end, { description = "open app launcher", group = "app" }),
    awful.key({ mod }, "s", function()
        awful.spawn("rofi -combi-modi window -show combi -modi combi -theme ~/.config/rofi/apps.css")
    end, { description = "open app launcher", group = "app" }),
    awful.key({ mod }, "p", function()
        awful.spawn("gpaste-client ui")
    end, { description = "open gpaste", group = "app" }),
    awful.key({ mod }, "q", function()
        if client.focus then
            client.focus:kill()
        end
    end, { description = "open app launcher", group = "app" }),
    awful.key({ mod }, "m", function()
        local focus = client.focus
        if focus then
            focus.maximized = not focus.maximized
        end
    end, { description = "maximized", group = "app" }),
    awful.key({ mod, shift }, "q", awesome.quit, { description = "quit awesome", group = "WM" }),
    awful.key({ mod, ctrl }, "q", function()
        awesome.emit_signal("module::exit_screen:show")
    end, { description = "power", group = "WM" }),
    awful.key({ mod }, ".", function()
        machi.default_editor.start_interactive(awful.screen.focused())
    end, { description = "edit the current layout if it is a machi layout", group = "layout" }),
    awful.key({ mod }, "/", function()
        machi.switcher.start(client.focus)
    end, { description = "switch between windows for a machi layout", group = "layout" }),
})
for i = 1, 9 do
    awful.keyboard.append_global_keybindings({
        awful.key({ mod }, "#" .. i + 9, function()
            local screen = awful.screen.focused()
            local tag = screen.tags[i]
            if tag then
                tag:view_only()
            end
        end, { description = "view tag #" .. i, group = "tag" }),
        -- Toggle tag display.
        awful.key({ mod, "Control" }, "#" .. i + 9, function()
            local screen = awful.screen.focused()
            local tag = screen.tags[i]
            if tag then
                awful.tag.viewtoggle(tag)
            end
        end, { description = "toggle tag #" .. i, group = "tag" }),
        -- Move client to tag.
        awful.key({ mod, "Shift" }, "#" .. i + 9, function()
            if client.focus then
                local tag = client.focus.screen.tags[i]
                if tag then
                    client.focus:move_to_tag(tag)
                end
            end
        end, { description = "move focused client to tag #" .. i, group = "tag" }),
        -- Toggle tag on focused client.
        awful.key({ mod, "Control", "Shift" }, "#" .. i + 9, function()
            if client.focus then
                local tag = client.focus.screen.tags[i]
                if tag then
                    client.focus:toggle_tag(tag)
                end
            end
        end, { description = "toggle focused client on tag #" .. i, group = "tag" }),
    })
end
awful.keyboard.append_global_keybindings({
    awful.key({ mod }, "j", function()
        awful.client.focus.byidx(1)
    end, { description = "focus next by index", group = "client" }),
    awful.key({ mod }, "k", function()
        awful.client.focus.byidx(-1)
    end, { description = "focus previous by index", group = "client" }),
    awful.key({ mod }, "Tab", function()
        awesome.emit_signal("signal::window_switcher::open")
    end, { description = "go back", group = "client" }),
    awful.key({ shift, mod }, "Tab", function()
        awful.tag.history.restore(awful.screen.focused(), 1)
    end, { description = "go back", group = "client" }),
    awful.key({ alt }, "Tab", function()
        awesome.emit_signal("signal::window_switcher::open")
    end, { description = "Window Switcher", group = "client" }),
    awful.key({ mod }, "f", function()
        local c = client.focus
        if c then
            c.floating = not c.floating
        end
    end, { description = "float window", group = "client" }),
    awful.key({ mod, shift }, "f", function()
        local c = client.focus
        if c then
            if c.fullscreen then
                c.fullscreen = false
                c.maximized = c.maximized_backup or false
                c.maximized_backup = nil
            else
                c.fullscreen = true
                c.maximized_backup = c.maximized
                c.maximized = true
            end
        end
    end, { description = "fullscreen", group = "client" }),
    awful.key({ mod, ctrl }, "j", function()
        awful.screen.focus_relative(1)
    end, { description = "focus the next screen", group = "screen" }),
    awful.key({ mod, ctrl }, "k", function()
        awful.screen.focus_relative(-1)
    end, { description = "focus the previous screen", group = "screen" }),
    awful.key({ mod }, "l", function()
        awesome.emit_signal("signal::lock")
    end, { description = "focus the previous screen", group = "screen" }),
    awful.key({ mod, ctrl }, "l", function()
        awesome.emit_signal("signal::lock")
        awful.spawn("systemctl suspend")
    end, { description = "focus the previous screen", group = "screen" }),
    awful.key({ mod, ctrl }, "n", function()
        local c = awful.client.restore()
        -- Focus restored client
        if c then
            c:activate({ raise = true, context = "key.unminimize" })
        end
    end, { description = "restore minimized", group = "client" }),
})

-- Layout related keybindings
awful.keyboard.append_global_keybindings({
    awful.key({ mod, "Shift" }, "j", function()
        awful.client.swap.byidx(1)
    end, { description = "swap with next client by index", group = "client" }),
    awful.key({ mod, "Shift" }, "k", function()
        awful.client.swap.byidx(-1)
    end, { description = "swap with previous client by index", group = "client" }),
    awful.key({ mod }, "u", awful.client.urgent.jumpto, { description = "jump to urgent client", group = "client" }),
    awful.key({ mod }, "KP_Next", function()
        awesome.emit_signal("tag::next::nonempty")
    end, { description = "next tagg", group = "layout" }),
    awful.key({ mod }, "Right", function()
        awesome.emit_signal("tag::next::nonempty")
    end, { description = "next tagg", group = "layout" }),
    awful.key({ mod }, "KP_Prior", function()
        awesome.emit_signal("tag::last::nonempty")
    end, { description = "previous tag", group = "layout" }),
    awful.key({ mod }, "Left", function()
        awesome.emit_signal("tag::last::nonempty")
    end, { description = "previous tag", group = "layout" }),
    awful.key({ mod, alt }, "l", function()
        awful.tag.incmwfact(0.05)
    end, { description = "increase master width factor", group = "layout" }),
    awful.key({ mod }, "Up", function()
        awful.tag.incmwfact(0.05)
    end, { description = "increase master width factor", group = "layout" }),
    awful.key({ mod, alt }, "h", function()
        awful.tag.incmwfact(-0.05)
    end, { description = "decrease master width factor", group = "layout" }),
    awful.key({ mod }, "Down", function()
        awful.tag.incmwfact(-0.05)
    end, { description = "decrease master width factor", group = "layout" }),
    awful.key({ mod, "Shift" }, "h", function()
        awful.tag.incnmaster(1, nil, true)
    end, { description = "increase the number of master clients", group = "layout" }),
    awful.key({ mod, "Shift" }, "l", function()
        awful.tag.incnmaster(-1, nil, true)
    end, { description = "decrease the number of master clients", group = "layout" }),
    awful.key({ mod, "Control" }, "h", function()
        awful.tag.incncol(1, nil, true)
    end, { description = "increase the number of columns", group = "layout" }),
    awful.key({ mod, "Control" }, "l", function()
        awful.tag.incncol(-1, nil, true)
    end, { description = "decrease the number of columns", group = "layout" }),
    awful.key({ mod }, "space", function()
        awful.layout.inc(1)
    end, { description = "select next", group = "layout" }),
    awful.key({ mod, "Shift" }, "space", function()
        awful.layout.inc(-1)
    end, { description = "select previous", group = "layout" }),
})

client.connect_signal("request::default_mousebindings", function()
    awful.mouse.append_client_mousebindings({
        awful.button({}, 1, function(c)
            c:activate({ context = "mouse_click" })
        end),
        awful.button({ mod }, 1, function(c)
            c:activate({ context = "mouse_click", action = "mouse_move" })
        end),
        awful.button({ mod }, 3, function(c)
            c:activate({ context = "mouse_click", action = "mouse_resize" })
        end),
    })
end)
root.buttons(awful.util.table.join(awful.button({}, 4, last_tag), awful.button({}, 5, next_tag)))
