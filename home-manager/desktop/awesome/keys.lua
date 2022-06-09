local awful = require("awful")
local apps = require("apps")
mod = "Mod4"
local alt = "Mod1"
local ctrl = "Control"
local shift = "Shift"

awful.keyboard.append_global_keybindings({
  awful.key({ mod }, "r", awesome.restart, { description = "reload awesome", group = "WM" }),

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
          .. " -e tmuxinator s 'workspace-"
          .. tag.index
          .. "'"
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
    awful.spawn(apps.default.app_launcher)
  end, { description = "open app launcher", group = "app" }),
  awful.key({ mod }, "e", function()
    local a
    a()
  end, { description = "open app launcher", group = "app" }),
  awful.key({ mod }, "p", function()
    awful.spawn("gpaste-client ui")
  end, { description = "open app launcher", group = "app" }),
  awful.key({ mod }, "q", function()
    if client.focus then
      client.focus:kill()
    end
  end, { description = "open app launcher", group = "app" }),
  awful.key({ mod, shift }, "q", awesome.quit, { description = "quit awesome", group = "WM" }),
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
    awful.client.focus.history.previous()
    if client.focus then
      client.focus:raise()
    end
  end, { description = "go back", group = "client" }),
  awful.key({ mod, ctrl }, "j", function()
    awful.screen.focus_relative(1)
  end, { description = "focus the next screen", group = "screen" }),
  awful.key({ mod, ctrl }, "k", function()
    awful.screen.focus_relative(-1)
  end, { description = "focus the previous screen", group = "screen" }),
  awful.key({ mod }, "l", function()
    awesome.emit_signal("signal::lock")
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
  awful.key({ mod }, "l", function()
    awful.tag.incmwfact(0.05)
  end, { description = "increase master width factor", group = "layout" }),
  awful.key({ mod }, "h", function()
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