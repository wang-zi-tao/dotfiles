local awful = require("awful")
local beautiful = require("beautiful")
awful.rules.rules = {
    -- All clients will match this rule.
    {
        rule = {},
        properties = {
            border_width = beautiful.border_width,
            border_color = beautiful.border_normal,
            focus = awful.client.focus.filter,
            raise = true,
            keys = nil,
            buttons = nil,
            screen = awful.screen.preferred,
            -- placement = awful.placement.centered,
        },
    },
    {
        rule = { class = { "rustdesk", "RustDesk" }, name = "RustDesk" },
        properties = { tag = "7" },
    },
    {
        rule = { class = "xiezuo" },
        properties = { tag = "6" },
    },
    {
        rule = { class = "xz_helper" },
        properties = {
            focusable = false,
            placement = function(d)
                awful.placement.top(d, { margins = 50 })
            end,
        },
    },
    {
        rule = {
            class = { "Virt-manager", "virt-manager" },
            name = "win10 - QEMU/KVM",
        },
        properties = { tag = "8" },
    },
    {
        rule = { class = "rdesktop" },
        properties = {
            maximized = true,
            tag = "7",
        },
    },
    {
        rule = { class = "Gpaste-ui" },
        properties = {
            placement = awful.placement.centered,
        },
    },
    -- Floating clients.
    {
        rule_any = {
            instance = {
                "DTA", -- Firefox addon DownThemAll.
                "copyq", -- Includes session name in class.
                "pinentry",
            },
            class = {
                "Arandr",
                "Blueman-manager",
                "Gpick",
                "Kruler",
                "MessageWin", -- kalarm.
                "Sxiv",
                "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
                "Wpa_gui",
                "veromix",
                "xtightvncviewer",
                "Nextcloud",
                "feh",
                "kdeconnect.app",
                "VirtualBox Manager",
                "Gnome-system-monitor",
                "kdeconnect-app",
                "org.jackhuang.hmcl.Launcher",
                "dolphin",
                "org.gnome.Nautilus",
            },

            -- Note that the name property shown in xprop might be set slightly after creation of the client
            -- and the name shown there might not match defined rules here.
            name = {
                "Event Tester", -- xev.
            },
            role = {
                "AlarmWindow", -- Thunderbird's calendar.
                "ConfigManager", -- Thunderbird's about:config.
                "pop-up", -- e.g. Google Chrome's (detached) Developer Tools.
            },
            type = {
                "dialog",
            },
        },
        properties = { floating = true },
    },

    -- Add titlebars to normal clients and dialogs
    { rule_any = { type = { "normal", "dialog" } }, properties = { titlebars_enabled = false } },

    { rule = { class = "firefox" }, properties = { opacity = 1, maximized = false, floating = false } },
    {
        rule = {
            instance = "alacrittydrop",
        },
        callback = function(c)
            c.skip_taskbar = true
        end,
    },

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },
}
