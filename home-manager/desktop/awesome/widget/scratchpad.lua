local bling = require("module.bling")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local rubato = require("module.rubato")
local awful = require("awful")

local chat_anim = {
    y = rubato.timed({
        pos = -1090,
        rate = 120,
        easing = rubato.quadratic,
        intro = 0.1,
        duration = 0.2,
        awestore_compat = true,
    }),
}

local terminal_scratch = bling.module.scratchpad:new({
    command = [[alacritty --class=alacrittydrop -e zellij attach drop -c]],
    rule = { instance = "alacrittydrop" },
    sticky = true,
    autoclose = false,
    floating = true,
    geometry = {
        x = math.floor(1920 * 0.05),
        y = math.floor(1080 * 0.05),
        height = math.floor(1080 * 0.9),
        width = math.floor(1920 * 0.9),
    },
    reapply = true,
    -- rubato = chat_anim,
})

awesome.connect_signal("scratch::chat", function()
    terminal_scratch:toggle()
end)
awful.keyboard.append_global_keybindings({
    awful.key({ mod }, "x", function()
        local screen = awful.screen.focused({})
        terminal_scratch.geometry = {
            x = screen.geometry.x + math.floor(screen.geometry.width * 0.05),
            y = screen.geometry.y + math.floor(screen.geometry.height * 0.05),
            height = math.floor(screen.geometry.height * 0.9),
            width = math.floor(screen.geometry.width * 0.9),
        }
        terminal_scratch:toggle()
    end, { description = "open drop terminal", group = "bling" }),
})
