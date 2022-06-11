local gears = require("gears")
local awful = require("awful")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local wibox = require("wibox")
local util = require("widget.util")

local gfs = require("gears.filesystem")
local config_dir = gfs.get_configuration_dir()
package.cpath = package.cpath .. ";" .. config_dir .. "module/lua_pam/lib/?.so;"
local pam = require("liblua_pam")
local authenticate = function(password)
  return pam.auth_current_user(password)
end
-- Word Clock Lock Screen
----------------
local lock_screen_symbol = ""
local lock_screen_fail_symbol = ""
local lock_animation_icon = wibox.widget({
  -- Set forced size to prevent flickering when the icon rotates
  forced_height = dpi(80),
  forced_width = dpi(80),
  font = beautiful.icon_font .. "Outlined 40",
  align = "center",
  valign = "center",
  widget = wibox.widget.textbox(lock_screen_symbol),
})

local some_textbox = wibox.widget.textbox()

local lock_screen_box = wibox({ visible = false, ontop = true, type = "splash", screen = screen.primary })
awful.placement.maximize(lock_screen_box)

local function screen_mask(s, bg)
  local mask = wibox({
    visible = false,
    ontop = true,
    type = "splash",
    screen = s,
  })
  awful.placement.maximize(mask)
  mask.bg = bg
  return mask
end

-- Add lockscreen to each screen
awful.screen.connect_for_each_screen(function(s)
  if s == screen.primary then
    s.mylockscreen = lock_screen_box
  else
    s.mylockscreen = screen_mask(s, beautiful.wallpaper)
  end
end)

local function set_visibility(v)
  for s in screen do
    s.mylockscreen.visible = v
  end
end

-- Word Clock
----------------

local time = wibox.widget({
  forced_num_cols = 11,
  spacing = dpi(6),
  layout = wibox.layout.grid,
})

-- Lock animation
local lock_animation_widget_rotate = wibox.container.rotate()

local arc = function()
  return function(cr, width, height)
    gears.shape.arc(cr, width, height, dpi(5), 0, math.pi / 2, true, true)
  end
end

local lock_animation_arc = wibox.widget({
  shape = arc(),
  bg = beautiful.background,
  forced_width = dpi(256),
  forced_height = dpi(256),
  widget = wibox.container.background,
})

local lock_animation = {
  {
    lock_animation_arc,
    widget = lock_animation_widget_rotate,
  },
  lock_animation_icon,
  layout = wibox.layout.stack,
}

-- Lock helper functions
local characters_entered = 0
local function reset()
  characters_entered = 0
  lock_animation_icon.markup = lock_screen_symbol
  lock_animation_widget_rotate.direction = "north"
  lock_animation_arc.bg = "#00000000"
end

local function fail()
  characters_entered = 0
  lock_animation_icon.markup = lock_screen_fail_symbol
  lock_animation_widget_rotate.direction = "north"
  lock_animation_arc.bg = "#00000000"
end

local animation_colors = {
  -- Rainbow sequence =)
  beautiful.xcolor1,
  beautiful.xcolor5,
  beautiful.xcolor4,
  beautiful.xcolor6,
  beautiful.xcolor2,
  beautiful.xcolor3,
}

local animation_directions = { "north", "west", "south", "east" }

-- Function that "animates" every key press
local function key_animation(char_inserted)
  local color
  local direction = animation_directions[(characters_entered % 4) + 1]
  if char_inserted then
    color = animation_colors[(characters_entered % 6) + 1]
    lock_animation_icon.markup = lock_screen_symbol
  else
    if characters_entered == 0 then
      reset()
    else
      color = beautiful.xcolor7 .. "55"
    end
  end

  lock_animation_arc.bg = color
  lock_animation_widget_rotate.direction = direction
end

-- Get input from user
local function grab_password()
  awful.prompt.run({
    hooks = {
      -- Custom escape behaviour: Do not cancel input with Escape
      -- Instead, this will just clear any input received so far.
      {
        {},
        "Escape",
        function(_)
          reset()
          grab_password()
        end,
      },
      -- Fix for Control+Delete crashing the keygrabber
      {
        { "Control" },
        "Delete",
        function()
          reset()
          grab_password()
        end,
      },
    },
    keypressed_callback = function(mod, key, cmd)
      -- Only count single character keys (thus preventing
      -- "Shift", "Escape", etc from triggering the animation)
      if #key == 1 then
        characters_entered = characters_entered + 1
        key_animation(true)
      elseif key == "BackSpace" then
        if characters_entered > 0 then
          characters_entered = characters_entered - 1
        end
        key_animation(false)
      end
    end,
    exe_callback = function(input)
      -- Check input
      if authenticate(input) then
        -- YAY
        reset()
        set_visibility(false)
      else
        -- NAY
        fail()
        grab_password()
      end
    end,
    textbox = some_textbox,
  })
end

local function lock_screen_show()
  set_visibility(true)
  grab_password()
end

awesome.connect_signal("signal::lock", lock_screen_show)
lock_screen_box:setup({
  {
    nil,
    {
      nil,
      {
        {
          resize = false,
          image = beautiful.wallpaper,
          -- forced_height = screen.primary.geometry.height,
          -- forced_width = screen.primary.geometry.width,
          widget = wibox.widget.imagebox,
          shape = gears.shape.rounded_bar,
        },
        spacing = dpi(60),
        layout = wibox.layout.fixed.vertical,
      },
      expand = "outside",
      layout = wibox.layout.align.vertical,
    },
    expand = "outside",
    layout = wibox.layout.align.horizontal,
  },
  {
    nil,
    {
      nil,
      {
        wibox.widget({
          forced_height = dpi(20),
          layout = wibox.layout.fixed.vertical,
        }),
        time,
        lock_animation,
        spacing = dpi(60),
        layout = wibox.layout.fixed.vertical,
      },
      expand = "outside",
      layout = wibox.layout.align.vertical,
    },
    expand = "outside",
    layout = wibox.layout.align.horizontal,
  },
  layout = wibox.layout.stack
})
