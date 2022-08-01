local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local menubar = require("menubar")
local naughty = require("naughty")
local util = require("widget.util")
local gears = require("gears")
naughty.connect_signal("request::icon", function(n, context, hints)
  if context ~= "app_icon" then
    return
  end

  local path = menubar.utils.lookup_icon(hints.app_icon) or menubar.utils.lookup_icon(hints.app_icon:lower())

  if path then
    n.icon = path
  end
end)

naughty.config.defaults.ontop = true
naughty.config.defaults.timeout = 5
naughty.config.defaults.title = "Notification"
naughty.config.defaults.position = "top_middle"

naughty.config.presets.low.timeout = 3
naughty.config.presets.normal.timeout = 5
naughty.config.presets.critical.timeout = 1

naughty.config.presets.normal = {
  font = beautiful.font_name .. "Regular 12",
  fg = beautiful.fg_normal,
  bg = beautiful.bg_normal,
}

naughty.config.presets.low = {
  font = beautiful.font_name .. "Regular 12",
  fg = beautiful.fg_normal,
  bg = beautiful.bg_normal,
}

naughty.config.presets.critical = {
  font = beautiful.font_name .. "Bold 12",
  fg = beautiful.xcolor1,
  bg = beautiful.bg_normal,
}

naughty.config.presets.ok = naughty.config.presets.normal
naughty.config.presets.info = naughty.config.presets.normal
naughty.config.presets.warn = naughty.config.presets.critical
local function notify_widget(notify)
  return util.rounded_box({
    {
      util.rounded_box({
        {
          {
            {
              image = notify.appicon,
              resize = true,
              widget = wibox.widget.imagebox,
            },
            strategy = "max",
            height = dpi(20),
            widget = wibox.container.constraint,
          },
          right = dpi(10),
          widget = wibox.container.margin,
        },
        {
          markup = notify.app_name,
          align = "left",
          font = beautiful.font_name .. "Bold 12",
          widget = wibox.widget.textbox,
        },
        {
          markup = notify.time,
          align = "right",
          font = beautiful.font_name .. "Bold 12",
          widget = wibox.widget.textbox,
        },
        layout = wibox.layout.align.horizontal,
      }, beautiful.background1, dpi(12), dpi(8)),
      {
        {
          {
            {
              -- util.vertical_pad(10),
              {
                {
                  step_function = wibox.container.scroll.step_functions.waiting_nonlinear_back_and_forth,
                  speed = 50,
                  {
                    markup = notify.title,
                    font = beautiful.font_name .. "Bold 11",
                    align = "left",
                    widget = wibox.widget.textbox,
                  },
                  forced_width = dpi(400),
                  widget = wibox.container.scroll.horizontal,
                },
                {
                  step_function = wibox.container.scroll.step_functions.waiting_nonlinear_back_and_forth,
                  speed = 50,
                  {
                    markup = notify.message,
                    align = "left",
                    font = beautiful.font_name .. "Regular 11",
                    widget = wibox.widget.textbox,
                  },
                  forced_width = dpi(210),
                  widget = wibox.container.scroll.horizontal,
                },
                spacing = 0,
                layout = wibox.layout.flex.vertical,
              },
              -- util.vertical_pad(10),
              layout = wibox.layout.align.vertical,
            },
            left = dpi(20),
            right = dpi(20),
            widget = wibox.container.margin,
          },
          {
            {
              nil,
              {
                {
                  image = notify.icon,
                  resize = true,
                  -- clip_shape = util.rrect(beautiful.border_radius),
                  widget = wibox.widget.imagebox,
                },
                strategy = "max",
                height = dpi(40),
                widget = wibox.container.constraint,
              },
              nil,
              expand = "none",
              layout = wibox.layout.align.vertical,
            },
            margins = dpi(10),
            widget = wibox.container.margin,
          },
          layout = wibox.layout.fixed.horizontal,
        },
        {
          {
            {
              notification = notify,
              base_layout = wibox.widget({
                spacing = dpi(8),
                layout = wibox.layout.flex.horizontal,
              }),
              widget_template = {
                {
                  {
                    id = "text_role",
                    align = "center",
                    valign = "center",
                    font = beautiful.font_name .. "Regular 12",
                    widget = wibox.widget.textbox,
                  },
                  left = dpi(6),
                  right = dpi(6),
                  widget = wibox.container.margin,
                },
                bg = beautiful.background2,
                forced_height = dpi(25),
                forced_width = dpi(20),
                shape = util.rounded_shape(16),
                widget = wibox.container.background,
              },
              style = { underline_normal = false, underline_selected = true },
              widget = naughty.list.actions,
            },
            layout = wibox.layout.fixed.vertical,
          },
          margins = dpi(10),
          visible = notify.actions and #notify.actions > 0,
          widget = wibox.container.margin,
        },
        layout = wibox.layout.fixed.vertical,
      },
      spacing = 8,
      layout = wibox.layout.fixed.vertical,
    },
    margins = 8,
    widget = wibox.container.margin,
  }, beautiful.background, dpi(12))
end

local notify_center_react = require("react")({
  default_states = {
    notifys = {},
  },
  render = function(self)
    if #self.state.notifys ~= 0 then
      return util.big_block1({
        layout = wibox.layout.fixed.vertical,
        -- forced_width = dpi(400),
        {
          {
            util.big_button(util.symbol("﫨"), function()
              self:set_state({ notifys = {} })
            end, {
              shape = gears.shape.rounded_rect,
              bg_default = beautiful.background,
              bg = beautiful.background,
            }),
            layout = wibox.layout.fixed.horizontal,
          },
          margins = dpi(8),
          widget = wibox.container.margin,
        },
        {
          util.map(self.state.notifys, function(notify)
            return notify_widget(notify)
          end, {
            spacing = 8,
            widget = wibox.layout.fixed.vertical,
          }),
          margins = { bottom = dpi(20), right = dpi(20) },
          widget = wibox.container.margin,
        },
      })
    else
      return nil
    end
  end,
})
local notify_center = notify_center_react()
naughty.connect_signal("request::display", function(notify, _)
  notify.time = os.date("%H:%M")
  naughty.layout.box({
    notification = notify,
    type = "notification",
    bg = beautiful.transparent,
    widget_template = notify_widget(notify),
  })
  gears.timer {
    timeout   = notify.timeout,
    call_now  = true,
    autostart = true,
    callback  = function()
      notify:destory()
    end
  }
  local state = notify_center.react.state
  state.notifys[#(notify_center.react.state.notifys or {}) + 1] = notify
  notify_center.react:set_state_force(state)
end)
return notify_center
