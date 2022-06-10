local wibox = require("wibox")
local gears = require("gears")
local function is_part_of(a, b)
  for k, v in pairs(a) do
    if b[k] ~= v then
      return false
    end
  end
  return true
end

local function merge(a, b, ctx)
  ctx = ctx or { history = {} }
  if not ctx.history[b] then
    ctx.history[b] = true
    for k, v in pairs(b) do
      local type = type(v)
      if type == "table" then
        local dst = a[k]
        if dst == nil then
          dst = {}
          a[k] = dst
        end
        merge(dst, v, ctx)
      elseif type == "nil" then
        a[k] = nil
      else
        a[k] = v
      end
    end
  end
end

local function compare_table(a, b)
  return is_part_of(b, a)
end

local M
M = {
  set_state_force = function(self, new_state)
    self.state = new_state
    self.widget:setup(self:render())
  end,
  compare_state = function(old, new)
    return is_part_of(new, old)
  end,
  compare_props = function(old, new)
    return compare_table(old, new)
  end,
  render = function()
    return {
      text = "react",
      widget = wibox.widget.textbox,
    }
  end,
  init = function(props) end,
  set_state = function(self, new_state)
    if not M.compare_state(new_state, self.state) then
      self.state = gears.table.crush(self.state, new_state)
      self.widget:setup(self:render())
    end
  end,
}

local metatable = {}
function metatable.__call(self, args)
  local init = args.init or M.init
  local render = args.render
  local default_props = args.default_props or {}
  local default_states = args.default_states or {}
  local compare_props = args.compare_props or M.compare_props
  -- local compare_state = args.compare_state or M.compare_state
  local index = {
    default_props = default_props,
    default_states = default_states,
    -- compare_state = compare_state,
    compare_props = compare_props,
    render = render,
    init = init,
    set_state = M.set_state,
    set_state_force = M.set_state_force,
  }
  return function(props)
    local widget = wibox.container.margin()
    props = gears.table.merge(props, index.default_props)
    local component = { widget = widget, state = index.default_states, props = props }
    widget.react = component
    setmetatable(component, { __index = index })
    component:init(props)
    widget:setup(component:render())
    return widget
  end
end

local M = {}
setmetatable(M, metatable)
return M
