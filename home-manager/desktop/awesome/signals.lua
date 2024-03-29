local awful = require("awful")
awesome.connect_signal("tag::last::nonempty", function()
    local t = awful.screen.focused().selected_tag
    local start_index = t.index
    local index = start_index
    local tags = awful.screen.focused().tags
    local tag
    repeat
        index = (index + #tags - 2) % #tags + 1
        tag = tags[index]
    until index == start_index or tag and 0 ~= #tag:clients()
    if tag then
        tag:view_only()
    end
end)
awesome.connect_signal("tag::next::nonempty", function()
    local t = awful.screen.focused().selected_tag
    local start_index = t.index
    local index = start_index
    local tags = awful.screen.focused().tags
    local tag
    repeat
        index = index % #tags + 1
        tag = tags[index]
    until index == start_index or tag and 0 ~= #tag:clients()
    if tag then
        tag:view_only()
    end
end)
