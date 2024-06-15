-- Weather widget stolen from mofiqul
local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local filesystem = gears.filesystem
local json = require("module.json")
local config = require("config")
local icon_dir = filesystem.get_configuration_dir() .. "/icons/"

local GET_FORECAST_CMD = [[bash -c "curl -s --show-error -X GET '%s'"]]

local icon_map = {
    ["01d"] = "weather-clear-sky",
    ["02d"] = "weather-few-clouds",
    ["03d"] = "weather-clouds",
    ["04d"] = "weather-few-clouds",
    ["09d"] = "weather-showers-scattered",
    ["10d"] = "weather-showers",
    ["11d"] = "weather-strom",
    ["13d"] = "weather-snow",
    ["50d"] = "weather-fog",
    ["01n"] = "weather-clear-night",
    ["02n"] = "weather-few-clouds-night",
    ["03n"] = "weather-clouds-night",
    ["04n"] = "weather-clouds-night",
    ["09n"] = "weather-showers-scattered",
    ["10n"] = "weather-showers",
    ["11n"] = "weather-strom",
    ["13n"] = "weather-snow",
    ["50n"] = "weather-fog",
}
local description_map = {
    ["01d"] = "无云",
    ["02d"] = "少云",
    ["03d"] = "阴",
    ["04d"] = "少云",
    ["09d"] = "小雨",
    ["10d"] = "阵雨",
    ["11d"] = "雷阵雨",
    ["13d"] = "下雪",
    ["50d"] = "雾",
    ["01n"] = "",
    ["02n"] = "少云",
    ["03n"] = "",
    ["04n"] = "阴",
    ["09n"] = "小雨",
    ["10n"] = "阵雨",
    ["11n"] = "雷阵雨",
    ["13n"] = "下雪",
    ["50n"] = "雾",
}
return require("react")({
    init = function(self)
        local api_key = config.widget.weather.key
        local city_id = config.widget.weather.city_id

        local url = ("https://api.openweathermap.org/data/2.5/weather" .. "?appid=" .. api_key .. "&q=" .. city_id)

        awful.widget.watch(string.format(GET_FORECAST_CMD, url), 600, function(_, stdout, stderr)
            if stderr == "" then
                local result = json.decode(stdout)
                self:set_state({
                    icon = icon_map[result.weather[1].icon],
                    description = description_map[result.weather[1].icon],
                    humidity = result.main.humidity,
                    temp_current = math.floor(result.main.temp - 273.15),
                    feels_like = math.floor(result.main.feels_like - 273.15),
                })
            end
        end)
    end,
    default_states = {
        icon = "weather-clear-sky",
        description = "",
        humidity = 19,
        temp_current = 20,
        feels_like = 20,
    },
    render = function(self)
        local icon = self.state.icon
        local description = self.state.description
        local humidity = self.state.humidity
        local temp_current = self.state.temp_current
        local feels_like = self.state.feels_like
        return {
            {
                {
                    {
                        image = icon and icon_dir .. icon .. ".svg",
                        resize = true,
                        forced_height = dpi(42),
                        forced_width = dpi(42),
                        widget = wibox.widget.imagebox,
                    },
                    {
                        {
                            {
                                text = description,
                                font = beautiful.font_name .. "Bold 14",
                                widget = wibox.widget.textbox,
                            },
                            {
                                text = " " .. humidity .. "%",
                                font = beautiful.font_name .. "Bold 11",
                                widget = wibox.widget.textbox,
                            },
                            layout = wibox.layout.fixed.vertical,
                        },
                        widget = wibox.container.place,
                    },
                    spacing = dpi(10),
                    layout = wibox.layout.fixed.horizontal,
                },
                nil,
                {
                    {
                        {
                            markup = temp_current .. "糖 ",
                            align = "right",
                            font = beautiful.font_name .. "Bold 16",
                            widget = wibox.widget.textbox,
                        },
                        {
                            markup = "体感温度: " .. feels_like .. "糖 ",
                            font = beautiful.font_name .. "Bold 10",
                            widget = wibox.widget.textbox,
                        },
                        spacing = dpi(-6),
                        layout = wibox.layout.fixed.vertical,
                    },
                    widget = wibox.container.place,
                },
                layout = wibox.layout.align.horizontal,
            },
            spacing = dpi(10),
            layout = wibox.layout.fixed.vertical,
        }
    end,
})
