local bufferline = require("bufferline")
local devicons = require("nvim-web-devicons")

local theme = require("core.theme")
local colors = theme.colors
local symbols = theme.symbols
bufferline.setup({
    highlights = {},
    options = {
        offsets = { { filetype = "NvimTree", text = "", padding = 1 } },
        buffer_close_icon = "󰖭",
        modified_icon = "",
        close_icon = "󰖭",
        show_close_icon = true,
        left_trunc_marker = "",
        right_trunc_marker = "",
        max_name_length = 14,
        max_prefix_length = 13,
        tab_size = 20,
        show_tab_indicators = true,
        duplicates_across_groups = true,
        -- groups = {
        --     options = {
        --         toggle_hidden_on_enter = true
        --     },
        --     items = {
        --         {
        --             name = "Tests", -- Mandatory
        --             icon = " ", -- Optional
        --             matcher = function(buf) -- Mandatory
        --                 return buf.path:match('%test%')
        --             end,
        --         },
        --         {
        --             name = "Config", -- Mandatory
        --             icon = " ", -- Optional
        --             matcher = function(buf) -- Mandatory
        --                 local path = buf.path
        --                 local filetype = buf.buftype
        --                 return path and (path:match('%CMakeLists.txt') or path:match('%Cargo.toml'))
        --                     or filetype and (filetype == "json" or filetype == "xml")
        --             end,
        --         },
        --         {
        --             name = "Terminal", -- Mandatory
        --             icon = " ", -- Optional
        --             matcher = function(buf) -- Mandatory
        --                 local filetype = buf.buftype
        --                 return filetype == "toggleterm" or filetype == "terminal"
        --             end,
        --         },
        --     }
        -- },
        enforce_regular_tabs = false,
        view = "multiwindow",
        show_buffer_close_icons = true,
        separator_style = "thick",
        always_show_bufferline = true,
        diagnostics = "nvim_lsp",
        diagnostics_indicator = function(count, level, diagnostics_dict, context)
            local s = " "
            for e, n in pairs(diagnostics_dict) do
                s = s .. n .. symbols[e]
            end
            return s
        end,
        custom_filter = function(buf_number)
            -- Func to filter out our managed/persistent split terms
            local present_type, type = pcall(function()
                return vim.api.nvim_buf_get_var(buf_number, "term_type")
            end)

            if present_type then
                if type == "vert" then
                    return false
                elseif type == "hori" then
                    return false
                end
                return true
            end

            return true
        end,
        custom_areas = {
            left = function()
                local filename = vim.fn.expand("%:t")
                local filetype = vim.bo.filetype
                local icon, icon_color = devicons.get_icon_color(filename, filetype)
                local result = {}
                table.insert(result, { text = icon, bg = colors.ui_main, fg = "#ffffff" })
                table.insert(result, { text = " " .. filename, bg = colors.ui_main, fg = "#ffffff" })
                table.insert(result, { text = " ", fg = colors.ui_main, bg = "#42464e" })
                table.insert(result, { text = "  " .. vim.b.gitsigns_head, bg = "#42464e", fg = colors.ui_main })
                table.insert(result, { text = "  ", fg = "#42464e" })

                local seve = vim.diagnostic.severity
                local error = #vim.diagnostic.get(0, { severity = seve.ERROR })
                local warning = #vim.diagnostic.get(0, { severity = seve.WARN })
                local info = #vim.diagnostic.get(0, { severity = seve.INFO })
                local hint = #vim.diagnostic.get(0, { severity = seve.HINT })

                if error ~= 0 then
                    table.insert(result, { text = " " .. symbols.error .. error, fg = colors.error })
                end

                if warning ~= 0 then
                    table.insert(result, { text = " " .. symbols.warn .. warning, fg = colors.warning })
                end

                if hint ~= 0 then
                    table.insert(result, { text = " " .. symbols.info .. hint, fg = colors.info })
                end

                if info ~= 0 then
                    table.insert(result, { text = " " .. symbols.hint .. info, fg = colors.hint })
                end

                return result
            end,
        },
    },
})
