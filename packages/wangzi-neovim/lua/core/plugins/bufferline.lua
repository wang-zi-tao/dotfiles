local bufferline = require("bufferline")
local devicons = require( 'nvim-web-devicons' )

local colors = require("core.colors").get()
bufferline.setup({
    highlights = {

    },
    options = {
        offsets = { { filetype = "NvimTree", text = "", padding = 1 } },
        buffer_close_icon = "",
        modified_icon = "",
        close_icon = "",
        show_close_icon = true,
        left_trunc_marker = "",
        right_trunc_marker = "",
        max_name_length = 14,
        max_prefix_length = 13,
        tab_size = 20,
        show_tab_indicators = true,
        duplicates_across_groups = true,
        enforce_regular_tabs = false,
        view = "multiwindow",
        show_buffer_close_icons = true,
        separator_style = "thick",
        always_show_bufferline = true,
        diagnostics = "nvim_lsp",
        diagnostics_indicator = function(count, level, diagnostics_dict, context)
            local s = " "
            for e, n in pairs(diagnostics_dict) do
                local sym = e == "error" and " " or (e == "warning" and " " or " ")
                s = s .. n .. sym
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
                local filename = vim.fn.expand('%:t')
                local filetype = vim.bo.filetype
                local icon, icon_color = devicons.get_icon_color(filename, filetype)
                local result = {}
                table.insert(result, { text = icon, bg = '#51afef', fg = '#ffffff' })
                table.insert(result, { text = " " .. filename, bg = '#51afef', fg = '#ffffff' })
                table.insert(result, { text = "" , fg = '#51afef', bg = "#42464e" })
                table.insert(result, { text = "  " .. vim.b.gitsigns_head, bg = "#42464e", fg = '#51afef' })
                table.insert(result, { text = "" , fg = "#42464e" })
                return result
            end,
            right = function()
                local result = {}
                local seve = vim.diagnostic.severity
                local error = #vim.diagnostic.get(0, { severity = seve.ERROR })
                local warning = #vim.diagnostic.get(0, { severity = seve.WARN })
                local info = #vim.diagnostic.get(0, { severity = seve.INFO })
                local hint = #vim.diagnostic.get(0, { severity = seve.HINT })

                if error ~= 0 then
                    table.insert(result, { text = "  " .. error, fg = "#EC5241" })
                end

                if warning ~= 0 then
                    table.insert(result, { text = "  " .. warning, fg = "#EFB839" })
                end

                if hint ~= 0 then
                    table.insert(result, { text = "  " .. hint, fg = "#A3BA5E" })
                end

                if info ~= 0 then
                    table.insert(result, { text = "  " .. info, fg = "#7EA9A7" })
                end
                return result
            end,
        },
    },
})
