local M = {}

M.hide_statusline = function()
    local hidden = {
        "help",
        "NvimTree",
        "terminal",
        "Undotree",
        "OUTLINE",
    }
    local shown = {}

    local api = vim.api
    local buftype = api.nvim_buf_get_option(0, "ft")

    -- shown table from config has the highest priority
    if vim.tbl_contains(shown, buftype) then
        api.nvim_set_option("laststatus", 2)
        return
    end

    if vim.tbl_contains(hidden, buftype) then
        api.nvim_set_option("laststatus", 0)
        return
    end

    api.nvim_set_option("laststatus", 2)
end

M.cache = {}
function M.cached(key, callback)
    if M.cache[key] ~= nil then
        return M.cache[key]
    else
        local value = callback()
        M.cache[key] = value
        return value
    end
end

function M.cachedinput(key, prompt, default, completion)
    default = M.cache[key] or default
    local value = vim.fn.input(prompt, default, completion)
    M.cache[key] = value
    return value
end

return M
