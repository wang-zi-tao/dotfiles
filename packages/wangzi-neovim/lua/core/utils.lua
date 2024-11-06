local database = require("core.database")

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

---@return string
function M.project_dir()
    return vim.fn.getcwd()
end

function M.cached(key, callback)
    if M.cache[key] ~= nil then
        return M.cache[key]
    else
        local value = callback()
        M.cache[key] = value
        return value
    end
end

function M.cachedinput(key, prompt, default, completion, callback)
    local function ui_input()
        local value
        if completion == nil then
            value = vim.fn.input(prompt, default)
        else
            value = vim.fn.input(prompt, default, completion)
        end
        database.tables.caches:insert({
            project = M.project_dir(),
            key = key,
            value = value,
        })
        callback(value)
    end

    local list = database.tables.caches:get({
        select = { "distinct value" },
        where = {
            project = M.project_dir(),
            key = key,
        }
    })

    if #list ~= 0 then
        local itemStringList = {}
        for _, item in pairs(list) do
            table.insert(itemStringList, item.value)
        end
        vim.ui.select(itemStringList, { prompt = prompt }, function(item, index)
            if index ~= nil then
                callback(item)
            else
                ui_input()
            end
        end)
    else
        ui_input()
    end
end

function M.argOrCachedInput(arg, key, prompt, default, completion, callback)
    if #arg > 0 then
        database.tables.caches:insert({
            project = M.project_dir(),
            key = key,
            value = arg,
        })
        callback(arg)
    else
        M.cachedinput(key, prompt, default, completion, callback)
    end
end

function M.num_of_core()
    local num_of_processers = 16
    if vim.fn.has("win32") > 0 then
        num_of_processers = tonumber(vim.env.NUMBER_OF_PROCESSORS or 0)
    elseif vim.fn.has("unix") > 0 then
        local handle = io.popen("nproc")
        local result = handle:read("*a")
        handle:close()
        num_of_processers = tonumber(result or 0)
    end
    local num_of_job = num_of_processers
    if num_of_processers > 4 then
        num_of_job = num_of_processers - 4
    end
    return num_of_job
end

function M.config_dir()
    if gen.core then
        return gen.core
    else
        return vim.fn.stdpath("config")
    end
end

M.module_dir_names = {
    "CMakeLists.txt",
    "Cargo.toml",
}

function M.module_dir()
    local current = vim.fn.expand('%:p:h')
    for k, v in ipairs(M.module_dir_names) do
        ---@type string
        local path = vim.fn.finddir('Coding/..', current .. ';')
        if path ~= current then
            return path
        end
    end
    return M.project_dir()
end

function M.add_mark()
    require("trailblazer").new_trail_mark()
    require("harpoon"):list():add()
    vim.cmd [[Arrow toggle_current_line_for_buffer]]
end

return M
