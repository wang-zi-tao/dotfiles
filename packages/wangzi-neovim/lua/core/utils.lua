local database = require("core.database")

local M = {}

M.try = function(f)
    local ok, err = pcall(f)
    if not ok then
        require("notify")(err, vim.log.levels.ERROR)
    end
end

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
        vim.ui.input({
            prompt = prompt,
            default = default,
            completion = completion,
        }, function(value)
            database.tables.caches:insert({
                project = M.project_dir(),
                key = key,
                value = value,
            })
            callback(value)
        end)
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

---@param arg string[]
---@param key string
---@param prompt string
---@param default string
---@param completion string
---@param callback fun(value: string)
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
    ".git",
}
M.module_file_names = {
    "CMakeLists.txt",
    "Cargo.toml",
}

function M.get_buffer_path(bufnr)
    local bufname = vim.fn.bufname(bufnr)
    local absolute_buffer_path = vim.fn.fnamemodify(bufname, ":p")
    return absolute_buffer_path
end

function M.get_current_buffer_path()
    return vim.fn.expand("%:p")
end

function M.module_dir()
    local current = vim.fn.expand('%:p:h')
    for k, v in ipairs(M.module_file_names) do
        ---@type string
        local path = vim.fn.findfile(v, current .. ';')
        if path ~= current then
            return vim.fn.fnamemodify(path, ":p:h")
        end
    end
    for k, v in ipairs(M.module_dir_names) do
        ---@type string
        local path = vim.fn.finddir(v, current .. ';')
        if path ~= current then
            return vim.fn.fnamemodify(path, ":p:h")
        end
    end
    return M.project_dir()
end

function M.add_mark()
    local buftype = vim.api.nvim_buf_get_option(0, "ft")
    if buftype == "alpha" then
        return
    end

    -- M.try(function() require("trailblazer").new_trail_mark() end)
    M.try(function() require("harpoon"):list():add() end)
    -- M.try(function() vim.cmd [[Arrow toggle_current_line_for_buffer]] end)
    M.try(function() require("arrow.persist").save(M.get_current_buffer_path()) end)
end

function M.get_changed_ranges()
    local ranges = {}
    local hunks = require("gitsigns").get_hunks()
    if hunks == nil then
        return ranges
    end
    for i = #hunks, 1, -1 do
        local hunk = hunks[i]
        if hunk ~= nil and hunk.type ~= "delete" then
            local start = hunk.added.start
            local last = start + hunk.added.count
            -- nvim_buf_get_lines uses zero-based indexing -> subtract from last
            local last_hunk_line = vim.api.nvim_buf_get_lines(0, last - 2, last - 1, true)[1]
            local range = { start = { start, 0 }, ["end"] = { last - 1, last_hunk_line:len() } }
            table.insert(ranges, range)
        end
    end
    return ranges
end

function M.get_selection()
    if vim.fn.mode() ~= "v" then
        return ""
    end

    return table.concat(vim.fn.getregion(
        vim.fn.getpos("."), vim.fn.getpos("v"), { mode = vim.fn.mode() }
    ), '\n')
end

---@param key string
---@param prompt string
---@param default string
---@param completion string
function M.cached_input_sync(key, prompt, default, completion)
    local co = coroutine.running()
    vim.schedule(function()
        M.cachedinput(key, prompt, default, completion, function(value)
            coroutine.resume(co, value)
        end)
    end)
    return coroutine.yield()
end

---@param opt table
function M.pick_file(opt)
    local actions = require "telescope.actions"
    local action_state = require "telescope.actions.state"
    local Path = require("plenary.path")
    local co = coroutine.running()

    vim.schedule(function()
        require("telescope").extensions.file_browser.file_browser(vim.tbl_deep_extend("keep", opt, {
            prompt = "executable file",
            use_fd = true,
            no_ignore = true,
            attach_mappings = function(prompt_bufnr, map)
                local do_map = function()
                    actions.close(prompt_bufnr)
                    local selection = action_state.get_selected_entry()
                    vim.print(selection)
                    local path = Path:new(selection[1])
                    if path:is_file() then
                        coroutine.resume(co, tostring(path))
                    end
                end
                map("n", "<Tab>", do_map)
                map("i", "<Tab>", do_map)
                return true
            end
        }))
    end)

    return coroutine.yield()
end

---@param file string
---@param callback fun(err: string, fname: string, status: string)
function M.watch_file(file, callback)
    local w = vim.uv.new_fs_event()
    local do_watch_file
    local function on_change(err, fname, status)
        -- Do work...
        vim.api.nvim_command('checktime')
        -- Debounce: stop/start.
        w:stop()
        do_watch_file(fname)
        callback(err, fname, status)
    end
    do_watch_file = function(fname)
        local fullpath = vim.api.nvim_call_function(
            'fnamemodify', { fname, ':p' })
        w:start(fullpath, {}, vim.schedule_wrap(on_change))
    end

    do_watch_file(file)
end

M.pwd = vim.fn.getcwd()

vim.api.nvim_create_autocmd("DirChanged", {
    pattern = "*",
    callback = function()
        M.pwd = vim.fn.getcwd()
    end,
})

M.toggleterm_nvim = {}
function M.toggle_term(number)
    require("toggleterm")
    M.toggleterm_nvim[number]:toggle()
end

M.get_coredmp = function()
    return M.cached_input_sync("coredump_path", "Path to coredump: ", "", "file")
    -- return M.pick_file({
    --     no_ignore = true,
    --     prompt = "Path to coredump: ",
    -- })
end

M.get_program = function()
    return M.cached_input_sync("program_path", "Path to executable: ", "", "file")
    -- return util.pick_file({
    --     no_ignore = true,
    --     prompt = "Path to executable: ",
    -- })
end

M.get_debug_ip = function()
    return M.cached_input_sync("debug_ip", "ip: ", "", "")
    -- return util.pick_file({
    --     no_ignore = true,
    --     prompt = "Path to executable: ",
    -- })
end

M.find_dap_config = function(name, language)
    local dap = require("dap")
    local configs = dap.configurations[language] or {}
    for _, config in ipairs(configs) do
        if config.name == name then
            return config
        end
    end
    return nil
end

M.remove_dap_config = function(name, language)
    local dap = require("dap")
    local configs = dap.configurations[language]
    for i, config in ipairs(configs) do
        if config.name == name then
            table.remove(configs, i)
            return true
        end
    end
    return false
end

M.override_dap_config = function(override_config_name, language, config)
    local dap = require("dap")
    local override_config = M.find_dap_config(override_config_name, language) or {}
    M.remove_dap_config(config.name, language)
    local new_config = vim.tbl_deep_extend("force", override_config, config)

    if override_config == nil then
        vim.notify("No such debug config: " .. override_config_name, vim.log.levels.ERROR)
        return
    end

    M.remove_dap_config(config.name, language)
    table.insert(dap.configurations[language], new_config)
end

function M.load_nvim_lua_file(dir)
    local nvim_lua = dir .. "/.nvim.lua"
    if vim.fn.filereadable(nvim_lua) == 1 then
        vim.api.nvim_create_autocmd({ "BufWritePost" }, {
            pattern = nvim_lua,
            callback = function()
                dofile(nvim_lua)
            end,
        })
        dofile(nvim_lua)
    end
end

---@param process_name string|nil
function M.pick_process(process_name)
    return require("dap.utils").pick_process({
        filter = function(process)
            if process_name == nil or process.name == process_name then
                return true
            end
            return false
        end,
    })
end

return M
