local database = require("core.database")
local Job = require("plenary.job")
local async = require('plenary.async')
local nio = require("nio")
local nui_components = require("nui-components")

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

---@param key string
---@param prompt string
---@param default string
---@param completion string
---@param callback fun(value: string|nil)
function M.cachedinput(key, prompt, default, completion, callback)
    local n = nui_components
    local project_dir = M.project_dir()

    -- 获取历史缓存列表
    local list = database.tables.caches:get({
        select = { "distinct value" },
        where = {
            project = project_dir,
            key = key,
        },
        order_by = {
            desc = "last_read"
        }
    })

    -- 准备所有历史选项
    ---@type [string]
    local all_options = {}
    for _, item in pairs(list) do
        table.insert(all_options, item.value)
    end

    local filtered_options = {}
    for _, opt in ipairs(all_options) do
        table.insert(filtered_options, n.option(opt, { id = opt }))
    end

    local has_history = #all_options > 0
    local has_submit = false

    -- 创建 Signal 管理状态
    local signal = n.create_signal({
        input_value = default or "",
        filtered_options = filtered_options,
        selected_value = nil,
    })

    -- 创建 Renderer
    local renderer = n.create_renderer({
        width = 60,
        height = has_history and 22 or 8,
        position = "50%",
        relative = "editor",
    })

    renderer:on_unmount(function()
        if not has_submit then
            callback(nil)
        end
    end)

    -- 过滤函数
    ---@param query string
    ---@return [string]
    local function filter_options(query)
        if not query or query == "" then
            return all_options
        end

        query = query:lower()
        local filtered = {}
        for _, opt in ipairs(all_options) do
            if opt:lower():find(query, 1, true) then
                table.insert(filtered, opt)
            end
        end
        return filtered
    end

    -- 提交处理
    local function submit_value(value)
        local time = os.date()
        if value and value ~= "" then
            database.tables.caches:update({
                where = {
                    project = project_dir,
                    key = key,
                    value = value,
                },
                set = {
                    last_read = time,
                }
            })
        end
        has_submit = true
        renderer:close()
        callback(value)
    end

    -- 构建 UI
    local body = function()
        return n.rows(
            n.select({
                id = "select",
                flex = 1,
                autofocus = true,
                border_label = "select",
                data = signal.filtered_options,
                selected = signal.selected_value,
                on_select = function(selects)
                    signal.selected_value = selects
                    submit_value(selects.id)
                end,
            }),
            n.prompt({
                prefix = prompt,
                id = "input",
                value = default or "",
                autofocus = false,
                border_label = "intput",
                on_change = function(value)
                    signal.input_value = value
                    -- 实时过滤列表
                    local filtered = filter_options(value)
                    local new_options = {}
                    for _, item in ipairs(filtered) do
                        table.insert(new_options, n.option(item, { id = item }))
                    end
                    signal.filtered_options = new_options
                end,
                on_submit = function(value)
                    submit_value(value)
                end,
                mappings = function()
                    return {
                        {
                            mode = { "n", "i", "v" },
                            key = "<Up>",
                            handler = function()
                                local component = renderer:get_component_by_id("select")
                                if component then
                                    component:focus()
                                    if #signal.filtered_options > 0 then
                                        signal.selected_value = signal.filtered_options[#signal.filtered_options]
                                    end
                                end
                            end,
                        },
                        {
                            mode = { "n", "i", "v" },
                            key = "<Down>",
                            handler = function()
                                local component = renderer:get_component_by_id("select")
                                if component then
                                    component:focus()
                                end
                            end,
                        }
                    }
                end
            })
        )
    end

    renderer:render(body)
    renderer:focus()
end

---@param arg string[]
---@param key string
---@param prompt string
---@param default string
---@param completion string
---@param callback fun(value: string|nil)
function M.argOrCachedInput(arg, key, prompt, default, completion, callback)
    if #arg > 0 then
        database.tables.caches:insert({
            project = M.project_dir(),
            key = key,
            value = arg,
        })
        callback(arg.text)
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
---@return string|nil
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

---@param override_config_name string
---@param language string
---@param config dap.Configuration
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

function M.platform_env_split()
    if vim.fn.has("win32") == 1 then
        return ";"
    else
        return ":"
    end
end

function M.append_env(env, value)
    if vim.env[env] == nil then
        vim.env[env] = value
    else
        vim.env[env] = vim.env[env] .. M.platform_env_split() .. value
    end
end

function M.ensure_env_list(env, value)
    local split_char = M.platform_env_split()
    local current_values = {}
    if vim.env[env] ~= nil then
        for v in string.gmatch(vim.env[env], "([^" .. split_char .. "]+)") do
            current_values[v] = true
        end
    end

    if current_values[value] == nil then
        M.append_env(env, value)
    end
end

---@param pid number
---@return number[]
function M.get_subprocess(pid)
    local ps_job = Job:new({ command = "nu", args = { "-c", "ps | where ppid = " .. pid .. " | get pid | str join '\n'" }, })
    ps_job:sync()
    local pids = ps_job:result()
    return pids
end

---@param pid number
---@return number[]
function M.get_subprocess_recursive(pid)
    local all_pids = {}
    local function dfs(current_pid)
        table.insert(all_pids, current_pid)
        local children = M.get_subprocess(current_pid)
        for _, child_pid_str in ipairs(children) do
            local child_pid = tonumber(child_pid_str)
            if child_pid ~= nil then
                dfs(child_pid)
            end
        end
    end
    dfs(pid)
    return all_pids
end

M.task_template_builders = {}

---@param template overseer.TemplateDefinition
function M.register_task_template(template)
    local name = template.name
    local builder = template.builder
    M.task_template_builders[name] = builder
    require("overseer").register_template(vim.tbl_deep_extend("force", template, {
        builder = function()
            return M.task_template_builders[name]()
        end,
    }))
end

---@return { env: table<string, string> } | nil
function M.load_direnv()
    local env_process = nio.process.run({
        cmd = "direnv", args = { "exec", ".", "env" },
    })

    if env_process == nil then
        return nil
    end

    if env_process.code ~= 0 then
        vim.notify("Failed to load direnv: " .. env_process.stderr.read(), vim.log.levels.ERROR)
        return nil
    end

    ---@type string
    local stdout = env_process.stdout.read()
    local env = {}
    for key, value in string.gmatch(stdout, "([^=]+)=([^\n]*)") do
        env[key] = value
    end
    return { env = env }
end

function M.apply_envrc()
    nio.run(function()
        local direnv = M.load_direnv()
        if direnv == nil then
            return
        end
        vim.schedule(function()
            for k, v in pairs(direnv.env) do
                vim.env[k] = v
            end
            vim.notify('Reload direnv', vim.log.levels.INFO)
        end)
    end)
end

---@param file1 string
---@param file2 string
---@return boolean
function M.is_same_file(file1, file2)
    local path1 = vim.fn.fnamemodify(file1, ":p:gs?\\\\?/?")
    local path2 = vim.fn.fnamemodify(file2, ":p:gs?\\\\?/?")
    return path1 == path2
end

---功能: 获取当前buffer光标下的路径的文件
---路径格式:
---file
---file:line
---file:line:col
---file(line,col)
---@return {file: string, line: number|nil, column: number|nil}|nil
function M.get_file_under_cursor()
    local cword = vim.fn.expand("<cWORD>")
    if not cword or cword == "" then
        return nil
    end

    cword = cword:gsub("^[%s%(%[%{%<\"']+", ""):gsub("[%s%)%]%}%>\"']+$", "")

    local file, line, col = cword:match("^([a-zA-Z]:[^:]+):(%d+):(%d+)$")
    if file then
        return { file = file, line = tonumber(line), column = tonumber(col) }
    end

    file, line = cword:match("^([a-zA-Z]:[^:]+):(%d+)$")
    if file then
        return { file = file, line = tonumber(line), column = nil }
    end

    file, line, col = cword:match("^([^:]+):(%d+):(%d+)$")
    if file then
        return { file = file, line = tonumber(line), column = tonumber(col) }
    end

    file, line = cword:match("^([^:]+):(%d+)$")
    if file then
        return { file = file, line = tonumber(line), column = nil }
    end

    file, line, col = cword:match("^(.+)%((%d+),(%d+)%)$")
    if file then
        return { file = file, line = tonumber(line), column = tonumber(col) }
    end

    return { file = cword, line = nil, column = nil }
end

return M
