local function load(module)
    local success, error = pcall(require, module)
    if not success then
        require("notify")("Failed to load " .. module .. ": " .. error, vim.log.levels.ERROR)
    end
end

load("core.opt")
load("core.map")
load("core.env")
load("core.auto")
load("core.cmd")
load("core.theme")
load("core.database")
load("core.plugins.wps")

local n = require("core.gen")
if n.core ~= null then
    for _, file in ipairs(vim.fn.readdir(n.core .. "/skeleton")) do
        vim.api.nvim_create_autocmd({ "BufNewFile" }, {
            pattern = { file },
            callback = function()
                vim.cmd("0r '" .. file .. "'")
            end,
        })
    end
end

local function load_nvim_lua_file(dir)
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

vim.api.nvim_create_autocmd("DirChanged", {
    pattern = "*",
    callback = function()
        load_nvim_lua_file(vim.fn.getcwd())
    end,
})

load_nvim_lua_file(vim.fn.getcwd())
