local plenary = require("plenary")
local a = require("async")
local utils = require("core.utils")
local async = require("plenary.async")

--- CoreAuto group: 归入统一 augroup，便于 hotreload 重载前精确清理
local group = vim.api.nvim_create_augroup("CoreAuto", { clear = true })
local autocmd = function(events, opts)
    opts.group = group
    vim.api.nvim_create_autocmd(events, opts)
end
autocmd({ "BufNewFile", "BufRead" }, {
    pattern = { "*.qrc", "*.ts", "*.natvis" },
    callback = function()
        vim.cmd([[setfiletype xml]])
    end,
})
autocmd({ "BufNewFile", "BufRead" }, {
    pattern = { "*.ets" },
    callback = function()
        vim.cmd([[setfiletype typescript]])
    end,
})
autocmd("FileType", {
    pattern = { "*.cpp", "*.h", "*.txt" },
    callback = function()
        vim.opt.noexpandtab = true
    end,
})

autocmd("FileType", {
    pattern = { "*.inc" },
    callback = function()
        vim.cmd([[set ft=cpp]])
    end,
})

autocmd("FileType", {
    pattern = { "*.wgsl" },
    callback = function()
        vim.cmd([[set ft=wgsl_bevy]])
    end,
})

autocmd("BufWritePost", {
    pattern = ".nvim.lua",
    callback = function(ev)
        local cwd_config = vim.fn.getcwd() .. "/.nvim.lua"
        if utils.is_same_file(cwd_config, ev.match) then
            dofile(ev.match)
            vim.notify("Reloaded .nvim.lua configuration", vim.log.levels.INFO)
        else
            vim.notify("skip reloading " .. ev.match, vim.log.levels.INFO)
        end
    end,
    desc = "Auto-reload .nvim.lua on save",
})

autocmd("BufWritePost", {
    pattern = { ".envrc", "shell.nix", "flake.nix" },
    callback = function(ev)
        local dir = vim.fn.fnamemodify(ev.match, ":h")
        if utils.is_same_file(dir, vim.fn.getcwd()) then
            utils.apply_envrc()
        end
    end,
    desc = "Auto-reload init.lua on save",
})

local envrc_path = vim.fn.getcwd() .. "/.envrc"
if vim.fn.filereadable(envrc_path) == 1 then
    utils.apply_envrc()
end

-- a.async(function()
--     while true do
--         plenary.sleep(360)
--         vim.cmd [[SessionManager save_current_session]]
--     end
-- end)

local gen = require("core.gen")
if gen.core ~= nil then
    for _, file in ipairs(vim.fn.readdir(gen.core .. "/skeleton")) do
        autocmd({ "BufNewFile" }, {
            pattern = { file },
            callback = function()
                vim.cmd("0r '" .. file .. "'")
            end,
        })
    end
end

-- 注册热加载钩子：重载本模块前清空 CoreAuto group，避免 autocmd 累积
require("core.hotreload").register_hook("core.auto", {
    before = function()
        pcall(vim.api.nvim_clear_autocmds, { group = "CoreAuto" })
    end,
})
