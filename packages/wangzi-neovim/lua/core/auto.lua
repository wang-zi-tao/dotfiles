local plenary = require("plenary")
local a = require("async")
local utils = require("core.utils")
local async = require 'plenary.async'


local autocmd = vim.api.nvim_create_autocmd
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
        vim.cmd [[set ft=cpp]]
    end,
})

autocmd("FileType", {
    pattern = { "*.wgsl" },
    callback = function()
        vim.cmd [[set ft=wgsl_bevy]]
    end,
})

vim.api.nvim_create_autocmd('BufWritePost', {
    pattern = '.nvim.lua',
    callback = function(ev)
        local cwd_config = vim.fn.getcwd() .. '/.nvim.lua'
        if utils.is_same_file(cwd_config, ev.file) then
            dofile(ev.file)
            vim.notify('Reloaded .nvim.lua configuration', vim.log.levels.INFO)
        end
    end,
    desc = 'Auto-reload .nvim.lua on save'
})

vim.api.nvim_create_autocmd('BufWritePost', {
    pattern = { ".envrc", "shell.nix", "flake.nix" },
    callback = function(ev)
        local dir = vim.fn.fnamemodify(ev.file, ":h")
        if utils.is_same_file(dir, vim.fn.getcwd()) then
            utils.apply_envrc()
        end
    end,
    desc = 'Auto-reload init.lua on save'
})

local envrc_path = vim.fn.getcwd() .. '/.envrc'
if vim.fn.filereadable(envrc_path) == 1 then
    utils.apply_envrc()
end

-- a.async(function()
--     while true do
--         plenary.sleep(360)
--         vim.cmd [[SessionManager save_current_session]]
--     end
-- end)
