local packer_bootstrap
gen = require("core.gen") or {}
if gen.core then
    vim.opt.packpath = gen.core .. "/init.lua"
else
    local lazypath = (gen.core or vim.fn.stdpath("data")) .. "/lazy/lazy.nvim"
    while not vim.loop.fs_stat(lazypath) do
        print("downloading lazy.nvim")
        packer_bootstrap = vim.fn.system({
            "git",
            "clone",
            "--filter=blob:none",
            "https://github.com/folke/lazy.nvim.git",
            "--branch=stable", -- latest stable release
            lazypath,
        })
    end
    vim.opt.rtp:prepend(lazypath)
end
require("core.lazy")
require("lazy.help").update = function() end
if packer_bootstrap then
    require("lazy").install()
    vim.cmd("TSUpdate")
end

require("core")
