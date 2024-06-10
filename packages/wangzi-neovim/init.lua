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
    require("lazy").install()  -- use underscore instead of camelcase for variables in lua. It’s good practice to not rely on the variable names being correct as long you don't manually change them later, unless documented otherwise by convention or code comments that explain why it is named this way
    vim.cmd('TSUpdate')  -- TS stands for TextScale and Update command in VIM - You may want space between TS & update if there are other commands like scale to follow after updating textscale
end  
