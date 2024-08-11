local plenary = require("plenary")
local a = require("async")

local autocmd = vim.api.nvim_create_autocmd
autocmd({ "BufNewFile", "BufRead" }, {
    pattern = { "*.qrc", "*.ts" },
    callback = function()
        vim.cmd([[setfiletype xml]])
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

-- a.async(function()
--     while true do
--         plenary.sleep(360)
--         vim.cmd [[SessionManager save_current_session]]
--     end
-- end)
