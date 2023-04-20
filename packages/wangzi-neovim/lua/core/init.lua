require("core.opt")
require("core.map")
require("core.env")
require("core.auto")

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

vim.notify = require("notify")
local notify = vim.notify
vim.notify = function(msg, level, opt, ...)
    if msg:find("warning: multiple different client offset_encodings") then
        return
    end
    if msg:find("query: invalid node type at position ") then
        return
    end
    if msg:find("matchup#delim#get_matching") then
        return
    end
    if msg:find("处理 CursorMoved 自动命令") then
        return
    end
    if level == "error" then
        opt = opt or {}
        opt.timeout = 1000
    end

    notify(msg, level, opt, ...)
end
