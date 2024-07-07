pcall(require, "core.opt")
pcall(require, "core.map")
pcall(require, "core.env")
pcall(require, "core.auto")
pcall(require, "core.cmd")
pcall(require, "core.theme")
pcall(require, "core.database")

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
