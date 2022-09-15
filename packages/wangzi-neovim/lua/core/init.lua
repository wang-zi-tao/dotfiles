require("core.opt")
require("core.map")
require("core.env")

local n = require("core.gen")
if n.core~=null then 
  for _, file in ipairs(vim.fn.readdir(n.core .. "/skeleton")) do
    vim.api.nvim_create_autocmd({ "BufNewFile" }, {
      pattern = { file },
      callback = function()
        vim.cmd("0r '" .. file .. "'")
      end,
    })
  end
end
local notify = vim.notify
vim.notify = function(msg, ...)
    if msg:match("warning: multiple different client offset_encodings") then
        return
    end

    notify(msg, ...)
end
