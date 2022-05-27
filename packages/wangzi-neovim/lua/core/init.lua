require("core.opt")
require("core.map")
require("core.env")

local n = require("core.gen")
for _, file in ipairs(vim.fn.readdir(n.core .. "/skeleton")) do
  vim.api.nvim_create_autocmd({ "BufNewFile" }, {
    pattern = { file },
    callback = function()
      vim.cmd("0r '" .. file .. "'")
    end,
  })
end
