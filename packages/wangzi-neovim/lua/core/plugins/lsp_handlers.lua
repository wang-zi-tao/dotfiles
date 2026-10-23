require("core.theme").define_sign()

-- hover 浮窗：原生 hover 内容 + 当前行诊断全文（诊断过长时 virtual text 会截断）
vim.lsp.handlers["textDocument/hover"] = require("core.diagnostic").hover_handler
vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
    border = "rounded",
})

-- suppress error messages from lang servers
-- local notify = vim.notify
-- vim.notify = function(msg, ...)
--     -- if msg:match("exit code") then
--     --     return
--     -- end
--
--     notify(msg, ...)
-- end
