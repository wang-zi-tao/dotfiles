require("core.theme").define_sign()

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
    border = "rounded",
})
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
