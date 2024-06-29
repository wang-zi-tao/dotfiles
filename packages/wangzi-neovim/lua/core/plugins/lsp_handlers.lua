require("core.theme").define_sign()

vim.diagnostic.config({
    virtual_text = {
        prefix = "",
    },
    signs = true,
    underline = true,
    update_in_insert = false,
})

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
