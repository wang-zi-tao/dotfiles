local handler = function(virtText, lnum, endLnum, width, truncate)
    local newVirtText = {}
    local suffix = (' 󰁂 %d '):format(endLnum - lnum)
    local sufWidth = vim.fn.strdisplaywidth(suffix)
    local targetWidth = width - sufWidth
    local curWidth = 0
    for _, chunk in ipairs(virtText) do
        local chunkText = chunk[1]
        local chunkWidth = vim.fn.strdisplaywidth(chunkText)
        if targetWidth > curWidth + chunkWidth then
            table.insert(newVirtText, chunk)
        else
            chunkText = truncate(chunkText, targetWidth - curWidth)
            local hlGroup = chunk[2]
            table.insert(newVirtText, { chunkText, hlGroup })
            chunkWidth = vim.fn.strdisplaywidth(chunkText)
            -- str width returned from truncate() may less than 2nd argument, need padding
            if curWidth + chunkWidth < targetWidth then
                suffix = suffix .. (' '):rep(targetWidth - curWidth - chunkWidth)
            end
            break
        end
        curWidth = curWidth + chunkWidth
    end
    table.insert(newVirtText, { suffix, 'MoreMsg' })
    return newVirtText
end


return {
    {
        "kevinhwang91/nvim-ufo",
        dir = gen.nvim_ufo,
        name = "nvim_ufo",
        lazy = true,
        event = "LspAttach",
        module = "ufo",
        dependencies = {
            {
                'kevinhwang91/promise-async',
                dir = gen.promise_async,
                name = "promise_async",
            }
        },
        config = function()
            vim.o.foldmethod = "manual"
            require("ufo").setup({
                close_fold_kinds_for_ft = {
                    default = { 'imports', 'comment' },
                    json = { 'array' },
                    c = { 'comment', 'region' }
                },
                fold_virt_text_handler = handler,
                preview = {
                    mappings = {
                        switch = "<C-l>",
                    },
                }
            })
            vim.cmd.UfoDisableFold()
        end,
        keys = {
            { "zR", function() require('ufo').openAllFolds() end,         desc = "Open All Folds" },
            { "zM", function() require('ufo').closeAllFolds() end,        desc = "Close All Folds" },
            { "zr", function() require('ufo').openFoldsExceptKinds() end, desc = "Open Folds Except Kinds" },
            { "zm", function() require('ufo').closeFoldsWith() end,       desc = "Close Folds With" },
        },
    },
}
