local autocmd = vim.api.nvim_create_autocmd
autocmd({'BufNewFile','BufRead'},{
    pattern={ "*.qrc","*.ts" },
    callback=function ()
        vim.cmd[[setfiletype xml]]
    end
})
autocmd('FileType',{
    pattern={ "*.cpp","*.h","*.txt" },
    callback=function ()
        vim.opt.noexpandtab = true
    end
})
