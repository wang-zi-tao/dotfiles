local function ClearTerm(reset)
    vim.opt_local.scrollback = 1

    vim.api.nvim_command("startinsert")
    if reset == 1 then
        vim.api.nvim_feedkeys("reset", 't', false)
    else
        vim.api.nvim_feedkeys("clear", 't', false)
    end
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<cr>', true, false, true), 't', true)

    vim.opt_local.scrollback = 10000
end

vim.api.nvim_create_user_command(
    'ClearTerm',
    ClearTerm,
    { nargs = 0 }
)
return {
    ClearTerm = ClearTerm,
}
