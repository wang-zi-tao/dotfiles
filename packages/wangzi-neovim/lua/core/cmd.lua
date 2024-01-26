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

vim.api.nvim_create_user_command("CopyFilePath", [[ let @+ = expand('%:p')  ]], {})

local function OpenInVS()
    local file = vim.fn.expand('%')
    local row = vim.api.nvim_win_get_cursor(0)[1]
    vim.cmd("!devenv " .. file .. " /edit " .. file .. " /command 'edit.goto " .. row .. "'")
end
vim.api.nvim_create_user_command("OpenInVS", OpenInVS, {})
vim.api.nvim_create_user_command("ToVS", OpenInVS, {})

vim.api.nvim_create_user_command("Open", function(opts)
    vim.cmd("e "..opts.args)
end, { nargs = 1, complete = "dir" })

vim.api.nvim_create_user_command("Rg", function(opts) vim.cmd("Telescope live_grep search_dirs="..( opts.args or "." )) end, { nargs = 1, complete = "dir" })

vim.api.nvim_create_user_command("Fd", function(opts) vim.cmd("Telescope fd search_dirs="..( opts.args or "." )) end, { nargs = 1, complete = "dir" })

vim.api.nvim_create_user_command("Switch", function(opts)
    local filetype = vim.api.nvim_buf_get_option(vim.api.nvim_win_get_buf(0), 'filetype')
    if ( filetype=="cpp" or filetype=="h" ) then
        vim.cmd[[ ClangdSwitchSourceHeader ]]
    end
end, { nargs = 0 })

vim.api.nvim_create_user_command("ProfileStart", function()
    require'plenary.profile'.start("profile.log") 
end, {})
vim.api.nvim_create_user_command("ProfileStartFlame", function()
    require'plenary.profile'.start("profile.log", {flame = true}) 
end, {})
vim.api.nvim_create_user_command("ProfileStop", function()
    require'plenary.profile'.stop()
end, {})
vim.api.nvim_create_user_command("Cd", function(opts)
    global.pwd = opts.args
end, { nargs = 1, complete = "dir"})

return {
    ClearTerm = ClearTerm,
    OpenInVS = OpenInVS,
}
