vim.cmd([[let $GIT_EDITOR = 'nvr -cc split --remote-wait']])
vim.cmd([[let $EDITOR = 'nvr -cc split --remote-wait']])

if vim.fn.has("win32") == 1 then
    require("plenary.job"):new({
        command = "powershell",
        args = { "-c", [[Get-WmiObject Win32_process -filter 'name = "nvim.exe"' | foreach-object { $_.SetPriority(128) }]] }
    }):start()
end
