local function config()
    local snacks = require("snacks")

    local ascii = [[ 
    ⣴⣶⣤⡤⠦⣤⣀⣤⠆     ⣈⣭⣿⣶⣿⣦⣼⣆            
     ⠉⠻⢿⣿⠿⣿⣿⣶⣦⠤⠄⡠⢾⣿⣿⡿⠋⠉⠉⠻⣿⣿⡛⣦         
           ⠈⢿⣿⣟⠦ ⣾⣿⣿⣷    ⠻⠿⢿⣿⣧⣄       
            ⣸⣿⣿⢧ ⢻⠻⣿⣿⣷⣄⣀⠄⠢⣀⡀⠈⠙⠿⠄      
           ⢠⣿⣿⣿⠈    ⣻⣿⣿⣿⣿⣿⣿⣿⣛⣳⣤⣀⣀     
    ⢠⣧⣶⣥⡤⢄ ⣸⣿⣿⠘  ⢀⣴⣿⣿⡿⠛⣿⣿⣧⠈⢿⠿⠟⠛⠻⠿⠄    
   ⣰⣿⣿⠛⠻⣿⣿⡦⢹⣿⣷   ⢊⣿⣿⡏  ⢸⣿⣿⡇ ⢀⣠⣄⣾⠄     
  ⣠⣿⠿⠛ ⢀⣿⣿⣷⠘⢿⣿⣦⡀ ⢸⢿⣿⣿⣄ ⣸⣿⣿⡇⣪⣿⡿⠿⣿⣷⡄    
  ⠙⠃   ⣼⣿⡟  ⠈⠻⣿⣿⣦⣌⡇⠻⣿⣿⣷⣿⣿⣿ ⣿⣿⡇ ⠛⠻⢷⣄   
       ⢻⣿⣿⣄   ⠈⠻⣿⣿⣿⣷⣿⣿⣿⣿⣿⡟ ⠫⢿⣿⡆       
        ⠻⣿⣿⣿⣿⣶⣶⣾⣿⣿⣿⣿⣿⣿⣿⣿⡟⢀⣀⣤⣾⡿⠃       
    ]]

    local in_git = Snacks.git.get_root() ~= nil

    snacks.setup({
        bigfile = {
            enabled = true,
        },
        dashboard = {
            enabled = true,
            preset = {
                header = ascii,
                keys = {
                    { icon = " ", key = "s", desc = "Restore Session", section = "session" },
                    { icon = " ", key = "f", desc = "Find File", action = "<cmd>Telescope fd no_ignore=true<CR>" },
                    { icon = " ", key = "g", desc = "Find Text", action = "<cmd>Telescope live_grep no_ignore=true<CR>" },
                    { icon = " ", key = "r", desc = "Recent Files", action = ":lua Snacks.dashboard.pick('oldfiles')" },
                    { icon = " ", key = "c", desc = "Config", action = ":lua Snacks.dashboard.pick('files', {cwd = vim.fn.stdpath('config')})" },
                    { icon = " ", key = "n", desc = "New File", action = ":ene | startinsert" },
                    { icon = "󰒲 ", key = "L", desc = "Lazy", action = ":Lazy", enabled = package.loaded.lazy ~= nil },
                    { icon = " ", key = "q", desc = "Quit", action = ":qa" },
                },
            },
            sections = {
                { section = "header" },
                { section = "keys", gap = 1, padding = 1 },
                { section = "startup" },
                {
                    pane = 2,
                    padding = 1,
                    height = 2,
                    icon = " ",
                    title = "Git Branch",
                    section = "terminal",
                    enabled = in_git,
                    cmd = "git branch --show-current",
                    ttl = 5 * 60,
                },
                {
                    pane = 2,
                    height = 10,
                    padding = 1,
                    icon = " ",
                    title = "Git Status",
                    section = "terminal",
                    cmd = "git --no-pager diff --stat -B -M -C",
                    enabled = in_git,
                    ttl = 5 * 60,
                },
                { pane = 2, icon = " ", title = "Recent Files", section = "recent_files", indent = 2, padding = 1 },
                { pane = 2, icon = " ", title = "Projects", section = "projects", indent = 2, padding = 1 },
            },
        },
    })
end

return {
    "folke/snacks.nvim",
    name = "snacks",
    priority = 70,
    lazy = false,
    dependencies = { "tokyonight", "nvim_web_devicons" },
    config = config,
}
