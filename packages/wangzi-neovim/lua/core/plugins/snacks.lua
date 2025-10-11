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

local function in_git()
    return Snacks.git.get_root() ~= nil
end

local symbol_filter = {
    "Class",
    "Constructor",
    "Enum",
    "Field",
    "Function",
    "Interface",
    "Method",
    "Module",
    "Namespace",
    "Package",
    "Property",
    "Struct",
    "Trait",
}

return {
    "folke/snacks.nvim",
    name = "snacks",
    dir = gen.snacks,
    priority = 70,
    lazy = false,
    dependencies = { "tokyonight", "nvim_web_devicons", "trouble_nvim" },
    keys = {
        {
            "<leader>fw",
            function()
                pcall(require("core.utils").add_mark)
                Snacks.picker.grep({
                    ignored = true,
                    dirs = { require("core.utils").pwd or "." },
                })
            end,
            mode = { "n" },
            desc = "Grep",
        },
        {
            "<leader>fW",
            function()
                pcall(require("core.utils").add_mark)
                Snacks.picker.grep_word({
                    ignored = true,
                    dirs = { require("core.utils").pwd or "." },
                })
            end,
            mode = { "n" },
            desc = "Grep word",
        },
        {
            "<leader>fd",
            function()
                Snacks.picker.lsp_symbols({
                    filter = { default = symbol_filter, },
                })
            end,
            desc = "Find LSP Symbols"
        },
        {
            "<leader>fi",
            function()
                pcall(require("core.utils").add_mark)
                Snacks.picker.lsp_references()
            end,
            desc = "LSP Reference",
        },
        {
            "<leader>fD",
            function()
                Snacks.picker.lsp_declarations()
            end,
            desc = "lsp definitions",
        },
        {
            "<leader>fI",
            function()
                pcall(require("core.utils").add_mark)
                Snacks.picker.lsp_implementations()
            end,
            desc = "LSP Implementation",
        },
        {
            "<leader>ft",
            function()
                pcall(require("core.utils").add_mark)
                Snacks.picker.lsp_definitions()
            end,
            desc = "LSP Define",
        },
        {
            "<leader>fT",
            function()
                pcall(require("core.utils").add_mark)
                Snacks.picker.lsp_type_definitions()
            end,
            desc = "LSP TypeDefinition",
        },
        {
            "<leader>fj",
            function()
                pcall(require("core.utils").add_mark)
                Snacks.picker.jumps()
            end,
            desc = "Jump list",
        },
        {
            "<leader>fu",
            function()
                Snacks.picker.undo()
            end,
            desc = "Undo list",
        },
        {
            "<leader>f/",
            function()
                Snacks.picker.search_history()
            end,
            desc = "Search history",
        },
        {
            "<leader>f:",
            function()
                Snacks.picker.command_history()
            end,
            desc = "command history",
        },
    },
    opts = {
        bigfile = {
            enabled = true,
        },
        explorer = {
            enabled = true,
        },
        picker = {
            db = vim.g.sqlite_clib_path,
            ui_select = true,
        },
        input = {
            enabled = true,
            position = "float",
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
                { section = "keys",   gap = 1, padding = 1 },
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
    },
    config = function(_, opts)


        require("snacks").setup(vim.tbl_deep_extend("force", opts, {
            picker = {
                actions = require("trouble.sources.snacks").actions,
                win = {
                    input = {
                        keys = {
                            ["<c-t>"] = {
                                "trouble_open",
                                mode = { "n", "i" },
                            },
                        },
                    },
                },
            },
        }))
        Snacks.input.enable()
    end,
}
