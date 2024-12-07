local function config_codecompanion()
    require("codecompanion").setup({
        display = {
            diff = {
                provider = "mini_diff",
            },
        },
        opts = {
            send_code = false,
        },
        strategies = {
            chat = {
                adapter = "openai",
            },
            inline = {
                adapter = "copilot",
            },
        },
        adapters = {
            ollama = function()
                local ollama_server = vim.env.OLLAMA_SERVER or "http://localhost:11434"
                local host = vim.env.HOST
                if host == "wangzi-nuc" or host == "wangzi-asus" then
                    ollama_server = "http://wangzi-pc.wg:11434"
                end
                local config = require("codecompanion.adapters").extend("openai_compatible", {
                    schema = {
                        model = {
                            default = vim.env.OLLAMA_MODEL or "deepseek-coder:6.7b",
                        },
                    },
                    env = {
                        url = ollama_server,
                        chat_url = "/v1/chat/completions",
                    },
                })
                return config
            end,
            openai = function()
                local Path = require("plenary.path")
                local key_path = Path:new(vim.loop.os_homedir()) / ".openapi.key"
                local api_key = key_path:read():match("^%s*(.-)%s*$")
                local config = require("codecompanion.adapters").extend("openai_compatible", {
                    schema = {
                        model = {
                            default = "gpt-4o",
                        },
                    },
                    env = {
                        url = "https://api.chatanywhere.tech",
                        api_key = api_key,
                        chat_url = "/v1/chat/completions",
                    },
                })
                return config
            end,
        },
    })
end

local function config_chatgpt()
    require("chatgpt").setup({
        api_type_cmd = 'echo azure',
        azure_api_base_cmd = 'echo https://api.chatanywhere.tech/',
        api_key_cmd = 'cat %userprofile%/.openapi.key',
        azure_api_engine_cmd = 'echo chat',
        azure_api_version_cmd = 'echo 2023-05-15'
    })
end

return {
    {
        "github/copilot.vim",
        dir = gen.copilot_vim,
        name = "copilot_vim",
        event = { "VeryLazy" },
        config = function()
            vim.keymap.set('i', '<C-J>', 'copilot#Accept("\\<CR>")', { expr = true, replace_keycodes = false })
            vim.keymap.set('i', '<C-\\>', 'copilot#Accept("\\<CR>")', { expr = true, replace_keycodes = false })
            vim.g.copilot_no_tab_map = true
        end,
    },
    {
        "olimorris/codecompanion.nvim",
        dir = gen.codecompanion,
        name = "codecompanion",
        dependencies = {
            "plenary_nvim",
            "nvim_treesitter",
            "nvim_cmp",
            "telescope_nvim",
            "dressing_nvim",
            "copilot_vim",
        },
        config = config_codecompanion,
        event = "VeryLazy",
        keys = {
            {
                "<leader>ac",
                function()
                    require("core.utils").cachedinput("prompt", "prompt", "", nil, function(input)
                        vim.cmd.CodeCompanion(input)
                    end)
                end,
                mode = { "n", "v" },
                desc = "generate code"
            },
            { "<leader>aa", [[<cmd>CodeCompanionActions<CR>]],     mode = { "n", "v" }, desc = "GPT Actions" },
            { "<leader>at", [[<cmd>CodeCompanionChat Toggle<CR>]], desc = "GPT Chat" },
        },
    },
}
