local function codecompanion_fidget()
    local progress = require("fidget.progress")

    local M = {}

    function M:init()
        local group = vim.api.nvim_create_augroup("CodeCompanionFidgetHooks", {})

        vim.api.nvim_create_autocmd({ "User" }, {
            pattern = "CodeCompanionRequestStarted",
            group = group,
            callback = function(request)
                local handle = M:create_progress_handle(request)
                M:store_progress_handle(request.data.id, handle)
            end,
        })

        vim.api.nvim_create_autocmd({ "User" }, {
            pattern = "CodeCompanionRequestFinished",
            group = group,
            callback = function(request)
                local handle = M:pop_progress_handle(request.data.id)
                if handle then
                    M:report_exit_status(handle, request)
                    handle:finish()
                end
            end,
        })
    end

    M.handles = {}

    function M:store_progress_handle(id, handle)
        M.handles[id] = handle
    end

    function M:pop_progress_handle(id)
        local handle = M.handles[id]
        M.handles[id] = nil
        return handle
    end

    function M:create_progress_handle(request)
        return progress.handle.create({
            title = " Requesting assistance (" .. request.data.strategy .. ")",
            message = "In progress...",
            lsp_client = {
                name = M:llm_role_title(request.data.adapter),
            },
        })
    end

    function M:llm_role_title(adapter)
        local parts = {}
        table.insert(parts, adapter.formatted_name)
        if adapter.model and adapter.model ~= "" then
            table.insert(parts, "(" .. adapter.model .. ")")
        end
        return table.concat(parts, " ")
    end

    function M:report_exit_status(handle, request)
        if request.data.status == "success" then
            handle.message = "Completed"
        elseif request.data.status == "error" then
            handle.message = " Error"
        else
            handle.message = "󰜺 Cancelled"
        end
    end

    return M
end

local function config_codecompanion()
    local ollama_server = vim.env.OLLAMA_SERVER or "http://localhost:11434"
    local host = vim.env.HOST
    if host == "wangzi-nuc" or host == "wangzi-asus" then
        ollama_server = "http://wangzi-pc.wg:11434"
    end

    local function ollama_adapter(model)
        return function()
            local config = require("codecompanion.adapters").extend("ollama", {
                schema = {
                    model = {
                        default = model,
                    },
                },
                env = {
                    url = ollama_server,
                    chat_url = "/v1/chat/completions",
                },
            })
            return config
        end
    end

    local function get_api_config(name)
        local Path = require("plenary.path")

        local key_path = Path:new("/run/secrets/ai")
        if not key_path:exists() then
            key_path = Path:new(vim.loop.os_homedir()) / ".ai.json"
        end

        local json = vim.json.decode(key_path:read())
        local config = json[name]
        local api_key = config.key
        local url = config.url
        local model = config.model

        return {
            schema = { model = { default = model, }, },
            env = {
                url = url,
                api_key = api_key,
                chat_url = "/v1/chat/completions",
            },
        }
    end

    require("codecompanion").setup({
        display = {
            diff = {
                provider = "mini_diff",
            },
        },
        opts = {
            language = "Chinese",
            send_code = true,
        },
        strategies = {
            chat = {
                adapter = "deepseek",
                slash_commands = {
                    -- add the vectorcode command here.
                    codebase = require("vectorcode.integrations").codecompanion.chat.make_slash_command(),
                },
                tools = {
                    vectorcode = {
                        description = "Run VectorCode to retrieve the project context.",
                        callback = require("vectorcode.integrations").codecompanion.chat.make_tool(),
                    }
                },
            },
            inline = {
                adapter = "copilot",
            },
        },
        adapters = {
            ollama = ollama_adapter(vim.env.OLLAMA_MODEL or "deepseek-r1:7b"),
            ollama_deepseek_r1 = ollama_adapter("deepseek-r1:7b"),
            ollama_deepseek_coder = ollama_adapter("deepseek-coder-v2:16b"),
            openai = function()
                local config = get_api_config("openai")
                return require("codecompanion.adapters").extend("openai_compatible", config)
            end,
            deepseek = function()
                local config = get_api_config("deepseek")
                return require("codecompanion.adapters").extend("deepseek", config)
            end,
        },
    })
    require("telescope").load_extension("codecompanion")
    codecompanion_fidget():init()
end

return {
    {
        "github/copilot.vim",
        dir = gen.copilot_vim,
        name = "copilot_vim",
        event = { "VeryLazy" },
        config = function()
            vim.keymap.set('i', '<C-J>', 'copilot#Accept("\\<CR>")', {
                expr = true,
                replace_keycodes = false,
                silent = true,
            })
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
            "fidget_nvim",
            {
                "Davidyz/VectorCode",
                dir = gen.vectorcode,
                name = "vectorcode",
                module = "vectorcode",
                version = "*", -- optional, depending on whether you're on nightly or release
                dependencies = { "plenary_nvim" },
                config = function()
                    local found_vectorcode_command = vim.fn.executable("vectorcode")
                    if not found_vectorcode_command then
                        vim.notify("VectorCode command not found. Please install VectorCode.", vim.log.levels.INFO)
                        return
                    end

                    vim.api.nvim_create_autocmd("LspAttach", {
                        callback = function()
                            local bufnr = vim.api.nvim_get_current_buf()
                            local cacher = require("vectorcode.config").get_cacher_backend()
                            cacher.async_check("config", function()
                                cacher.register_buffer(
                                    bufnr,
                                    {
                                        n_query = 10,
                                    }
                                )
                            end, nil)
                        end,
                        desc = "Register buffer for VectorCode",
                    })
                    require("vectorcode").setup({
                        async_opts = {
                            debounce = 10,
                            events = { "BufWritePost", "InsertEnter", "BufReadPost" },
                            exclude_this = true,
                            n_query = 5,
                            notify = false,
                            query_cb = require("vectorcode.utils").make_surrounding_lines_cb(-1),
                            run_on_register = true,
                        },
                        async_backend = "lsp",
                        exclude_this = true,
                        n_query = 5,
                        notify = true,
                        timeout_ms = 5000,
                        on_setup = {
                            update = false,
                        }
                    })
                end
            }
        },
        config = config_codecompanion,
        cmd = { "CodeCompanion", "CodeCompanionActions", "CodeCompanionChat" },
        module = "codecompanion",
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
            { "<leader>aa", [[<cmd>CodeCompanionActions<CR>]],     mode = { "n", "v" }, desc = "AI Actions" },
            { "<leader>at", [[<cmd>CodeCompanionChat Toggle<CR>]], desc = "AI Chat" },
        },
    },
}
