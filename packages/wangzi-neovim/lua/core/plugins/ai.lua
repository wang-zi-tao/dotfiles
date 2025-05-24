local found_vectorcode_command = vim.fn.executable("vectorcode") ~= 0

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
            schema = {
                schema = { default = 0.0, },
                model = { default = model, },
            },
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
                    codebase = found_vectorcode_command and
                        require("vectorcode.integrations").codecompanion.chat.make_slash_command(),
                },
                tools = {
                    vectorcode = found_vectorcode_command and {
                        description = "Run VectorCode to retrieve the project context.",
                        callback = require("vectorcode.integrations").codecompanion.chat.make_tool(),
                    },
                    mcp = {
                        -- calling it in a function would prevent mcphub from being loaded before it's needed
                        callback = function() return require("mcphub.extensions.codecompanion") end,
                        description = "Call tools and resources from the MCP Servers",
                    }
                },
            },
            inline = {
                adapter = "deepseek_v3",
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
            deepseek_v3 = function()
                local config = get_api_config("deepseek-v3")
                return require("codecompanion.adapters").extend("deepseek", config)
            end,
        },
    })
    require("telescope").load_extension("codecompanion")
    codecompanion_fidget():init()
end

local function config_cmp_ai()
    local cmp_ai = require('cmp_ai.config')

    cmp_ai:setup({
        max_lines = 1000,
        provider = 'Ollama',
        provider_options = {
            -- model = 'qwen2.5-coder:7b-base-q6_K',
            model = 'deepseek-r1:7b',
            auto_unload = false, -- Set to true to automatically unload the model when
            -- exiting nvim.
            prompt = function(prefix, suffix)
                -- local retrieval_results = require("vectorcode").query(prefix .. " " .. suffix, {
                --     n_query = 5,
                -- })
                local file_context = ""
                -- for _, source in pairs(retrieval_results) do
                --     -- This works for qwen2.5-coder.
                --     file_context = file_context
                --         .. "<|file_sep|>"
                --         .. source.path
                --         .. "\n"
                --         .. source.document
                --         .. "\n"
                -- end
                return file_context
                    .. "<|fim_prefix|>"
                    .. prefix
                    .. "<|fim_suffix|>"
                    .. suffix
                    .. "<|fim_middle|>"
            end
        },
        notify = true,
        notify_callback = function(msg)
            vim.notify(msg)
        end,
        run_on_every_keystroke = true,
        ignored_file_types = {
            -- default is not to ignore
            -- uncomment to ignore in lua:
            -- lua = true
        },
    })
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
            "mcphub",
            {
                "Davidyz/VectorCode",
                dir = gen.vectorcode,
                name = "vectorcode",
                module = "vectorcode",
                version = "*", -- optional, depending on whether you're on nightly or release
                dependencies = { "plenary_nvim" },
                enabled = found_vectorcode_command,
                config = function()
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
            { "<leader>aa", [[<cmd>CodeCompanionActions<CR>]], mode = { "n", "v" }, desc = "AI Actions" },
            {
                "<leader>at",
                function()
                    -- require("mcphub")
                    vim.cmd [[CodeCompanionChat Toggle]]
                end,
                desc = "AI Chat"
            },
        },
    },
    {
        "tzachar/cmp-ai",
        name = "cmp_ai",
        dir = gen.cmp_ai,
        module = "cmp_ai",
        lazy = true,
        dependencies = {
            "plenary_nvim",
        },
        config = config_cmp_ai
    },
    {
        "Davidyz/VectorCode",
        dir = gen.vectorcode,
        name = "vectorcode",
        module = "vectorcode",
        enabled = found_vectorcode_command,
        version = "*", -- optional, depending on whether you're on nightly or release
        -- enabled = false,
        dependencies = { "plenary_nvim" },
        config = function()
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
                timeout_ms = 32000,
                on_setup = {
                    update = false,
                }
            })
        end
    },
    {
        "ravitemer/mcphub.nvim",
        name = "mcphub",
        dir = gen.mcphub,
        module = "mcphub",
        dependencies = { "plenary_nvim" },
        cmd = "MCPHub",
        build = vim.fn.has("win32") == 1 and "npm install -g mcp-hub@latest",
        keys = {
            { "<leader>am", "<cmd>MCPHub<CR>", desc = "MCPHub" },
        },
        config = function()
            local home = vim.loop.os_homedir()
            local Path = require("plenary.path")
            local mcpconfig = Path:new(home .. "/.config/mcphub/servers.json")
            if not mcpconfig:exists() then
                local mcpconfig_dir = Path:new(home .. "/.config/mcphub")
                mcpconfig_dir:mkdir({ parents = true })
                mcpconfig:write([[{"mcpServers":{}}]], "w")
            end

            require("mcphub").setup({
                extensions = {
                    codecompanion = {
                        -- Show the mcp tool result in the chat buffer
                        show_result_in_chat = true,
                        make_vars = true,           -- make chat #variables from MCP server resources
                        make_slash_commands = true, -- make /slash_commands from MCP server prompts
                    },
                },
                cmd = gen.mcp_hub and gen.mcp_hub .. "/bin/mcp-hub",
            })
        end,
    }
}
