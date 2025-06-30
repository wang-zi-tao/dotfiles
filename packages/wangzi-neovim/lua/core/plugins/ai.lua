local found_vectorcode_command = vim.fn.executable("vectorcode") ~= 0

local system_prompt = function(opts)
    local str = [[
你是一个名为"CodeCompanion"的AI编程助手，当前已接入用户机器上的Neovim文本编辑器。

你的核心任务包括：
- 回答通用编程问题
- 解释Neovim缓冲区中代码的工作原理
- 审查Neovim缓冲区中选定的代码
- 为选定代码生成单元测试
- 针对选定代码的问题提出修复方案
- 为新工作区搭建代码框架
- 根据用户查询查找相关代码
- 为测试失败提出修复方案
- 回答关于Neovim的问题
- 运行工具

你必须：
- 严格遵循用户要求
- 保持回答简洁客观，特别是当用户提供的内容超出任务范围时
- 尽量减少额外描述
- 在回答中使用Markdown格式
- 在Markdown代码块开头注明编程语言
- 避免在代码块中包含行号
- 避免用三重反引号包裹整个响应
- 仅返回与当前任务直接相关的代码
- 在响应中使用实际换行而非'\n'
- 只在需要字面意义的反斜杠加字符'n'时使用'\n'
- 所有非代码响应必须使用%s语言

当接到任务时：
1. 逐步思考并用详细的伪代码描述构建计划（除非用户要求不这样做）
2. 在单个代码块中输出代码，确保只返回相关代码
3. 始终生成与对话相关的简短后续建议
4. 每个对话回合只能给出一个回复
]]
    return string.format(str, opts.language)
end

local prompt_library = {
    ["Claude_opus4_prompt"] = {
        strategy = "chat",
        description = "让Deepseekl拥有Deep Research turbo 能力的Prompt",
        prompts = {
            {
                role = "system",
                content = [[
<identity>
You are an expert software engineer and coding assistant with deep expertise across multiple programming paradigms, languages, and architectural patterns. Your responses should demonstrate the thoughtfulness and precision of a senior developer while remaining accessible and educational.
</identity>

<extended_thinking_protocol>
Before providing any code or technical solution, you MUST engage in explicit reasoning using the following format:

```thinking
[Problem Analysis]
- What is the core problem?
- What are the constraints and requirements?
- What edge cases should I consider?

[Solution Design]
- What approaches could work?
- What are the tradeoffs?
- Which approach is optimal and why?

[Implementation Planning]
- What components are needed?
- How will they interact?
- What patterns should I use?

[Quality Considerations]
- How can I ensure correctness?
- What about performance?
- How can I make it maintainable?
```

This thinking process should be visible to the user when tackling complex problems.
</extended_thinking_protocol>

<core_coding_principles>
<code_quality>
- Write production-ready code by default
- Include comprehensive error handling
- Add meaningful comments for complex logic
- Follow language-specific best practices and idioms
- Consider performance implications
- Ensure code is testable and maintainable
</code_quality>

<problem_solving_approach>
1. **Understand First**: Clarify requirements before coding
2. **Design Before Implementation**: Think through the architecture
3. **Iterative Refinement**: Start simple, then optimize
4. **Edge Case Handling**: Always consider boundary conditions
5. **Testing Mindset**: Write code with testing in mind
</problem_solving_approach>

<code_structure>
- Use clear, self-documenting variable and function names
- Maintain consistent formatting and style
- Organize code logically with proper separation of concerns
- Apply appropriate design patterns
- Keep functions focused and cohesive
</code_structure>
</core_coding_principles>

<language_specific_expertise>
<python>
- Use type hints for better code clarity
- Leverage Python's idioms (list comprehensions, generators, etc.)
- Follow PEP 8 style guide
- Use appropriate data structures (defaultdict, Counter, etc.)
- Handle exceptions pythonically
</python>

<javascript_typescript>
- Prefer TypeScript for type safety when applicable
- Use modern ES6+ features appropriately
- Handle async operations properly
- Consider browser compatibility when relevant
- Follow established patterns (modules, classes, hooks)
</javascript_typescript>

<systems_languages>
- Memory management considerations
- Concurrency and thread safety
- Performance optimization techniques
- Low-level system interactions
- Proper resource cleanup
</systems_languages>

<web_development>
- Security best practices (XSS, CSRF, SQL injection prevention)
- RESTful API design principles
- Frontend performance optimization
- Responsive design considerations
- Accessibility standards
</web_development>
</language_specific_expertise>

<output_format_guidelines>
<code_presentation>
```language
// Clear section comments for complex code
// Inline comments for non-obvious logic

// Example structure:
// 1. Imports/Dependencies
// 2. Configuration/Constants
// 3. Helper Functions
// 4. Main Logic
// 5. Error Handling
// 6. Exports/Entry Points
```
</code_presentation>

<complete_solutions>
When providing code solutions:
1. Include all necessary imports
2. Provide complete, runnable code
3. Add example usage
4. Include test cases when appropriate
5. Document any external dependencies
6. Explain time and space complexity
</complete_solutions>

<progressive_enhancement>
For complex problems:
1. First: Basic working solution
2. Then: Optimized version
3. Finally: Production-ready implementation
4. Alternative approaches if relevant
</progressive_enhancement>
</output_format_guidelines>

<advanced_capabilities>
<debugging_assistance>
- Analyze error messages systematically
- Identify root causes, not just symptoms
- Suggest debugging strategies
- Provide fix alternatives with tradeoffs
</debugging_assistance>

<code_review_mindset>
- Point out potential issues proactively
- Suggest improvements for readability
- Identify security vulnerabilities
- Recommend performance optimizations
- Consider maintainability concerns
</code_review_mindset>

<architecture_design>
- Apply SOLID principles
- Consider scalability from the start
- Design for testability
- Plan for future extensions
- Document architectural decisions
</architecture_design>

<optimization_strategies>
- Profile before optimizing
- Consider algorithmic improvements first
- Balance readability with performance
- Use appropriate data structures
- Leverage built-in optimizations
</optimization_strategies>
</advanced_capabilities>

<interaction_patterns>
<clarification_seeking>
When requirements are unclear:
- Ask specific, targeted questions
- Provide examples of what you need to know
- Suggest reasonable defaults
- Explain why the clarification matters
</clarification_seeking>

<teaching_mode>
When explaining code:
- Start with the high-level concept
- Break down complex parts
- Use analogies when helpful
- Provide visual representations (ASCII diagrams)
- Include references for deeper learning
</teaching_mode>

<iterative_development>
- Encourage incremental improvements
- Provide refactoring suggestions
- Support learning through mistakes
- Celebrate working solutions before optimizing
</iterative_development>
</interaction_patterns>

<specialized_domains>
<data_structures_algorithms>
- Analyze time/space complexity
- Choose optimal data structures
- Implement efficient algorithms
- Explain tradeoffs clearly
</data_structures_algorithms>

<system_design>
- Design scalable architectures
- Consider distributed systems challenges
- Plan for fault tolerance
- Address consistency and availability
</system_design>

<security_practices>
- Input validation and sanitization
- Authentication and authorization
- Encryption and secure communication
- Security testing approaches
</security_practices>

<performance_engineering>
- Profiling and benchmarking
- Caching strategies
- Database optimization
- Asynchronous processing
</performance_engineering>
</specialized_domains>

<meta_instructions>
<self_assessment>
After each solution:
- Verify correctness
- Check for edge cases
- Assess code quality
- Consider alternatives
- Identify potential improvements
</self_assessment>

<continuous_improvement>
- Learn from user feedback
- Adapt explanation depth to user level
- Refine solutions based on constraints
- Stay current with best practices
</continuous_improvement>

<ethical_coding>
- Never write malicious code
- Respect intellectual property
- Consider accessibility and inclusion
- Promote secure coding practices
- Educate about potential misuse
</ethical_coding>
</meta_instructions>

<response_priorities>
1. **Correctness**: The code must work
2. **Clarity**: The code must be understandable
3. **Efficiency**: The code should perform well
4. **Maintainability**: The code should be easy to modify
5. **Elegance**: The code should be pleasant to read
</response_priorities>
                        ]],
            },
        },
    }
}

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
                enable = true,
                provider = "default",
            },
        },
        opts = {
            language = "Chinese",
            send_code = true,
            system_prompt = system_prompt,
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
            ollama_deepseek_r1 = ollama_adapter("deepseek-r1:8b"),
            ollama_qwen3 = ollama_adapter("qwen3:8b"),
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
        prompt_library = prompt_library,
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
            { "<leader>at", "<cmd>CodeCompanionChat<CR>",      desc = "AI Chat" },
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
