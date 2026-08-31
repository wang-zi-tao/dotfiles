local found_vectorcode_command = vim.fn.executable("vectorcode") ~= 0
local enable_vectorcode = false

local prompt_library = {
}

local function hermes_acp()
  local helpers = require("codecompanion.adapters.acp.helpers")
  return {
    name = "hermes",
    formatted_name = "Hermes",
    type = "acp",
    roles = {
      llm = "assistant",
      user = "user",
    },
    commands = {
      default = {
        "hermes",
        "acp"
      },
    },
    defaults = {
      mcpServers = {},
      timeout = 20000, -- 20 seconds
    },
    parameters = {
      protocolVersion = 1,
      clientCapabilities = {
        fs = { readTextFile = true, writeTextFile = true },
      },
      clientInfo = {
        name = "CodeCompanion.nvim",
        version = "1.0.0",
      },
    },
    handlers = {
      setup = function(self)
        return true
      end,
      auth = function(self)
        return true
      end,
      form_messages = function(self, messages, capabilities)
        return helpers.form_messages(self, messages, capabilities)
      end,
      on_exit = function(self, code) end,
    },
  }
end

local function codecompanion_fidget()
  local progress = require("fidget.progress")

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
      title = " Requesting assistance",
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

local function ollama_adapter(ollama_server, model)
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

local function getAiJson()
  local Path = require("plenary.path")

  local key_path = Path:new("/run/secrets/ai")
  if not key_path:exists() then
    key_path = Path:new(vim.loop.os_homedir()) / ".ai.json"
  end

  local json = vim.json.decode(key_path:read())
  return json
end

local function create_key_env(name, key)
  local api_key_name = "AI_APIKEY_" .. name
  vim.fn.setenv(api_key_name, key)
  return api_key_name
end

local function get_al_api_config()
  local json = getAiJson()

  local configs = {}
  for name, config in pairs(json) do
    local config = json[name]
    local api_key = config.key
    local url = config.url
    local model = config.model

    configs[name] = {
      schema = {
        schema = { default = 0.0, },
        model = { default = model, },
      },
      env = {
        url = url,
        api_key = create_key_env(model, api_key),
        raw_api_key = api_key,
        chat_url = "/v1/chat/completions",
      },
    }
  end
  return configs
end

local function get_api_config(name)
  local json = getAiJson()
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
      api_key = create_key_env(model, api_key),
      raw_api_key = api_key,
      chat_url = "/v1/chat/completions",
    },
  }
end

local function launch_codemem_serve()
  local Job = require("plenary.job")
  local task = Job:new({
    command = "codemem",
    args = { "serve" },
    on_stderr = function(error, data, self)
      vim.notify("codemem stderr: " .. data, "WARN")
    end,
    on_exit = function(self, code, signal)
      vim.notify("codemem exit with code " .. code, "ERROR")
    end
  })
  task:start()
end

local function config_codecompanion()
  local ollama_server = vim.env.OLLAMA_SERVER or "http://localhost:11434"
  local host = vim.env.HOST
  if host == "wangzi-nuc" or host == "wangzi-asus" then
    ollama_server = "http://wangzi-pc.wg:11434"
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
      log_level = "TRACE",
      -- system_prompt = system_prompt,
    },
    strategies = {
      chat = {
        adapter = {
          name = "opencode",
          model = "moonshotai-cn/kimi-k2.5",
        },
        slash_commands = {
        },
        tools = {
          mcp = {
            -- calling it in a function would prevent mcphub from being loaded before it's needed
            callback = function() return require("mcphub.extensions.codecompanion") end,
            description = "Call tools and resources from the MCP Servers",
          }
        },
      },
      inline = {
        adapter = "deepseek_flash",
      },
    },
    adapters = {
      http = {
        ollama_deepseek_r1 = ollama_adapter(ollama_server, "deepseek-r1:8b"),
        ollama_qwen3 = ollama_adapter(ollama_server, "qwen3:8b"),
        openai = function()
          local config = get_api_config("openai")
          return require("codecompanion.adapters").extend("openai_compatible", config)
        end,
        deepseek = function()
          local config = get_api_config("deepseek-pro")
          return require("codecompanion.adapters").extend("deepseek", config)
        end,
        deepseek_flash = function()
          local config = get_api_config("deepseek-flash")
          return require("codecompanion.adapters").extend("deepseek", config)
        end,
        wps_deepseek = function()
          local config = get_api_config("wps-deepseek-pro")
          return require("codecompanion.adapters").extend("deepseek", config)
        end,
        wps_deepseek_flash = function()
          local config = get_api_config("wps-deepseek-flash")
          return require("codecompanion.adapters").extend("deepseek", config)
        end,
        wps_glm = function()
          local config = get_api_config("wps-glm")
          return require("codecompanion.adapters").extend("deepseek", config)
        end,
        deepseek_flash_free = function()
          local config = get_api_config("deepseek-v4-flash-free")
          return require("codecompanion.adapters").extend("openai_responses", config)
        end,
        gpt = function()
          local config = get_api_config("gpt")
        end,
        kimi = function()
          local config = get_api_config("kimi")
          return require("codecompanion.adapters").extend("openai_compatible", config)
        end,
      },
      acp = {
        opts = {
          show_presets = true,
          show_model_choices = true,
        },

        hermes_acp = hermes_acp,

        opencode = function()
          return require("codecompanion.adapters").extend("opencode", {

            opts = {
              vision = true,
              trim_tool_output = true,
            },
          })
        end,
      }
    },
    prompt_library = prompt_library,
    extensions = {
      vectorcode = enable_vectorcode and {
        ---@type VectorCode.CodeCompanion.ExtensionOpts
        opts = {
          tool_group = {
            -- this will register a tool group called `@vectorcode_toolbox` that contains all 3 tools
            enabled = true,
            -- a list of extra tools that you want to include in `@vectorcode_toolbox`.
            -- if you use @vectorcode_vectorise, it'll be very handy to include
            -- `file_search` here.
            extras = {},
            collapse = false, -- whether the individual tools should be shown in the chat
          },
          tool_opts = {
            ---@type VectorCode.CodeCompanion.ToolOpts
            ["*"] = {},
            ---@type VectorCode.CodeCompanion.LsToolOpts
            ls = {},
            ---@type VectorCode.CodeCompanion.VectoriseToolOpts
            vectorise = {},
            ---@type VectorCode.CodeCompanion.QueryToolOpts
            query = {
              max_num = { chunk = -1, document = -1 },
              default_num = { chunk = 50, document = 10 },
              include_stderr = false,
              use_lsp = true,
              no_duplicate = true,
              chunk_mode = false,
              ---@type VectorCode.CodeCompanion.SummariseOpts
              summarise = {
                ---@type boolean|(fun(chat: CodeCompanion.Chat, results: VectorCode.QueryResult[]):boolean)|nil
                enabled = false,
                adapter = nil,
                query_augmented = true,
              }
            },
            files_ls = {},
            files_rm = {}
          }
        },
      } or nil,
    }
  })
  require("telescope").load_extension("codecompanion")
  codecompanion_fidget():init()
end

local opencode_cmd = { 'opencode', '-c', '--port' }
---@type snacks.terminal.Opts
local snacks_terminal_opts = {
  win = {
    position = 'right',
    enter = true,
    width = 0.25,
    winbar = "",
    wo = {
      number = false,
      relativenumber = false,
      winbar = "",
    }
  },
}


return {
  found_vectorcode_command = found_vectorcode_command,
  enable_vectorcode = enable_vectorcode,
  {
    "github/copilot.vim",
    dir = gen.copilot_vim,
    enabled = false,
    name = "copilot.vim",
    event = { "VeryLazy" },
    cmd = { "Copilot" },
    config = function()
      vim.keymap.set('i', '<C-J>', 'copilot#Accept("\\<CR>")', {
        expr = true,
        replace_keycodes = false,
        silent = true,
      })
      vim.keymap.set('i', '<C-\\>', 'copilot#Accept("\\<CR>")', { expr = true, replace_keycodes = false })
      vim.g.copilot_no_tab_map = true

      launch_codemem_serve()
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
      "copilot.vim",
      "fidget_nvim",
      "mcphub",
      {
        "Davidyz/VectorCode",
        dir = gen.vectorcode,
        name = "vectorcode",
        module = "vectorcode",
        version = "*", -- optional, depending on whether you're on nightly or release
        dependencies = { "plenary_nvim" },
        enabled = enable_vectorcode,
        config = function()
          if not enable_vectorcode then
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
      { "<leader>aa", [[<cmd>CodeCompanionChat adapter=opencode<CR>]], mode = { "n", "v" }, desc = "AI Agent" },
      { "<leader>at", "<cmd>CodeCompanionChat adapter=deepseek<CR>",   desc = "AI Chat" },
    },
  },
  {
    'milanglacier/minuet-ai.nvim',
    dir = gen.minuet_ai,
    name = "minuet_ai",
    lazy = true,
    config = function()
      local utils = require("core.utils")
      local llm = get_api_config("deepseek-flash")
      require('minuet').setup {
        provider = 'openai_fim_compatible',
        provider_options = {
          openai_fim_compatible = {
            api_key = llm.env.api_key,
            model = 'deepseek-v4-flash',
            name = 'deepseek',
            optional = {
              max_tokens = 256,
              top_p = 0.9,
            },
          },
        },
        cmp = {
          enable_auto_complete = true,
        },
        virtualtext = {
          -- Specify the filetypes to enable automatic virtual text completion,
          -- e.g., { 'python', 'lua' }. Note that you can still invoke manual
          -- completion even if the filetype is not on your auto_trigger_ft list.
          auto_trigger_ft = utils.file_type_whitelist,
          -- specify file types where automatic virtual text completion should be
          -- disabled. This option is useful when auto-completion is enabled for
          -- all file types i.e., when auto_trigger_ft = { '*' }
          auto_trigger_ignore_ft = utils.file_type_blacklist,
          keymap = {
            accept = "<A-a>",
            accept_line = "<A-s>",
            accept_n_lines = nil,
            -- Cycle to next completion item, or manually invoke completion
            next = nil,
            -- Cycle to prev completion item, or manually invoke completion
            prev = nil,
            dismiss = nil,
          },
          -- Whether show virtual text suggestion when the completion menu
          -- (nvim-cmp or blink-cmp) is visible.
          show_on_completion_menu = false,
        },
      }
    end,
  },
  {
    "Davidyz/VectorCode",
    dir = gen.vectorcode,
    name = "vectorcode",
    module = "vectorcode",
    enabled = enable_vectorcode,
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
  },
  {
    "nickjvandyke/opencode.nvim",
    name = "opencode",
    dir = gen.opencode,
    keys = {
      {
        "<leader>ao",
        function()
          require("opencode").ask("@this: ")
        end,
        desc = "Ask OpenCode…"
      },
      {
        "<leader>as",
        function()
          require("opencode").select()
        end,
        desc = "Select OpenCode…"
      },
      {
        "go",
        function()
          return require("opencode").operator("@this ")
        end,
        expr = true,
        desc = "Append range to OpenCode",
        mode = { "n", "x" }
      },
      {
        "<leader>ta",
        function()
          vim.schedule(function()
            require('snacks.terminal').toggle(opencode_cmd, snacks_terminal_opts)
          end)
        end,
        expr = true,
        desc = "toggle OpenCode",
        mode = { "n", "x" }
      },
    },
    config = function()
      ---@type opencode.Opts
      vim.g.opencode_opts = {
        server = {
          -- Your configuration, if any; goto definition on the type for details
          start = function()
            require('snacks.terminal').open(opencode_cmd, snacks_terminal_opts)
          end,
          stop = function()
            local win = require('snacks.terminal').get(opencode_cmd, { create = false })
            if win then
              win:destroy()
            end
          end,
          toggle = function()
            local win = require('snacks.terminal').get(opencode_cmd, { create = false })
            if win then
              win:toggle()
            end
          end
        },
      }

      vim.api.nvim_create_autocmd('User', {
        pattern = { 'OpencodeEvent:tui.command.execute' },
        callback = function(args)
          ---@type opencode.server.Event
          local event = args.data.event
          if event.properties.command == 'prompt.submit' then
            local win = require('snacks.terminal').get(opencode_cmd, { create = false })
            if win then
              win:show()
            end
          end
        end,
      })

      vim.o.autoread = true -- Required for `vim.g.opencode_opts.events.reload`
    end,
  }
}
