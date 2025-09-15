---@type dap.AdapterFactory
local function vsdbg_adapter(callback)
    local Job = require("plenary.job")
    local rpc = require('dap.rpc')
    local scan = require 'plenary.scandir'
    local home = vim.fn.expand("~")

    local vscode_extensions_dir = (home .. "/.vscode/extensions")
    local vscode_cpptoools_dir = "."
    local plugins = scan.scan_dir(vscode_extensions_dir, { depth = 1, only_dirs = true })
    for _, dir in ipairs(plugins) do
        if string.match(dir, "ms%-vscode%.cpptools%-[0-9.]+%-win32%-x64") then
            vscode_cpptoools_dir = dir
        end
    end
    if vscode_cpptoools_dir == "." then
        vim.notify("vscode cpptools not found, please install it first", vim.log.levels.ERROR)
        return
    end

    local vsdbg_js_adapter = [[
        /* declare module vsda {
	        export class signer {
		        sign(arg: string): string;
	        }
	        export class validator {
		        createNewMessage(arg: string): string;
		        validate(arg: string): 'ok' | 'error';
	        }
        } */
        const vsda_location = 'C:/Program Files/Microsoft VS Code/resources/app/node_modules/vsda/build/Release/vsda.node';
        const a /* : typeof vsda */ = require(vsda_location);
        const signer /* : vsda.signer */ = new a.signer();
        process.argv.forEach((value, index, array) => {
        if (index >= 1) {
            const r = signer.sign(value);
            console.log(r);
        }
        });
    ]]

    local function vsdbg_send_payload(client, payload)
        local msg = rpc.msg_with_content_length(vim.json.encode(payload))
        client.write(msg)
    end

    local function vsdbg_RunHandshake(self, request_payload)
        local job = Job:new({
            command = "node",
            args = { "-e", vsdbg_js_adapter, request_payload.arguments.value },
            on_exit = function(job, code)
                if code ~= 0 then
                    vim.schedule(function()
                        vim.notify('error while signing handshake', vim.log.levels.ERROR)
                    end)
                    return
                end

                local signature = string.gsub(job:result()[1], '\n', '')
                local response = {
                    type = "response",
                    seq = 0,
                    command = "handshake",
                    request_seq = request_payload.seq,
                    success = true,
                    body = {
                        signature = signature
                    }
                }
                vsdbg_send_payload(self.client, response)
            end,
            enable_recording = true,
        })
        job:sync()
    end

    callback({
        id = 'cppvsdbg',
        type = 'executable',
        command = vscode_cpptoools_dir .. [[\debugAdapters\vsdbg\bin\vsdbg.exe]],
        args = {
            "--interpreter=vscode",
            '--engineLogging=' .. vim.fn.stdpath("log") .. '/vsdbg.log',
        },
        options = {
            externalTerminal = true,
            logging = {
                moduleLoad = false,
            }
        },
        runInTerminal = true,
        reverse_request_handlers = {
            handshake = vsdbg_RunHandshake,
        },
    })
end

local function init()
    local dap = require("dap")
    local dapui = require("dapui")
    local util = require("core.utils")
    local Path = require("plenary.path")

    dap.listeners.before.attach.dapui_config = function()
        dapui.open()
    end
    dap.listeners.before.launch.dapui_config = function()
        dapui.open()
    end
    dap.listeners.before.event_terminated.dapui_config = function()
        dapui.close()
    end
    dap.listeners.before.event_exited.dapui_config = function()
        dapui.close()
    end
    dap.defaults.fallback.auto_continue_if_many_stopped = false

    dap.adapters.gdb = {
        type = "executable",
        command = "gdb",
        args = { "--interpreter=dap", "--eval-command", "set print pretty on" }
    }

    dap.adapters.lldb = {
        type = "executable",
        command = "lldb-vscode", -- adjust as needed
        name = "lldb",
    }

    local home = vim.fn.expand("~")

    dap.adapters.cppvsdbg = vsdbg_adapter

    local get_program = util.get_program
    local get_coredmp = util.get_coredmp

    local function vsdbg_find_natvis()
        local file = "C:/dotfiles-install/all.natvis"

        if Path:new(file):exists() then
            return file
        end
        return nil
    end

    local vsdbg_symbolSearchPath = "srv*;srv*http://localhost:8001"
    local vsdbg_config = {
        type = "cppvsdbg",
        clientID = 'vscode',
        clientName = 'Visual Studio Code',
        externalTerminal = true,
        columnsStartAt1 = true,
        linesStartAt1 = true,
        locale = "zh-CN",
        pathFormat = "path",
        externalConsole = false,
        cwd = "${workspaceFolder}",
        visualizerFile = vsdbg_find_natvis,
        initCommands = ".childdbg 1",
        showDisplayString = true,
        stopAtEntry = false,
        logging = {
            moduleLoad = true,
        },
        -- symbolSearchPath = "srv*;srv*http://localhost:8001;srv*https://msdl.microsoft.com/download/symbols",
    }

    local lldb_config = {
        type = "lldb",
        program = get_program,
        cwd = "${workspaceFolder}",
        initCommands = "settings set target.process.follow-fork-mode child",
    }

    local gdb_config = {
        type = "gdb",
        cwd = "${workspaceFolder}",
        stopAtBeginningOfMainSubprogram = false,
        setupCommands = {
            {
                description = "Enable pretty-printing for gdb",
                text = "-enable-pretty-printing",
                ignoreFailures = true,
            },
            -- {
            --     -- https://sourceware.org/gdb/onlinedocs/gdb/Forks.html
            --     description = "Fork follows Child process",
            --     text = "set follow-fork-mode child",
            --     ignoreFailures = true
            -- },
            {
                -- https://sourceware.org/gdb/onlinedocs/gdb/Forks.html
                description = "Fork will keep the other process attached to debugger",
                text = "set detach-on-fork off",
                ignoreFailures = true
            }
        },
    }

    dap.configurations.cpp = {
        vim.tbl_deep_extend("force", lldb_config, {
            name = "cargo run",
            request = "launch",
            program = get_program,
            cwd = "${workspaceFolder}",
        }),
        vim.tbl_deep_extend("force", lldb_config, {
            name = "lldb launch",
            request = "launch",
            program = get_program,
        }),
        vim.tbl_deep_extend("force", lldb_config, {
            name = "lldb attach",
            processId = util.pick_process,
            -- stopOnEntry = true,
        }),
        vim.tbl_deep_extend("force", lldb_config, {
            name = "lldb remote attach",
            request = "attach",
            processId = getPID,
            connect = {
                host = getIp,
                port = 1234,
            }
            -- stopOnEntry = true,
        }),
        vim.tbl_deep_extend("force", gdb_config, {
            name = "gdb launch",
            request = "launch",
            program = get_program,
        }),
        vim.tbl_deep_extend("force", gdb_config, {
            name = "gdb attach",
            program = get_program,
            processId = util.pick_process,
        }),
        {
            name = "Attach to gdbserver :1234",
            type = "cppdbg",
            request = "launch",
            MIMode = "gdb",
            miDebuggerServerAddress = util.get_debug_ip,
            cwd = "${workspaceFolder}",
            program = get_program,
        },
        {
            name = "cppdbg launch",
            type = "cppdbg",
            request = "launch",
            program = get_program,
            cwd = "${workspaceFolder}",
            stopOnEntry = true,
        },
        vim.tbl_deep_extend("force", vsdbg_config, {
            name = "vsdbg launch",
            request = "launch",
            program = get_program,
        }),
        vim.tbl_deep_extend("force", vsdbg_config, {
            name = "vsdbg launch(load symbol)",
            request = "launch",
            program = get_program,
            symbolSearchPath = vsdbg_symbolSearchPath,
        }),
        vim.tbl_deep_extend("force", vsdbg_config, {
            name = "vsdbg coredump",
            request = "launch",
            program = get_program,
            dumpPath = get_coredmp,
            symbolSearchPath = vsdbg_symbolSearchPath,
        }),
        vim.tbl_deep_extend("force", vsdbg_config, {
            name = "vsdbg attach",
            request = "attach",
            processId = util.pick_process,
            symbolSearchPath = vsdbg_symbolSearchPath,
        }),
        vim.tbl_deep_extend("force", vsdbg_config, {
            name = "vsdbg attach(without symbol)",
            request = "attach",
            processId = util.pick_process,
        }),
    }
    dap.adapters.cppdbg = {
        id = 'cppdbg',
        type = 'executable',
        command = gen.OpenDebugAD7 or 'OpenDebugAD7',
        options = {
            detached = false
        }
    }

    dap.configurations.c = dap.configurations.cpp
    dap.configurations.rust = dap.configurations.cpp

    dap.adapters.markdown = {
        type = "executable",
        name = "mockdebug",
        command = "node",
        args = { "./out/debugAdapter.js" },
        cwd = "path/to/vscode-mock-debug/",
    }

    dap.configurations.markdown = {
        {
            type = "mock",
            request = "launch",
            name = "mock test",
            program = "/path/to/a/readme.md",
            stopOnEntry = true,
            debugServer = 4711,
        },
    }

    -- https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_dap.md
    dap.adapters.go = {
        type = "executable",
        command = "node",
        args = { home .. "/dev/golang/vscode-go/dist/debugAdapter.js" },
    }
    dap.configurations.go = {
        {
            type = "go",
            name = "Debug",
            request = "launch",
            showLog = false,
            program = "${file}",
            dlvToolPath = vim.fn.exepath("dlv"), -- Adjust to where delve is installed
        },
    }

    dap.adapters.python = {
        type = "executable",
        command = "python",
        args = { "-m", "debugpy.adapter" },
    }
    dap.configurations.python = {
        {
            -- The first three options are required by nvim-dap
            type = "python", -- the type here established the link to the adapter definition: `dap.adapters.python`
            request = "launch",
            name = "Launch file",

            -- Options below are for debugpy, see https://github.com/microsoft/debugpy/wiki/Debug-configuration-settings for supported options

            program = "${file}", -- This configuration will launch the current file if used.
            pythonPath = function()
                local python_path_cache = "python"
                -- debugpy supports launching an application with a different interpreter then the one used to launch debugpy itself.
                -- The code below looks for a `venv` or `.venv` folder in the current directly and uses the python within.
                -- You could adapt this - to for example use the `VIRTUAL_ENV` environment variable.
                if vim.fn.executable("python") == 1 then
                    return "python"
                elseif vim.fn.executable("python3") == 1 then
                    return "python3"
                else
                    python_path_cache = vim.fn.input("Path to python: ", python_path_cache, "file")
                    return python_path_cache
                end
            end,
        },
    }

    dap.configurations.lua = {
        {
            type = 'nlua',
            request = 'attach',
            name = "Attach to running Neovim instance",
        }
    }
    dap.adapters.nlua = function(callback, config)
        callback({ type = 'server', host = config.host or "127.0.0.1", port = config.port or 8086 })
    end


    dap.adapters["pwa-node"] = function(callback, config, parent)
        local js_dap_path = (gen.vscode_js_debug or "./") .. "/lib/node_modules/js-debug/dist/src/dapDebugServer.js"
        if vim.fn.has("win32") == 1 then
            js_dap_path = (vim.env.DOTFILE_WINDOWS or "C:/dotfiles-windows/") .. "repo/js-debug/src/dapDebugServer.js"
        end

        local host = config.host or "localhost"
        local port = config.port or 9222
        callback({
            type = "server",
            host = host,
            port = port,
            executable = {
                command = "node",
                args = { js_dap_path, port },
            }
        })
    end

    dap.configurations.javascript = {
        {
            type = "pwa-node",
            request = "launch",
            name = "Node Launch file",
            program = "${file}",
            cwd = "${workspaceFolder}",
        },
        {
            type = "pwa-node",
            request = "attach",
            name = "Node attach",
            cwd = "${workspaceFolder}",
            port = 9229,
            restart = true,
        },
        {
            type = "pwa-node",
            request = "launch",
            name = "Deno Launch file",
            program = "${file}",
            cwd = "${workspaceFolder}",
            runtimeExecutable = "deno",
            attachSimplePort = 9229,
            runtimeArgs = {
                "run",
                "--inspect-wait",
                "--allow-all",
            },
        },
    }
    dap.configurations.typescript = dap.configurations.javascript
end

return {
    "mfussenegger/nvim-dap",
    dir = gen.dap,
    name = "dap",
    module = "dap",
    init = function()
        require("which-key").add({
            { "<leader>d",  group = "Debugger" },
            { "<leader>du", group = "Debugger UI" },
            { "<leader>df", group = "Debugger UI Float" },
            { "<leader>ds", group = "Debugger Telescope" },
        })
        init()
    end,
    keys = {
        {
            "<leader>dp",
            function()
                require("dap").pause()
            end,
            desc = "Pause",
        },
        {
            "<leader>dc",
            function()
                require("dap").continue()
            end,
            desc = "Continue",
        },
        {
            "<leader>dr",
            function()
                require("dap").continue()
            end,
            desc = "Continue",
        },
        {
            "<leader>dC",
            function()
                require("dap").close()
            end,
            desc = "Close",
        },
        {
            "<leader>do",
            function()
                require("dap").step_over()
            end,
            desc = "Step Over",
        },
        {
            "<leader>dO",
            function()
                require("dap").step_back()
            end,
            desc = "Step Back",
        },
        {
            "<leader>di",
            function()
                require("dap").step_into()
            end,
            desc = "Step Into",
        },
        {
            "<leader>dI",
            function()
                require("dap").step_out()
            end,
            desc = "Step Out",
        },
        {
            "<leader>dl",
            function()
                require("dap").run_to_cursor()
            end,
            desc = "Run to Corsor",
        },
        {
            "<leader>dr",
            function() require("dap").run() end,
            desc = "Run",
        },
        { "<leader>dn", [[<cmd>DapNew<CR>]], desc = "New session", },
        {
            "<leader>dR",
            function()
                require("dap").repl.open()
            end,
            desc = "Repl",
        },
        {
            "<leader>dE",
            function()
                local expr = vim.fn.input("Expr: ", "")
                require("dapui").eval(nil, { enter = true })
            end,
            desc = "Eval expr",
        },
        {
            "<leader>de",
            function()
                require("dapui").eval(nil, { enter = true })
            end,
            mode = { "n", "v" },
            desc = "Eval expr",
        },
        {
            "<leader>dh",
            function()
                require("dapui").float_element("eval", { enter = true })
            end,
            desc = "Eval",
        },
        {
            "<leader>dF",
            function()
                require("dapui").float_element(nil, { enter = true })
            end,
            desc = "Eval",
        },
        {
            "<leader>dfv",
            function()
                require("dapui").float_element("scopes", { enter = true })
            end,
            desc = "Scopes",
        },
        {
            "<leader>dfs",
            function()
                require("dapui").float_element("stacks", { enter = true })
            end,
            desc = "stacks",
        },
        {
            "<leader>dfr",
            function()
                require("dapui").float_element("repl", { enter = true })
            end,
            desc = "repl window",
        },
        {
            "<leader>dfb",
            function()
                require("dapui").float_element("breakpoints", { enter = true })
            end,
            desc = "breakpoints window",
        },
        {
            "<leader>dsb",
            function()
                vim.cmd [[Telescope dap list_breakpoints]]
            end,
            desc = "breakpoints",
        },
        {
            "<leader>dsf",
            function()
                vim.cmd [[Telescope dap frames]]
            end,
            desc = "frames",
        },
        {
            "<leader>dt",
            function()
                local dap = require("dap")
                local dapui = require("dapui")

                local sessions = dap.sessions()
                local sessions_index = {}
                for i, session in pairs(sessions) do
                    table.insert(sessions_index, i)
                end
                vim.ui.select(sessions_index, {
                    prompt = "Select a session",
                    format_item = function(sessions_index)
                        local session = sessions[sessions_index]
                        local config_name = session.config.name or "unnamed"
                        local frame = session.current_frame and "--" .. session.current_frame.name or ""
                        local thread_id = session.stopped_thread_id
                        local thread_info = ""
                        if thread_id then
                            local thread = session.threads[thread_id]
                            thread_info = string.format(" (thread %d: %s)", thread_id, thread.name or "unnamed")
                        end
                        return string.format(
                            "[%d]: %s %s %s",
                            session.id,
                            thread_info,
                            config_name,
                            frame
                        )
                    end,
                }, function(sessions_index)
                    local session = sessions[sessions_index]
                    if session then
                        dap.set_session(session)
                        dapui.close()
                        dapui.open()
                    else
                        vim.notify("No session selected", vim.log.levels.INFO)
                    end
                end)
            end,
            desc = "switch session",
        },
        {
            "\\9",
            function()
                require("persistent-breakpoints.api").toggle_breakpoint()
            end,
            mode = "n",
            desc = "BreakPoint",
        },
        {
            "<F9>",
            function()
                require("persistent-breakpoints.api").toggle_breakpoint()
            end,
            mode = "n",
            desc = "BreakPoint",
        },
        {
            "\\%",
            function()
                require("dap").close()
            end,
            mode = "n",
            desc = "Close",
        },
        {
            "<S-F5>",
            function()
                require("dap").continue()
            end,
            mode = "n",
            desc = "Continue",
        },
        {
            "<S-F5>",
            function()
                require("dap").close()
            end,
            mode = "n",
            desc = "Close",
        },
        {
            "\\-",
            function()
                require("dap").step_into()
            end,
            mode = "n",
            desc = "Step over",
        },
        {
            "<S-F11>",
            function()
                require("dap").step_out()
            end,
            mode = "n",
            desc = "Step out",
        },
        {
            "<A-F11>",
            function()
                require("dap").step_into({ steppingGranularity = "instruction" })
            end,
            mode = "n",
            desc = "Step over instruction",
        },
        {
            "<F11>",
            function()
                require("dap").step_into()
            end,
            mode = "n",
            desc = "Step over",
        },
        {
            "\\0",
            function()
                require("dap").step_over()
            end,
            mode = "n",
            desc = "Step Into",
        },
        {
            "<F10>",
            function()
                require("dap").step_over()
            end,
            mode = "n",
            desc = "Step into",
        },
        {
            "\\5",
            function()
                require("dap").continue()
            end,
            mode = "n",
            desc = "Run",
        },
        {
            "<F5>",
            function()
                require("dap").continue()
            end,
            mode = "n",
            desc = "Run",
        },
    },
    dependencies = {
        {
            "nvim-neotest/nvim-nio",
            dir = gen.nvim_nio,
            name = "nvim_nio",
            module = "nio",
            lazy = true,
        },
        {
            "rcarriga/nvim-dap-ui",
            dir = gen.dap_ui,
            name = "dapui",
            lazy = true,
            keys = {
                {
                    "<leader>duo",
                    function()
                        require("dapui").toggle()
                    end,
                    desc = "Open",
                },
                {
                    "<leader>duc",
                    function()
                        require("dapui").toggle()
                    end,
                    desc = "Close",
                },
            },
            opts = {
                icons = { expanded = "▾", collapsed = "▸" },
                mappings = {
                    -- Use a table to apply multiple mappings
                    expand = { "<CR>", "<2-LeftMouse>" },
                    open = "o",
                    remove = "d",
                    edit = "e",
                    repl = "r",
                    toggle = "t",
                },
                -- Expand lines larger than the window
                -- Requires >= 0.7
                expand_lines = vim.fn.has("nvim-0.7"),
                -- Layouts define sections of the screen to place windows.
                -- The position can be "left", "right", "top" or "bottom".
                -- The size specifies the height/width depending on position. It can be an Int
                -- or a Float. Integer specifies height/width directly (i.e. 20 lines/columns) while
                -- Float value specifies percentage (i.e. 0.3 - 30% of available lines/columns)
                -- Elements are the elements shown in the layout (in order).
                -- Layouts are opened in order so that earlier layouts take priority in window sizing.
                layouts = {
                    {
                        elements = {
                            -- Elements can be strings or table with id and size keys.
                            { id = "watches", size = 0.25 },
                            { id = "scopes",  size = 0.25 },
                            "stacks",
                            { id = "breakpoints", size = 0.1 },
                        },
                        size = 40, -- 40 columns
                        position = "left",
                    },
                    {
                        elements = {
                            "repl",
                            { id = "console", size = 0.25 }
                        },
                        size = 0.25, -- 25% of total lines
                        position = "bottom",
                    },
                },
                floating = {
                    max_height = nil,   -- These can be integers or a float between 0 and 1.
                    max_width = nil,    -- Floats will be treated as percentage of your screen.
                    border = "rounded", -- Border style. Can be "single", "double" or "rounded"
                    mappings = {
                        close = { "q", "<Esc>" },
                    },
                },
                windows = { indent = 1 },
                render = {
                    max_type_length = nil, -- Can be integer or nil.
                },
            },
            config = function(opts)
                require("dapui").setup(opts)
            end,
        },
        {
            "Jorenar/nvim-dap-disasm",
            dir = gen.dap_disasm,
            name = "nvim-dap-disasm",
            opts = {
                -- Add disassembly view to elements of nvim-dap-ui
                dapui_register = true,
                -- Add custom REPL commands for stepping with instruction granularity
                repl_commands = true,
                -- The sign to use for instruction the exectution is stopped at
                sign = "DapStopped",
                -- Number of instructions to show before the memory reference
                ins_before_memref = 16,
                -- Number of instructions to show after the memory reference
                ins_after_memref = 16,
                -- Labels of buttons in winbar
                controls = {
                    step_into = "Step Into",
                    step_over = "Step Over",
                    step_back = "Step Back",
                },
                -- Columns to display in the disassembly view
                columns = {
                    "address",
                    "instructionBytes",
                    "instruction",
                },
            },
            specs = {
                "rcarriga/nvim-dap-ui",
                dir = gen.dap_ui,
                name = "dapui",
                opts = {
                    layouts = {
                        {
                            elements = { { id = "disassembly" } },
                            position = "bottom",
                            size = 0.15,
                        },
                    }
                }
            },
            keys = {
                { "<leader>da", [[<cmd>DapDisasm<CR>]], desc = "Disassemble" }
            }
        },
        {
            "theHamsta/nvim-dap-virtual-text",
            dir = gen.dap_virtual_text,
            name = "dap_virtual_text",
            config = function()
                require("nvim-dap-virtual-text").setup({
                    enabled = true,                        -- enable this plugin (the default)
                    enabled_commands = true,               -- create commands DapVirtualTextEnable, DapVirtualTextDisable, DapVirtualTextToggle, (DapVirtualTextForceRefresh for refreshing when debug adapter did not notify its termination)
                    highlight_changed_variables = true,    -- highlight changed values with NvimDapVirtualTextChanged, else always NvimDapVirtualText
                    highlight_new_as_changed = true,       -- highlight new variables in the same way as changed variables (if highlight_changed_variables)
                    show_stop_reason = true,               -- show stop reason when stopped for exceptions
                    commented = true,                      -- prefix virtual text with comment string
                    only_first_definition = false,         -- only show virtual text at first definition (if there are multiple)
                    all_references = true,                 -- show virtual text on all all references of the variable (not only definitions)
                    filter_references_pattern = "<module", -- filter references (not definitions) pattern when all_references is activated (Lua gmatch pattern, default filters out Python modules)
                    -- experimental features:
                    virt_text_pos = "inline",              -- position of virtual text, see `:h nvim_buf_set_extmark()`
                    all_frames = true,                     -- show virtual text for all stack frames not only current. Only works for debugpy on my machine.
                    virt_lines = false,                    -- show virtual lines instead of virtual text (will flicker!)
                    virt_text_win_col = nil,               -- position the virtual text at a fixed window column (starting from the first text column) ,
                    -- e.g. 80 to position at column 80, see `:h nvim_buf_set_extmark()`
                    display_callback = function(variable, buf, stackframe, node, options)
                        ---@type string
                        local value = variable.value
                        if #value > 64 then
                            value = string.sub(value, 1, 64) .. "..."
                        end
                        if options.virt_text_pos == "inline" then
                            return " = " .. value
                        else
                            return variable.name .. " = " .. value
                        end
                    end,
                })
            end,
        },
        {
            "Weissle/persistent-breakpoints.nvim",
            dir = gen.persistent_breakpoints_nvim,
            name = "persistent_breakpoints_nvim",
            module = { "persistent-breakpoints", "persistent-breakpoints.api" },
            config = function()
                require("persistent-breakpoints").setup({
                    save_dir = vim.fn.stdpath("data") .. "/nvim_checkpoints",
                    -- record the performance of different function. run :lua require('persistent-breakpoints.api').print_perf_data() to see the result.
                    perf_record = false,
                    load_breakpoints_event = { "BufReadPost" },
                })
            end,
            keys = {
                {
                    "<leader>db",
                    function()
                        require("persistent-breakpoints.api").toggle_breakpoint()
                    end,
                    desc = "BreakPoint",
                },
                {
                    "<leader>dd",
                    function()
                        require("persistent-breakpoints.api").set_conditional_breakpoint()
                    end,
                    desc = "Conditional BreakPoint",
                },
                {
                    "<leader>dB",
                    function()
                        require("persistent-breakpoints.api").clear_all_breakpoints()
                    end,
                    desc = "Clear BreakPoint",
                },
            },
        },
        {
            "jbyuki/one-small-step-for-vimkind",
            name = "one_small_step_for_vimkind",
            dir = gen.one_small_step_for_vimkind,
            module = "osv",
            keys = {
                {
                    "<leader>dv",
                    function()
                        require("osv").launch({ port = 8086 })
                    end,
                    desc = "debug this neovim"
                },
            },
        },
    },
}
