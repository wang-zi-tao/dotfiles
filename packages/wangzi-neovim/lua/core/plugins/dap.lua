local function config()
    local dap = require("dap")
    local dapui = require("dapui")

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

    dap.adapters.lldb = {
        type = "executable",
        command = "lldb-vscode", -- adjust as needed
        name = "lldb",
    }
    local path_cache = vim.fn.getcwd() .. "/"
    local ip_cache = "localhost:1234"

    dap.configurations.cpp = {
        {
            name = "cargo run",
            type = "lldb",
            request = "launch",
            program = function()
                path_cache = vim.fn.input("Path to executable: ", path_cache, "file")
                return path_cache
            end,
            cwd = "${workspaceFolder}",
            -- stopOnEntry = true,
        },
        {
            name = "Launch",
            type = "lldb",
            request = "launch",
            program = function()
                path_cache = vim.fn.input("Path to executable: ", path_cache, "file")
                return path_cache
            end,
            cwd = "${workspaceFolder}",
            -- stopOnEntry = true,
        },
        {
            name = "attach",
            type = "lldb",
            request = "attach",
            processId = require("dap.utils").pick_process,
            cwd = "${workspaceFolder}",
            -- stopOnEntry = true,
        },
        {
            name = "Launch file",
            type = "cppdbg",
            request = "launch",
            program = function()
                return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file", "file")
            end,
            cwd = "${workspaceFolder}",
            stopOnEntry = true,
        },
        {
            name = "Attach to gdbserver :1234",
            type = "cppdbg",
            request = "launch",
            MIMode = "gdb",
            miDebuggerServerAddress = function()
                ip_cache = vim.fn.input("ip address to attach: ", ip_cache, "")
                return ip_cache
            end,
            cwd = "${workspaceFolder}",
            program = function()
                return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
            end,
        },
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
        args = { "/dev/golang/vscode-go/dist/debugAdapter.js" },
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
end

return {
    "mfussenegger/nvim-dap",
    dir = gen.dap,
    name = "dap",
    module = "dap",
    lazy = true,
    config = config,
    init = function()
        require("which-key").add({
            { "<leader>d",  group = "Debugger" },
            { "<leader>du", group = "Debugger UI" },
        })
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
            function()
                require("dap").run()
            end,
            desc = "Run",
        },
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
                require("dapui").eval(expr)
            end,
            desc = "Eval expr",
        },
        {
            "<leader>de",
            function()
                require("dapui").eval()
            end,
            mode = "v",
            desc = "Eval expr",
        },
        {
            "<leader>de",
            function()
                require("dapui").eval()
            end,
            mode = "n",
            desc = "Eval expr",
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
                require("dap").close()
            end,
            mode = "n",
            desc = "Close",
        },
        {
            "\\-",
            function()
                require("dap").step_over()
            end,
            mode = "n",
            desc = "Step over",
        },
        {
            "<F11>",
            function()
                require("dap").step_over()
            end,
            mode = "n",
            desc = "Step over",
        },
        {
            "\\0",
            function()
                require("dap").step_into()
            end,
            mode = "n",
            desc = "Step Into",
        },
        {
            "<F10>",
            function()
                require("dap").step_into()
            end,
            mode = "n",
            desc = "Step into",
        },
        {
            "\\5",
            function()
                require("dap").run()
            end,
            mode = "n",
            desc = "Run",
        },
        {
            "<F5>",
            function()
                require("dap").run()
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
            name = "dap_ui",
            module = "dapui",
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
            config = function()
                require("dapui").setup({
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
                                { id = "scopes",  size = 0.25 },
                                { id = "watches", size = 0.16 },
                                "stacks",
                                { id = "breakpoints", size = 0.1 },
                            },
                            size = 40, -- 40 columns
                            position = "left",
                        },
                        {
                            elements = {
                                "repl",
                                "console",
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
                })
            end,
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
                    all_references = false,                -- show virtual text on all all references of the variable (not only definitions)
                    filter_references_pattern = "<module", -- filter references (not definitions) pattern when all_references is activated (Lua gmatch pattern, default filters out Python modules)
                    -- experimental features:
                    virt_text_pos = "inline",              -- position of virtual text, see `:h nvim_buf_set_extmark()`
                    all_frames = true,                     -- show virtual text for all stack frames not only current. Only works for debugpy on my machine.
                    virt_lines = false,                    -- show virtual lines instead of virtual text (will flicker!)
                    virt_text_win_col = nil,               -- position the virtual text at a fixed window column (starting from the first text column) ,
                    -- e.g. 80 to position at column 80, see `:h nvim_buf_set_extmark()`
                    display_callback = function(variable, buf, stackframe, node, options)
                        if options.virt_text_pos == "inline" then
                            return " = " .. variable.value
                        else
                            return variable.name .. " = " .. variable.value
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
    },
}
