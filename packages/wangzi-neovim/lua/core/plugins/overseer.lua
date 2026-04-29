local heirline_component = function()
    local Spacer = { provider = " " }
    local function rpad(child)
        return {
            condition = child.condition,
            child,
            Spacer,
        }
    end

    local function OverseerTasksForStatus(status)
        return {
            condition = function(self)
                return self.tasks[status]
            end,
            provider = function(self)
                return string.format("%s%d", self.symbols[status], #self.tasks[status])
            end,
            hl = function(self)
                return {
                    fg = self.colors[status],
                }
            end,
        }
    end

    local Overseer = {
        condition = function()
            return package.loaded.overseer
        end,
        init = function(self)
            local tasks = require("overseer.task_list").list_tasks({ unique = true })
            local tasks_by_status = require("overseer.util").tbl_group_by(tasks, "status")
            self.tasks = tasks_by_status
        end,
        on_click = {
            callback = function()
                require("overseer").toggle()
            end,
            name = "overseer_status",
        },
        static = {
            symbols = {
                ["CANCELED"] = " ",
                ["FAILURE"] = "󰅚 ",
                ["SUCCESS"] = "󰄴 ",
                ["RUNNING"] = "󰑮 ",
            },
            colors = {
                ["CANCELED"] = "orange",
                ["FAILURE"] = "red",
                ["SUCCESS"] = "green",
                ["RUNNING"] = "cyan",
            }
        },

        rpad(OverseerTasksForStatus("CANCELED")),
        rpad(OverseerTasksForStatus("RUNNING")),
        rpad(OverseerTasksForStatus("SUCCESS")),
        rpad(OverseerTasksForStatus("FAILURE")),
    }

    return Overseer
end

local function config()
    local overseer = require("overseer")
    overseer.setup({
        dap = true,
        templates = { "builtin", },
        output = {
            use_terminal = true,
        },
        actions = {
            ["on finish"] = {
                condition = function(task, status)
                    return status == "SUCCESS" or status == "FAILURE"
                end,
                run = function(task, status)
                    if status == "SUCCESS" then
                        vim.notify(string.format("Task '%s' completed successfully.", task.name), vim.log.levels.INFO)
                    elseif status == "FAILURE" then
                        vim.notify(string.format("Task '%s' failed with status: %s", task.name, status),
                            vim.log.levels.ERROR)
                        overseer.open()
                    end
                end,
            }
        },
        component_aliases = {
            msvc_quickfix = {
                "on_output_quickfix",
                errorformat = [[ %#%f(%l\,%c): error C%n: %m [%.%#"]],
                items_only = true,
                open_on_match = true,
                relative_file_root = ".",
            },
            msvc_link_quickfix = {
                "on_output_quickfix",
                errorformat = [[ %#%o : error LNK%n: %m]],
                items_only = true,
                open_on_match = true,
                relative_file_root = ".",
            },
            default = {
                "on_exit_set_status",
                { "on_complete_notify" },
                "on_result_diagnostics_trouble",
                "unique",
                { "open_output",       on_complete = "failure", on_start = "never" },
                "msvc_quickfix",
                "msvc_link_quickfix",
            },
        }
    })

    overseer.register_template({
        name = "Git checkout",
        params = function()
            local stdout = vim.system({ "git", "branch", "--format=%(refname:short)" }):wait().stdout or ""
            local branches = vim.split(stdout, "\n", { trimempty = true })
            return {
                branch = {
                    desc = "Branch to checkout",
                    type = "enum",
                    choices = branches,
                },
            }
        end,
        builder = function(params)
            return {
                cmd = { "git", "checkout", params.branch },
            }
        end,
    })
end

return {
    "stevearc/overseer.nvim",
    dir = gen.overseer,
    name = "overseer",
    module = "overseer",
    cmd = {
        "OverseerRun",
        "OverseerToggle",
        "OverseerClose",
        "OverseerLoadBundle",
        "OverseerDeleteBundle",
    },
    config = config,
    Overseer = heirline_component,
    keys = {
        { "<leader>to", "<cmd>OverseerToggle<cr>",       desc = "Toggle Overseer" },
        { "<leader>tr", "<cmd>OverseerRun<cr>",          desc = "Run Task" },
        { "<leader>tc", "<cmd>OverseerClose<cr>",        desc = "Close Task" },
        { "<leader>tl", "<cmd>OverseerLoadBundle<cr>",   desc = "Load Task" },
        { "<leader>td", "<cmd>OverseerDeleteBundle<cr>", desc = "Delete Task" },
    }
}
