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
                    fg = "cyan",
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
        static = {
            symbols = {
                ["CANCELED"] = " ",
                ["FAILURE"] = "󰅚 ",
                ["SUCCESS"] = "󰄴 ",
                ["RUNNING"] = "󰑮 ",
            },
        },

        rpad(OverseerTasksForStatus("CANCELED")),
        rpad(OverseerTasksForStatus("RUNNING")),
        rpad(OverseerTasksForStatus("SUCCESS")),
        rpad(OverseerTasksForStatus("FAILURE")),
    }

    return Overseer
end

local function config()
    require('overseer').setup({
        dap = true,
        templates = { "builtin", },
        strategy = {
            "toggleterm",
        },
    })

    require("overseer").register_template({
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
