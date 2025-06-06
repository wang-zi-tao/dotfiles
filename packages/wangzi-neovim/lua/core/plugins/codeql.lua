return {
    "pwntester/codeql.nvim",
    dir = gen.codeql,
    name = "codeql",
    event = "LspAttach",
    dependencies = {
        "nui_nvim",
        "telescope_nvim",
        "nvim_web_devicons",
        "nvim_window_picker",
    },
    opts = {},
    config = function()
        require("codeql").setup {
            results = {
                max_paths = 10,
                max_path_depth = nil,
            },
            panel = {
                width = 50,
                pos = "botright",
                group_by = "sink", -- "source"
                show_filename = true,
                long_filename = false,
                context_lines = 3,
            },
            max_ram = 32000,
            job_timeout = 15000,
            format_on_save = true,
            additional_packs = {
                "./.codeql",
            },
            mappings = {
                run_query = { modes = { "n" }, lhs = "<leader>qr", desc = "run query" },
                quick_eval = { modes = { "x", "n" }, lhs = "<leader>qe", desc = "quick evaluate" },
                quick_eval_predicate = { modes = { "n" }, lhs = "<leader>qp", desc = "quick evaluate enclosing predicate" },
            },
        }
    end,
    setup = function()

    end
}
