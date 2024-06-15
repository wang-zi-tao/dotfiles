local util = require("perfanno.util")
local bgcolor = vim.fn.synIDattr(vim.fn.hlID("Normal"), "bg", "gui")

require("perfanno").setup({
    -- List of highlights that will be used to highlight hot lines (or nil to disable highlighting)
    line_highlights = util.make_bg_highlights(bgcolor, "#d19a66", 10),
    -- Highlight used for virtual text annotations (or nil to disable virtual text)
    vt_highlight = util.make_fg_highlight("#d19a66"),

    -- Annotation formats that can be cycled between via :PerfCycleFormat
    --   "percent" controls whether percentages or absolute counts should be displayed
    --   "format" is the format string that will be used to display counts / percentages
    --   "minimum" is the minimum value below which lines will not be annotated
    -- Note: this also controls what shows up in the telescope finders
    formats = {
        { percent = true, format = "%.2f%%", minimum = 0.5 },
        { percent = false, format = "%d", minimum = 1 },
    },

    -- Automatically annotate files after :PerfLoadFlat and :PerfLoadCallGraph
    annotate_after_load = true,
    -- Automatically annoate newly opened buffers if information is available
    annotate_on_open = true,

    -- Options for telescope-based hottest line finders
    telescope = {
        -- Enable if possible, otherwise the plugin will fall back to vim.ui.select
        enabled = pcall(require, "telescope"),
        -- Annotate inside of the preview window
        annotate = true,
    },

    -- Node type patterns used to find the function that surrounds the cursor
    ts_function_patterns = {
        -- These should work for most languages (at least those used with perf)
        default = {
            "function",
            "method",
        },
        -- Otherwise you can add patterns for specific languages like:
        -- weirdlang = {
        --     "weirdfunc",
        -- }
    },
})

local telescope = require("telescope")
local actions = telescope.extensions.perfanno.actions
telescope.setup({
    extensions = {
        perfanno = {
            -- Special mappings in the telescope finders
            mappings = {
                ["i"] = {
                    -- Find hottest callers of selected entry
                    ["<C-h>"] = actions.hottest_callers,
                    -- Find hottest callees of selected entry
                    ["<C-l>"] = actions.hottest_callees,
                },

                ["n"] = {
                    ["gu"] = actions.hottest_callers,
                    ["gd"] = actions.hottest_callees,
                },
            },
        },
    },
})
