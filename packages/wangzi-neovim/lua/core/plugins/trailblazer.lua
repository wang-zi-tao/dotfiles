local function config()
    require("trailblazer").setup({
        -- lang = "en",
        auto_save_trailblazer_state_on_exit = true,
        auto_load_trailblazer_state_on_enter = true,
        -- custom_session_storage_dir = vim.fn.stdpath("data") .. "/trailblazer/", -- i.e. "~/trail_blazer_sessions/"
        -- trail_options = {
        --     -- The trail mark priority sets the global render priority of trail marks in the sign/number
        --     -- column as well as the highlights within the text (e.g. Treesitter sets a value of 100).
        --     -- Make sure this value is higher than any other plugin you use to ensure that trail marks
        --     -- are always visible and don't get overshadowed.
        --     trail_mark_priority = 10001,
        --     -- Available modes to cycle through. Remove any you don't need.
        --     available_trail_mark_modes = {
        --         "global_chron",
        --         "global_buf_line_sorted",
        --         "global_fpath_line_sorted",
        --         "global_chron_buf_line_sorted",
        --         "global_chron_fpath_line_sorted",
        --         "global_chron_buf_switch_group_chron",
        --         "global_chron_buf_switch_group_line_sorted",
        --         "buffer_local_chron",
        --         "buffer_local_line_sorted"
        --     },
        --     -- The current / initially selected trail mark selection mode. Choose from one of the
        --     -- available modes: global_chron, global_buf_line_sorted, global_chron_buf_line_sorted,
        --     -- global_chron_buf_switch_group_chron, global_chron_buf_switch_group_line_sorted,
        --     -- buffer_local_chron, buffer_local_line_sorted
        --     current_trail_mark_mode = "global_chron",
        --     current_trail_mark_list_type = "quickfix", -- currently only quickfix lists are supported
        --     trail_mark_list_rows = 10, -- number of rows to show in the trail mark list
        --     verbose_trail_mark_select = true, -- print current mode notification on mode change
        --     mark_symbol = "", --  will only be used if trail_mark_symbol_line_indicators_enabled = true
        --     newest_mark_symbol = "", -- disable this mark symbol by setting its value to ""
        --     cursor_mark_symbol = "", -- disable this mark symbol by setting its value to ""
        --     next_mark_symbol = "", -- disable this mark symbol by setting its value to ""
        --     previous_mark_symbol = "", -- disable this mark symbol by setting its value to ""
        --     multiple_mark_symbol_counters_enabled = true,
        --     number_line_color_enabled = true,
        --     trail_mark_in_text_highlights_enabled = true,
        --     trail_mark_symbol_line_indicators_enabled = true, -- show indicators for all trail marks in symbol column
        --     symbol_line_enabled = true,
        --     default_trail_mark_stacks = {
        --         -- this is the list of trail mark stacks that will be created by default. Add as many
        --         -- as you like to this list. You can always create new ones in Neovim by using either
        --         -- `:TrailBlazerSwitchTrailMarkStack <name>` or `:TrailBlazerAddTrailMarkStack <name>`
        --         "default" -- , "stack_2", ...
        --     },
        --     available_trail_mark_stack_sort_modes = {
        --         "alpha_asc", -- alphabetical ascending
        --         "alpha_dsc", -- alphabetical descending
        --         "chron_asc", -- chronological ascending
        --         "chron_dsc", -- chronological descending
        --     },
        --     -- The current / initially selected trail mark stack sort mode. Choose from one of the
        --     -- available modes: alpha_asc, alpha_dsc, chron_asc, chron_dsc
        --     current_trail_mark_stack_sort_mode = "alpha_asc",
        --     -- Set this to true if you always want to move to the nearest trail mark first before
        --     -- continuing to peek move in the current selection mode order. This effectively disables
        --     -- the "current trail mark cursor" to which you would otherwise move first before continuing
        --     -- to move through your trail mark stack.
        --     move_to_nearest_before_peek = false,
        --     move_to_nearest_before_peek_motion_directive_up = "fpath_up", -- "up", "fpath_up" -> For more information see section "TrailBlazerMoveToNearest Motion Directives"
        --     move_to_nearest_before_peek_motion_directive_down = "fpath_down", -- "down", "fpath_down" -> For more information see section "TrailBlazerMoveToNearest Motion Directives"
        --     move_to_nearest_before_peek_dist_type = "lin_char_dist", -- "man_dist", "lin_char_dist" -> Manhattan Distance or Linear Character Distance
        -- },
        -- event_list = {
        --     -- Add the events you would like to add custom callbacks for here. For more information see section "Custom Events"
        --     -- "TrailBlazerTrailMarkStackSaved",
        --     -- "TrailBlazerTrailMarkStackDeleted",
        --     -- "TrailBlazerCurrentTrailMarkStackChanged",
        --     -- "TrailBlazerTrailMarkStackSortModeChanged"
        -- },
        -- mappings = { -- rename this to "force_mappings" to completely override default mappings and not merge with them
        --     nv = { -- Mode union: normal & visual mode. Can be extended by adding i, x, ...
        --         motions = {
        --             new_trail_mark = '<A-l>',
        --             track_back = '<A-b>',
        --             peek_move_next_down = '<A-J>',
        --             peek_move_previous_up = '<A-K>',
        --             move_to_nearest = '<A-n>',
        --             toggle_trail_mark_list = '<A-m>',
        --         },
        --         actions = {
        --             delete_all_trail_marks = '<A-L>',
        --             paste_at_last_trail_mark = '<A-p>',
        --             paste_at_all_trail_marks = '<A-P>',
        --             set_trail_mark_select_mode = '<A-t>',
        --             switch_to_next_trail_mark_stack = '<A-.>',
        --             switch_to_previous_trail_mark_stack = '<A-,>',
        --             set_trail_mark_stack_sort_mode = '<A-s>',
        --         },
        --     },
        --     -- You can also add/move any motion or action to mode specific mappings i.e.:
        --     -- i = {
        --     --     motions = {
        --     --         new_trail_mark = '<C-l>',
        --     --         ...
        --     --     },
        --     --     ...
        --     -- },
        -- },
        -- quickfix_mappings = { -- rename this to "force_quickfix_mappings" to completely override default mappings and not merge with them
        --     nv = {
        --         motions = {
        --             qf_motion_move_trail_mark_stack_cursor = "<CR>",
        --         },
        --         actions = {
        --             qf_action_delete_trail_mark_selection = "d",
        --             qf_action_save_visual_selection_start_line = "v",
        --         },
        --         alt_actions = {
        --             qf_action_save_visual_selection_start_line = "V",
        --         }
        --     },
        --     v = {
        --         actions = {
        --             qf_action_move_selected_trail_marks_down = "<C-j>",
        --             qf_action_move_selected_trail_marks_up = "<C-k>",
        --         }
        --     }
        -- },
        -- -- Your custom highlight group overrides go here
        -- hl_groups = {
        --     TrailBlazerTrailMark = {
        --         guifg = "LightSlateBlue",
        --         guibg = "none",
        --         gui = "bold",
        --     },
        --     TrailBlazerTrailMarkNext = {
        --         guifg = "Green",
        --         guibg = "none",
        --         gui = "bold",
        --     },
        --     TrailBlazerTrailMarkPrevious = {
        --         guifg = "Orange",
        --         guibg = "none",
        --         gui = "bold",
        --     },
        --     TrailBlazerTrailMarkCursor = {
        --         guifg = "White",
        --         guibg = "LightSlateBlue",
        --         gui = "bold",
        --     },
        --     TrailBlazerTrailMarkNewest = {
        --         guifg = "White",
        --         guibg = "LightSlateBlue",
        --         gui = "bold",
        --     },
        --     TrailBlazerTrailMarkCustomOrd = {
        --         guifg = "White",
        --         guibg = "LightSlateBlue",
        --         gui = "bold",
        --     },
        --     TrailBlazerTrailMarkGlobalChron = {
        --         guifg = "White",
        --         guibg = "Orange",
        --         gui = "bold",
        --     },
        --     TrailBlazerTrailMarkGlobalBufLineSorted = {
        --         guifg = "Black",
        --         guibg = "Orange",
        --         gui = "bold",
        --     },
        --     TrailBlazerTrailMarkGlobalFpathLineSorted = {
        --         guifg = "White",
        --         guibg = "Orange",
        --         gui = "bold",
        --     },
        --     TrailBlazerTrailMarkGlobalChronBufLineSorted = {
        --         guifg = "White",
        --         guibg = "Orange",
        --         gui = "bold",
        --     },
        --     TrailBlazerTrailMarkGlobalChronFpathLineSorted = {
        --         guifg = "White",
        --         guibg = "Orange",
        --         gui = "bold",
        --     },
        --     TrailBlazerTrailMarkGlobalChronBufSwitchGroupChron = {
        --         guifg = "White",
        --         guibg = "VioletRed",
        --         gui = "bold",
        --     },
        --     TrailBlazerTrailMarkGlobalChronBufSwitchGroupLineSorted = {
        --         guifg = "Black",
        --         guibg = "MediumSpringGreen",
        --         gui = "bold",
        --     },
        --     TrailBlazerTrailMarkBufferLocalChron = {
        --         guifg = "Black",
        --         guibg = "Green",
        --         gui = "bold",
        --     },
        --     TrailBlazerTrailMarkBufferLocalLineSorted = {
        --         guifg = "Black",
        --         guibg = "LightGreen",
        --         gui = "bold",
        --     },
        -- }
    })
end

return {
    "LeonHeidelbach/trailblazer.nvim",
    dir = gen.trailblazer,
    name = "trailblazer",
    lazy = true,
    event = "BufRead",
    module = "trailblazer",
    cmd = {
        "TrailBlazerNewTrailMark",
        "TrailBlazerNewTrailMark",
        "TrailBlazerPeekMovePreviousUp",
        "TrailBlazerPeekMoveNextDown",
        "TrailBlazerMoveToNearest",
        "TrailBlazerMoveToTrailMarkCursor",
        "TrailBlazerDeleteAllTrailMarks",
        "TrailBlazerPasteAtLastTrailMark",
        "TrailBlazerPasteAtAllTrailMarks",
        "TrailBlazerTrailMarkSelectMode",
        "TrailBlazerToggleTrailMarkList",
        "TrailBlazerOpenTrailMarkList",
        "TrailBlazerCloseTrailMarkList",
        "TrailBlazerSwitchTrailMarkStack",
        "TrailBlazerAddTrailMarkStack",
        "TrailBlazerDeleteTrailMarkStacks",
        "TrailBlazerDeleteAllTrailMarkStacks",
        "TrailBlazerSwitchNextTrailMarkStack",
        "TrailBlazerSwitchPreviousTrailMarkStack",
        "TrailBlazerSetTrailMarkStackSortMode",
        "TrailBlazerSaveSession",
        "TrailBlazerLoadSession",
        "TrailBlazerDeleteSession",
    },
    keys = {
        { "<A-l>" },
        { "<A-b>" },
        { "<A-J>" },
        { "<A-K>" },
        { "<A-n>" },
        { "<A-m>" },
        { "<A-L>" },
        { "<A-p>" },
        { "<A-P>" },
        { "<A-t>" },
        { "<A-.>" },
        { "<A-,>" },
        { "<A-s>" },
        {
            "[t",
            function()
                require("trailblazer").move_to_nearest("%", "up")
            end,
            desc = "Move Up",
        },
        {
            "[T",
            function()
                require("trailblazer").move_to_nearest("%", "fpath_up")
            end,
            desc = "Move up or next file",
        },
        {
            "]t",
            function()
                require("trailblazer").move_to_nearest("%", "down")
            end,
            desc = "Move Down",
        },
        {
            "]T",
            function()
                require("trailblazer").move_to_nearest("%", "fpath_down")
            end,
            desc = "Move down or next file",
        },
        {
            "<leader>tt",
            function()
                require("trailblazer").new_trail_mark()
            end,
            desc = "New Mark",
        },
        {
            "<C-p>",
            function()
                require("trailblazer").track_back()
            end,
            desc = "Delete Mark",
        },
        {
            "<leader>tc",
            function()
                require("trailblazer").paste_at_last_trail_mark()
            end,
            desc = "Copy to last mark",
        },
        {
            "<leader>tC",
            function()
                require("trailblazer").paste_at_all_trail_marks()
            end,
            desc = "Copy to all mark",
        },
        {
            "<leader>ta",
            function()
                require("trailblazer").toggle_trail_mark_list()
            end,
            desc = "List",
        },
        {
            "<leader>ts",
            function()
                require("trailblazer").save_trailblazer_state_to_file(nil, nil, true)
            end,
            desc = "Save",
        },
        {
            "<leader>tl",
            function()
                require("trailblazer").load_trailblazer_state_from_file(nil, true)
            end,
            desc = "Load",
        },
        {
            "<leader>tp",
            function()
                require("trailblazer").peek_move_previous_up()
            end,
            desc = "Previous",
        },
        {
            "<leader>tA",
            function()
                local stack_name =
                    vim.fn.input("Stack Name To Create: ", "", "customlist,v:lua.GET_AVAILABLE_TRAIL_MARK_STACKS")
                require("trailblazer").add_trail_mark_stack(stack_name)
            end,
            desc = "Add Stack",
        },
        {
            "<leader>tS",
            function()
                local stack_name =
                    vim.fn.input("Stack Name To Delete: ", "", "customlist,v:lua.GET_AVAILABLE_TRAIL_MARK_STACKS")
                require("trailblazer").switch_to_next_trail_mark_stack(stack_name)
            end,
            desc = "Switch Stack",
        },
    },
    config = config,
}
