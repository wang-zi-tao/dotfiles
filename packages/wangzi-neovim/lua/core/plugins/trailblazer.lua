local function config()
    require("trailblazer").setup({
        -- lang = "en",
        auto_save_trailblazer_state_on_exit = true,
        auto_load_trailblazer_state_on_enter = false,
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
                require("core.utils").add_mark()
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
