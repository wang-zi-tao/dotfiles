return {
    "tanvirtin/vgit.nvim",
    dir = gen.vgit_nvim,
    name = "vgit",
    dependencies = "plenary_nvim",
    event = 'VimEnter',
    cmd = "VGit",
    lazy = true,
    config = config,
    opts = {
        keymaps = {
            -- ["n [h"] = "hunk_up",
            -- ["n ]h"] = "hunk_down",
            -- ["n <leader>hs"] = "buffer_hunk_stage",
            -- ["n <leader>hr"] = "buffer_hunk_reset",
            -- ["n <leader>hp"] = "buffer_hunk_preview",
            -- ["n <leader>bb"] = "buffer_blame_preview",
            -- ["n <leader>bf"] = "buffer_diff_preview",
            -- ["n <leader>bh"] = "buffer_history_preview",
            -- ["n <leader>br"] = "buffer_reset",
            -- ["n <leader>bg"] = "buffer_gutter_blame_preview",
            -- ["n <leader>glu"] = "project_hunks_preview",
            -- ["n <leader>gls"] = "project_hunks_staged_preview",
            -- ["n <leader>gd"] = "project_diff_preview",
            -- ["n <leader>gq"] = "project_hunks_qf",
            -- ["n <leader>gx"] = "toggle_diff_preference",
        },
        settings = {
            signs = {
                definitions = {
                    GitSignsDelete = {
                        text = '_'
                    }
                }
            }
        },
    },
}
