local function config()
    require("trouble").setup({
    })
end

return {
    "folke/trouble.nvim",
    dir = gen.trouble_nvim,
    name = "trouble_nvim",
    dependencies = "nvim_web_devicons",
    lazy = true,
    config = config,
    keys = {
        {
            "<leader>wr",
            function()
                vim.cmd.Trouble(diagnostics)
            end,
            desc = "Error/Warning",
        },
    },
}
