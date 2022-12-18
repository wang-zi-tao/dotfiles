local function map(mod, key, exec)
    vim.keymap.set(mod, key, exec)
end

local wk = require("which-key")
local function telescope()
    return require("telescope.builtin")
end

wk.register({
    q = { "<cmd>close<CR>", "close" },
    w = { "<cmd>wa<CR>", "save" },
    Q = { "<cmd>quitall<CR>", "quit" },
    ["'"] = {
        function()
            require("FTerm").toggle()
        end,
        "Terminal",
    },
    f = {
        name = "Telescope",
        k = {
            function()
                telescope().keymaps()
            end,
            "Keymaps",
        },
        w = {
            function()
                telescope().live_grep()
            end,
            "Grep",
        },
        W = {
            function()
                require("telescope").extensions.live_grep_args.live_grep_args()
            end,
            "Grep Args",
        },
        f = {
            function()
                telescope().find_files()
            end,
            "Files",
        },
        F = {
            function()
                require("telescope").extensions.file_browser.file_browser()
            end,
            "File Tree",
        },
        r = {
            function()
                telescope().registers()
            end,
            "Registers",
        },
        o = {
            function()
                telescope().lsp_workspace_symbols()
            end,
            "WorkspaceSymbols",
        },
        a = {
            function()
                vim.lsp.buf.code_action()
            end,
            "Actions",
        },
        i = {
            function()
                telescope().lsp_references()
            end,
            "LSP Reference",
        },
        b = {
            function()
                telescope().buffers()
            end,
            "Buffers",
        },
        s = {
            function()
                telescope().git_status()
            end,
            "GitStatus",
        },
        t = {
            function()
                telescope().tags()
            end,
            "Tags",
        },
        c = {
            function()
                telescope().git_commits()
            end,
            "GitCommits",
        },
        B = {
            function()
                telescope().git_branches()
            end,
            "GitBranches",
        },
        m = {
            function()
                telescope().marks()
            end,
            "Marks",
        },
        d = {
            function()
                telescope().lsp_document_symbols()
            end,
            "Lsp_document_symbols",
        },
        p = {
            function()
                require 'telescope'.extensions.project.project {}
            end,
            "Projects",
        },
    },
    g = {
        name = "Git",
        r = {
            function()
                telescope().lsp_references()
            end,
            "LspReferences",
        },
        d = {
            function()
                require("diffview").open()
            end,
            "Open Diff",
        },
        D = {
            function()
                require("diffview").close()
            end,
            "Close Diff",
        },
        h = {
            function()
                require("diffview").file_history()
            end,
            "Git Log",
        },
        a = { ":!git add .<CR>", "git add ." },
        c = { ":terminal git commit<CR>", "git commit" },
        p = { ":!git push<CR>", "git push" },
        P = { ":!git pull<CR>", "git pull" },
    },
    l = {
        name = "LSP",
        a = {
            function()
                require("lspsaga.codeaction").code_actions()
            end,
            "CodeActions",
        },
        r = {
            function()
                require("lspsaga.rename").rename()
            end,
            "Rename",
        },
        d = {
            function()
                require("lspsaga.provider").preview_definition()
            end,
            "PreviewDefinition",
        },
    },
    p = { name = "perf",
        l = {
            name = "perf load",
            f = { ":PerfLoadFlat<CR>", "perf load flat" },
            g = { ":PerfLoadCallGraph<CR>", "perf load call graph" },
            o = { ":PerfLoadFlameGraph<CR>", "perf load flame graph" },
        },
        e = { ":PerfPickEvent<CR>", "perf pick event" },
        a = { ":PerfAnnotate<CR>", "perf annotate" },
        f = { ":PerfAnnotateFunction<CR>", "perf annotate function" },
        A = { ":PerfAnnotateSelection<CR>", "perf annotate selection" },
        n = { ":PerfToggleAnnotations<CR>", "perf toggle annotate" },
        h = { ":PerfHottestLines<CR>", "perf hottest lines" },
        s = { ":PerfHottestSymbols<CR>", "perf hottest symbols" },
        c = { ":PerfHottestCallersFunction<CR>", "perf hottest callers function" },
        C = { ":PerfHottestCallersSelection<CR>", "perf hottest callers selection" },
    },
    c = {
        name = "CMake",
        m = { ":CMake<CR>", "CMake" },
        c = { ":CMake configure<CR>", "CMake configure" },
        C = { ":CMake clean<CR>", "CMake clean" },
        r = { ":CMake build_and_run<CR>", "CMake run" },
        d = { ":CMake build_and_debug<CR>", "CMake debug" },
        t = { ":CMake select_build_type<CR>", "CMake build type" },
        s = { ":CMake select_target<CR>", "CMake select target" },
        B = { ":CMake build_all<CR>", "CMake build all" },
        b = { ":CMake build<CR>", "CMake build" },
    },
    d = {
        name = "debug",
        b = {
            function()
                require('persistent-breakpoints.api').toggle_breakpoint()
            end,
            "BreakPoint",
        },
        d = {
            function()
                require('persistent-breakpoints.api').set_conditional_breakpoint()
            end,
            "BreakPoint",
        },
        B = {
            function()
                require('persistent-breakpoints.api').clear_all_breakpoints()
            end,
            "Clear BreakPoint",
        },
        p = {
            function()
                require("dap").pause()
            end,
            "Pause",
        },
        c = {
            function()
                require("dap").continue()
            end,
            "Continue",
        },
        C = {
            function()
                require("dap").close()
            end,
            "Close",
        },
        o = {
            function()
                require("dap").step_over()
            end,
            "Step Over",
        },
        i = {
            function()
                require("dap").step_into()
            end,
            "Step Into",
        },
        l = {
            function()
                require("dap").run_to_cursor()
            end,
            "Run to Corsor",
        },
        r = {
            function()
                require("dap").run()
            end,
            "Run",
        },
        R = {
            function()
                require("dap").repl.open()
            end,
            "Repl",
        },
        u = {
            name = "UI",
            a = {
                function()
                    require("telescope").extensions.dap.commands({})
                end,
                "Commands",
            },
            o = {
                function()
                    require("dapui").open()
                end,
                "Open",
            },
            c = {
                function()
                    require("dapui").close()
                end,
                "Close",
            },
            C = {
                function()
                    require("telescope").extensions.dap.configurations({})
                end,
                "Configurations",
            },
        },
    },
    h = { name = "hunk" },
    b = {
        name = "buffer",
        d = {
            "<cmd>bd<CR>",
            "Delete Buffer",
        },
    },
    j = { function()
        require 'hop'.hint_char1()
    end, "hop char1" },
    k = { function()
        require 'hop'.hint_char2()
    end, "hop char1" },
    t = {
        name = "Hop",
        a = { function()
            require 'hop'.hint_anywhere()
        end, "any" },
        w = { function()
            require 'hop'.hint_words()
        end, "words" },
        c = { function()
            require 'hop'.hint_char1()
        end, "char1" },
        h = { function()
            require 'hop'.hint_char1()
        end, "char1" },
        e = { function()
            require 'hop'.hint_char1({ direction = require 'hop.hint'.HintDirection.AFTER_CURSOR })
        end, "back" },
        b = { function()
            require 'hop'.hint_char1({ direction = require 'hop.hint'.HintDirection.BEFORE_CURSOR })
        end, "forward" },
    }
}, { prefix = "<leader>" })
wk.register({
    g = {
        d = {
            function()
                require("telescope.builtin").lsp_definitions()
            end,
            "Define",
        },
        i = {
            function()
                require("telescope.builtin").lsp_implementations()
            end,
            "Implementation",
        },
        D = {
            function()
                require("telescope.builtin").lsp_type_definitions()
            end,
            "TypeDefinition",
        },
    },
})
map("n", "\\'", function()
    require("FTerm").toggle()
end)
map("n", "\\9", function()
    require("dap").toggle_breakpoint()
end)
map("n", "<F9>", function()
    require("dap").toggle_breakpoint()
end)
map("n", "\\%", function()
    require("dap").close()
end)
map("n", "<S-F5>", function()
    require("dap").close()
end)
map("n", "\\-", function()
    require("dap").step_over()
end)
map("n", "<F11>", function()
    require("dap").step_over()
end)
map("n", "\\0", function()
    require("dap").step_into()
end)
map("n", "<F10>", function()
    require("dap").step_into()
end)
map("n", "\\5", function()
    require("dap").run()
end)
map("n", "<F5>", function()
    require("dap").run()
end)

map("n", "<leader>hs", "<cmd>Gitsigns stage_hunk<CR>")
map("v", "<leader>hs", "<cmd>Gitsigns stage_hunk<CR>")
map("n", "<leader>hr", "<cmd>Gitsigns reset_hunk<CR>")
map("v", "<leader>hr", "<cmd>Gitsigns reset_hunk<CR>")
map("n", "<leader>hp", "<cmd>Gitsigns preview_hunk<CR>")
map("n", "<leader>hu", "<cmd>Gitsigns undo_stage_hunk<CR>")
map("n", "<leader>bs", "<cmd>Gitsigns stage_buffer<CR>")
map("n", "<leader>br", "<cmd>Gitsigns reset_buffer<CR>")
map("n", "<leader>hb", "<cmd>Gitsigns reset_buffer<CR>")
map("n", "<leader>wgb", "<cmd>Gitsigns toggle_current_line_blame<CR>")
map("n", "<leader>hd", "<cmd>Gitsigns diffthis<CR>")
map("n", "<leader>hD", "<cmd>Gitsigns diffthis<CR>")
map("n", "<leader>wgd", "<cmd>Gitsigns toggle_deleted<CR>")
map("o", "ih", "<cmd><C-U>Gitsigns select_hunk<CR>")
map("x", "ih", "<cmd><C-U>Gitsigns select_hunk<CR>")

map("n", "<leader>e", function()
    require("nvim-tree").toggle()
end)
map("n", "<leader>we", function()
    require("nvim-tree").focus()
end)
map("n", "<leader>o", function()
    require("symbols-outline").toggle_outline()
end)
map("n", "<leader>r", function()
    require("trouble").toggle()
end)
map("n", "<leader>u", "<cmd>UndotreeToggle<CR>")
map("n", "<leader><Tab>", "<cmd>b#<CR>")

map("n", "<leader>ws", "<cmd>sp<CR>")
map("n", "<leader>wv", "<cmd>vs<CR>")

map("n", "[d", function()
    vim.diagnostic.goto_prev()
end)
map("n", "]d", function()
    vim.diagnostic.goto_next()
end)
map("n", "K", function()
    vim.lsp.buf.hover()
end)
map("n", "ge", function()
    vim.diagnostic.open_float()
end)

map("n", "\\q", "<cmd>close<CR>")
map("n", "\\y", '"+y')
map("n", "\\p", '"+p')
map("n", "\\P", '"+P')
map("v", "\\y", '"+y')
map("v", "\\p", '"+p')
map("v", "\\P", '"+P')

map("n", "<C-up>", "<cmd>res +1<CR>")
map("n", "<C-down>", "<cmd>res -1<CR>")
map("n", "<C-left>", "<cmd>vertical resize-1<CR>")
map("n", "<C-right>", "<cmd>vertical resize+1<CR>")

map("i", "<C-h>", "<Left>")
map("i", "<C-e>", "<End>")
map("i", "<C-l>", "<Right>")
map("i", "<C-k>", "<Up>")
map("i", "<C-j>", "<Down>")
map("i", "<C-a>", "<ESC>^i")

map("t", "<leader><ESC>", "<C-\\><C-n>")
map({ "n", "t" }, "<C-\\>", function() require("FTerm").toggle() end)
map("n", "Q", "<Nop>")

map("n", "<leader>by", "<cmd>%y+ <CR>") -- copy whole file content
map("n", "<C-t>", "<cmd>enew <CR>") -- new buffer

map("n", "<Tab>", function()
    require("bufferline").cycle(1)
end)
map("n", "<S-Tab>", function()
    require("bufferline").cycle(-1)
end)
map({ "n", "t" }, "<C-h>", function() require('Navigator').left() end)
map({ "n", "t" }, "<C-k>", function() require('Navigator').up() end)
map({ "n", "t" }, "<C-l>", function() require('Navigator').right() end)
map({ "n", "t" }, "<C-j>", function() require('Navigator').down() end)
map("n", "<A-p>", function() require('Navigator').previous() end)
