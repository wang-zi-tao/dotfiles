local M = {}
M.null_ls = function()
  local null_ls = require("null-ls")
  local b = null_ls.builtins
  local sources = {
     -- Rust
     -- b.formatting.rustfmt,
     -- C++
     b.formatting.clang_format,
     -- java
     b.formatting.google_java_format,
     -- mark
     b.formatting.prettier.with { filetypes = { "html", "markdown", "css" } },
     -- js/ts
     b.formatting.deno_fmt,
     -- Lua
     b.formatting.stylua,
     b.diagnostics.luacheck.with { extra_args = { "--global vim" } },
     -- Shell
     b.formatting.shfmt,
     b.diagnostics.shellcheck.with { diagnostics_format = "#{m} [#{c}]" },
  }
  null_ls.setup {
    sources = sources,
    on_attach = function(client)
        if client.resolved_capabilities.document_formatting then
            vim.cmd "autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()"
        end
    end,
  }
end
M.which_key = function()
  require("which-key").setup {

  }
end
M.cmp_tabnine = function()
  require( "cmp_tabnine.config"):setup {
     max_lines = 1000,
     max_num_results = 20,
     sort = true,
     run_on_every_keystroke = true,
     snippet_placeholder = "..",
     ignored_file_types = { -- default is not to ignore
        -- uncomment to ignore in lua:
        -- lua = true
     },
  }
end
M.marks = function()
  require("marks").setup {}
end
M.auto_save = function()
  require ("autosave") .setup {
     enabled = true,
     execution_message = function()
        return "AutoSave: saved at " .. vim.fn.strftime "%H:%M:%S"
     end,
     events = {"InsertLeave", "TextChanged"},
     conditions = {
        exists = true,
        filename_is_not = {},
        filetype_is_not = {},
        modifiable = true,
     },
     write_all_buffers = false,
     on_off_commands = true,
     clean_command_line_interval = 1000,
     debounce_delay = 135,
  }
end
M.ts_rainbow = function()
  require("nvim-treesitter.configs").setup {
     rainbow = {
        -- Setting colors
        colors = {
           -- Colors here
        },
        -- Term colors
        termcolors = {
           -- Term colors here
        },
     },
  }
end
M.navigator = function()
  require('Navigator').setup()
  local map = vim.api.nvim_set_keymap
  local opts = { noremap = true, silent = true }
  map('n', "<A-h>", "<CMD>lua require('Navigator').left()<CR>", opts)
  map('n', "<A-k>", "<CMD>lua require('Navigator').up()<CR>", opts)
  map('n', "<A-l>", "<CMD>lua require('Navigator').right()<CR>", opts)
  map('n', "<A-j>", "<CMD>lua require('Navigator').down()<CR>", opts)
  map('n', "<A-p>", "<CMD>lua require('Navigator').previous()<CR>", opts)
end
M.scrollbar = function()
  require("scrollbar").setup()
  require("scrollbar.handlers.search").setup()
end
M.hlslens = function()
  require('hlslens').setup({
    override_lens = function(render, plist, nearest, idx, r_idx)
        local sfw = vim.v.searchforward == 1
        local indicator, text, chunks
        local abs_r_idx = math.abs(r_idx)
        if abs_r_idx > 1 then
            indicator = ('%d%s'):format(abs_r_idx, sfw ~= (r_idx > 1) and '▲' or '▼')
        elseif abs_r_idx == 1 then
            indicator = sfw ~= (r_idx == 1) and '▲' or '▼'
        else
            indicator = ''
        end

        local lnum, col = unpack(plist[idx])
        if nearest then
            local cnt = #plist
            if indicator ~= '' then
                text = ('(%s %d/%d)'):format(indicator, idx, cnt)
            else
                text = ('(%d/%d)'):format(idx, cnt)
            end
            chunks = {{' ', 'Ignore'}, {text, 'HlSearchLensNear'}}
        else
            text = ('(%s %d)'):format(indicator, idx)
            chunks = {{' ', 'Ignore'}, {text, 'HlSearchLens'}}
        end
        render.set_virt(0, lnum - 1, col - 1, chunks, nearest)
    end
  })
end
return M
