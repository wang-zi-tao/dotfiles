{ stdenvNoCC
, neovim-unwrapped
, lib
, fetchFromGitHub
, pkgs
, makeWrapper
, vimPlugins
, neovim-remote
, gcc
}:

stdenvNoCC.mkDerivation {
  pname = "wangzi-neovim";
  version = "1.0.0";

  src = ./.;

  buildInputs = [
    makeWrapper
    neovim-unwrapped
    neovim-remote
    gcc
  ];

  installPhase = with pkgs.vimPlugins; ''
    rm default.nix
    mkdir -p $out/
    cp * -r $out
    mkdir -p $out/bin
    cat << EOF > $out/lua/core/gen.lua
    local M = {
      core = "$out",
      packer = "${packer-nvim}",
      onedark_nvim = "${onedark-nvim}",
      alpha_nvim = "${pkgs.fetchgit {
        url = "https://github.com/goolord/alpha-nvim";
        rev = "4781fcfea5ddc1a92d41b32dc325132ed6fce7a8";
        sha256 = "sha256-GA+fIfVlHOllojGyErYGC0+zyYTl9rOxendqOgApJw4=";
      }}",
      plenary_nvim = "${plenary-nvim}",
      impatient_nvim = "${impatient-nvim}",
      nvim_base16 = "${nvim-base16}",
      nvim_web_devicons = "${nvim-web-devicons}",
      feline_nvim = "${feline-nvim}",
      bufferline_nvim = "${bufferline-nvim}",
      indent_blankline_nvim = "${indent-blankline-nvim}",
      nvim_colorizer_lua = "${nvim-colorizer-lua}",
      nvim_treesitter = "${nvim-treesitter}",
      gitsigns_nvim = "${gitsigns-nvim}",
      nvim_lspconfig = "${nvim-lspconfig}",
      lsp_signature_nvim = "${lsp_signature-nvim}",
      vim_matchup = "${vim-matchup}",
      better_escape_nvim = "${better-escape-nvim}",
      friendly_snippets = "${friendly-snippets}",
      nvim_cmp = "${nvim-cmp}",
      luasnip = "${luasnip}",
      cmp_luasnip = "${cmp_luasnip}",
      cmp_nvim_lua = "${cmp-nvim-lua}",
      cmp_nvim_lsp = "${cmp-nvim-lsp}",
      cmp_buffer = "${cmp-buffer}",
      cmp_path = "${cmp-path}",
      nvim_autopairs = "${nvim-autopairs}",
      dashboard_nvim = "${dashboard-nvim}",
      nvim_comment = "${nvim-comment}",
      nvim_tree_lua = "${nvim-tree-lua}",
      telescope_nvim = "${telescope-nvim}",
      which_key = "${which-key-nvim}",
      null_ls = "${null-ls-nvim}",
      symbols_outline = "${symbols-outline-nvim}",
      rust_tools = "${rust-tools-nvim}",
      cmp_tabnine = "${cmp-tabnine}",
      cmp_spell = "${cmp-spell}",
      markdown_preview = "${markdown-preview-nvim}",
      marks = "${marks-nvim}",
      auto_save = "${pkgs.fetchgit {
        url = "https://github.com/Pocco81/AutoSave.nvim/";
        rev = "3d342d6fcebeede15b6511b13a38a522c6f33bf8";
        sha256 = "sha256-1tAYnd4/hGgG2NG8n9hZi9zWM+v1OTh0YBlG8kEZeXI=";
      }}",
      undotree = "${undotree}",
      ts_rainbow = "${nvim-ts-rainbow}",
      diffview = "${diffview-nvim}",
      filetype = "${pkgs.fetchgit {
        url = "https://github.com/nathom/filetype.nvim/";
        rev = "5a9837d51da6c7c0767767e1ac224e074587a001";
        sha256 = "sha256-M5+ByFdSxjnvABGkJB4K0fADIVLl+hl6WOjqHTXzSRY=";
      }}",
      navigator = "${Navigator-nvim}",
      illuminate = "${vim-illuminate}",
      hlslens = "${nvim-hlslens}",
      pretty_fold = "${pkgs.fetchgit {
        url = "https://github.com/anuvyklack/pretty-fold.nvim/";
        rev = "e6385d62eec67fdc8a21700b42a701d0d6fb8b32";
        sha256 = "sha256-GhPSqkCHToZazJv8hDNGfhN8NZyRvplNv0ZKNEnC7FU=";
      }}",
      ts_autotag = "${nvim-ts-autotag}",
      lspsaga = "${lspsaga-nvim}",
      dap = "${nvim-dap}",
      dap_ui = "${nvim-dap-ui}",
      dap_virtual_text = "${nvim-dap-virtual-text}",
      vscode_lldb = "${pkgs.vscode-extensions.vadimcn.vscode-lldb}", 
      lldb_lib = "${pkgs.lldb.lib}",
      lldb = "${pkgs.lldb}",
      fterm = "${FTerm-nvim}",
      mini = "${mini-nvim}",
      session_manager = "${pkgs.fetchgit {
        url = "https://github.com/Shatur/neovim-session-manager";
        rev = "f0b0501ed4c311adc24fa00f27f17694faeb36a9";
        sha256 = "sha256-W0JFa3fDLByUFUjOmULSVw1UmtC3wqm6vN6exJfS/do=";
      }}",
      firenvim = "${pkgs.fetchgit {
        url = "https://github.com/glacambre/firenvim";
        rev = "c17f053b5d10eb0673875e30833eb463ceddd7eb";
        sha256 = "sha256-4Zvcexh6a4qd78jkBqOWjr3J948kHbnnWjHhTLMsNpw=";
      }}",
      dressing_nvim = "${dressing-nvim}",
      telescope_ui_select = "${telescope-ui-select-nvim}",
      telescope_dap_nvim = "${telescope-dap-nvim}",
      project = "${pkgs.fetchgit {
        url = "https://github.com/ahmedkhalf/project.nvim";
        rev = "612443b27f5feda45ea478bd6ddc8f95d4ec7b77";
        sha256 = "sha256-fGzcceS5veUgxVGXWHZb1PNCTBvYp0HFeesU0GY0Acc=";
      }}",
      compile_path = "$out/plugins.lua",
    }
    return setmetatable({},{
      __index = function(o,field)
        local value = M[field]
        assert (value ~= nil, "not field with name " .. field .. " in core.gen")
        return value
      end
    })
    EOF
    export LUA_PATH="$out/lua/?.lua;${pkgs.vimPlugins.packer-nvim}/lua/?.lua;;"
    HOME=. ${neovim-unwrapped}/bin/nvim -u $out/lua/core/pack.lua --headless +qa
    cat << EOF > $out/init.lua
    vim.opt.packpath = "$out/site"
    -- require("impatient")
    EOF
    cat $out/plugins.lua >> $out/init.lua
    rm $out/plugins.lua
    makeWrapper ${neovim-unwrapped}/bin/nvim $out/bin/wnvim --add-flags '-u' --add-flags "$out/init.lua" \
      --set LUA_PATH "$out/lua/?.lua;${pkgs.vimPlugins.packer-nvim}/lua/?.lua;;"
    cp $out/bin/wnvim $out/bin/nvim
    cp $out/bin/wnvim $out/bin/vim
    cp $out/bin/wnvim $out/bin/vi
    cp $out/bin/wnvim $out/bin/v
  '';
}
