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
        rev = "78e48d7ec6b7005e0d3d5363ccfd9870d5a02b35";
        sha256 = "sha256-fSPCgqn6Gd6RUg1jvHH5+0PmuSE5OHP/YO8VcxxpWiw=";
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
      comment_nvim = "${comment-nvim}",
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
        rev = "4d2c0d4488a05f9b0d18a7e2004c0182e350bb45";
        sha256 = "sha256-dzrJ8ddUnj7zRNLWlSoknXtSB0LT9VUuYnHSsBJpgGQ=";
      }}",
      navigator = "${pkgs.fetchgit {
        url = "https://github.com/numToStr/Navigator.nvim/";
        rev = "f7b689d72649e1d5132116c76ac2ad8b97c210d4";
        sha256 = "sha256-OLQuIepNbCB+xog184Ist7Pb6ogbUL4bclT7pPVhzp8=";
      }}",
      illuminate = "${vim-illuminate}",
      hlslens = "${nvim-hlslens}",
      pretty_fold = "${pkgs.fetchgit {
        url = "https://github.com/anuvyklack/pretty-fold.nvim/";
        rev = "6f921739fc16504acf75f90836146d36b2c0b275";
        sha256 = "sha256-WrZ6G5/jicBET619yERiFljvjeEVCBsywPVrQUVhS/g=";
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
        rev = "16bc2ff389fa4e6c51d5bdaee39fa308109bf3d7";
        sha256 = "sha256-0KAAV8RIN832OZpOUsVhA41H4aVP+ZEm23xPjmKVkXU=";
      }}",
      firenvim = "${pkgs.fetchgit {
        url = "https://github.com/glacambre/firenvim";
        rev = "668b350ce88cc9a2257644c67945c9abbdd36cb5";
        sha256 = "sha256-e+ZniVYOJEuCTwjNkta9K0jrVddxOHa/vqWQuMGO7lk=";
      }}",
      dressing_nvim = "${dressing-nvim}",
      telescope_ui_select = "${telescope-ui-select-nvim}",
      telescope_dap_nvim = "${telescope-dap-nvim}",
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
    require("impatient")
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
