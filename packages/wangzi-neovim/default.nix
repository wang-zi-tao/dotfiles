{ stdenvNoCC
, lib
, fetchFromGitHub
, pkgs
, makeWrapper
, vimPlugins
, neovim-unwrapped
, neovim-remote
, gcc
, enable-all ? true
, enable-debuger ? enable-all
, enable-markdown-preview ? enable-all
, enable-tabnine ? enable-all && (pkgs.system == "x86_64-linux" || pkgs.system == "x86_64-darwin")
}:

let
  #  neovim-unwrapped = pkgs.unstable.neovim-unwrapped;
  lazy-nvim = pkgs.fetchgit {
    url = "https://github.com/folke/lazy.nvim";
    rev = "b3eca0c3fb4ef5e547f70c571b8cd27688db83bb";
    sha256 = "sha256-ggaW3pIWvsUGuwuZRIVdRyQqlO7OMme1OIkBt6ycM70=";
  };
in
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

  installPhase = with pkgs.unstable; with pkgs.unstable.vimPlugins; ''
    rm default.nix
    mkdir -p $out/
    cp * -r $out/
    mkdir -p $out/bin
    rm $out/lua/core/gen.lua
    cat << EOF >> $out/lua/core/gen.lua
    local M = {
      core = "$out",
      packer = "${packer-nvim}",
      onedark_nvim = "${onedark-nvim}",
      alpha_nvim = "${pkgs.fetchgit {
        url = "https://github.com/goolord/alpha-nvim";
        rev = "1e12c492fdd09a812f960e83048af6d2d18b9b6b";
        sha256 = "sha256-JB8rP9Bs/e8ZR7pbPl07jwo/YD/2VFNKe6PzvA2L5dk=";
      }}",
      plenary_nvim = "${plenary-nvim}",
      impatient_nvim = "${impatient-nvim}",
      notify_nvim = "${nvim-notify}",
      nvim_base16 = "${nvim-base16}",
      nvim_web_devicons = "${nvim-web-devicons}",
      feline_nvim = "${feline-nvim}",
      lualine_nvim = "${lualine-nvim}",
      bufferline_nvim = "${bufferline-nvim}",
      indent_blankline_nvim = "${indent-blankline-nvim}",
      nvim_colorizer_lua = "${nvim-colorizer-lua}",
      baleia_nvim = "${pkgs.fetchgit {
        url = "https://github.com/m00qek/baleia.nvim";
        rev = "v1.2.0";
        sha256 = "sha256-LULcXLoGBUirOwgt5CobR5lrblb4kQUsvWtxEPhUIas=";
      }}",
      nvim_treesitter = "${if enable-all then nvim-treesitter.withAllGrammars else nvim-treesitter.withPlugins ( p: with p; [
        nix
      ])}",
      nvim_treesitter_textobjects = "${nvim-treesitter-textobjects}",
      gitsigns_nvim = "${gitsigns-nvim}",
      vgit_nvim = "${pkgs.fetchgit {
        url = "https://github.com/tanvirtin/vgit.nvim";
        rev = "ee9081c304b44509b2f4267f1f7addc303f9fb9b";
        sha256 = "sha256-ucjVbpIwJ9eNBEQKVxN0a09GB773M2lTMdzYcLQ88QM=";
      }}",
      nvim_lspconfig = "${nvim-lspconfig}",
      lsp_signature_nvim = "${lsp_signature-nvim}",
      vim_matchup = "${vim-matchup}",
      better_escape_nvim = "${better-escape-nvim}",

      friendly_snippets = "${friendly-snippets}",
      luasnip = "${luasnip}",

      nvim_cmp = "${nvim-cmp}",
      cmp_luasnip = "${cmp_luasnip}",
      cmp_nvim_lua = "${cmp-nvim-lua}",
      cmp_nvim_lsp = "${cmp-nvim-lsp}",
      cmp_buffer = "${cmp-buffer}",
      cmp_path = "${cmp-path}",
      cmp_tabnine = ${if enable-tabnine then ''"${cmp-tabnine}"'' else "false" },
      cmp_spell = "${cmp-spell}",
      cmp_cmdline = "${cmp-cmdline}",
      cmp_cmdline_history = "${cmp-cmdline-history}",
      cmp_zsh = "${pkgs.fetchgit {
        url = "https://github.com/tamago324/cmp-zsh";
        rev = "1d8133e5637c73b3eb392682ae9661d521738268";
        sha256 = "sha256-49MghjnaVzrOlbawP+WM7nQvkGH2nWt/AFdiT4ijQgQ=";
      }}",
      cmp_git = "${pkgs.fetchgit {
        url = "https://github.com/petertriho/cmp-git";
        rev = "fae6cdb407ad6c63a0b1928670bad1a67a55b887";
        sha256 = "sha256-/fHoZxtJFG9v1sw/rQU2fa0ybO7bIovvRvY6M/mU5sc=";
      }}",

      nvim_autopairs = "${nvim-autopairs}",
      dashboard_nvim = "${dashboard-nvim}",
      nvim_comment = "${nvim-comment}",
      nvim_tree_lua = "${pkgs.vimPlugins.nvim-tree-lua}",

      telescope_nvim = "${telescope-nvim}",
      telescope_ui_select = "${telescope-ui-select-nvim}",
      telescope_dap_nvim = "${telescope-dap-nvim}",
      telescope_project_nvim = "${telescope-project-nvim}",
      telescope_live_grep_args_nvim = "${telescope-live-grep-args-nvim}",
      telescope_fzf_native_nvim = "${telescope-fzf-native-nvim}",
      telescope_frecency_nvim = "${telescope-frecency-nvim}",
      telescope_file_browser_nvim = "${telescope-file-browser-nvim}",

      libsqlite = "${sqlite.out}/lib/libsqlite3.so",
      sqlite = "${pkgs.fetchgit {
        url = "https://github.com/kkharji/sqlite.lua";
        rev = "53cac3fdb5f5e4e63e243232b6eccf3c764ae18a";
        sha256 = "sha256-F4xfIDZfhpQvsvS4Qwmp5qJQmDYWacIqRxdO06RZ42I=";
      }}",

      which_key = "${which-key-nvim}",
      noice_nvim = "${noice-nvim}",
      nui_nvim = "${nui-nvim}",
      null_ls = "${null-ls-nvim}",
      symbols_outline = "${symbols-outline-nvim}",
      rust_tools = "${rust-tools-nvim}",
      clangd_extensions_nvim = "${clangd_extensions-nvim}",
      markdown_preview = ${if enable-markdown-preview then ''"${markdown-preview-nvim}"'' else "false" },
      marks = "${marks-nvim}",
      trailblazer = "${pkgs.fetchgit {
        url = "https://github.com/LeonHeidelbach/trailblazer.nvim";
        rev = "674bb6254a376a234d0d243366224122fc064eab";
        sha256 = "sha256-DWlacXJUzuHbyLLbO13zGV2dFPZrt+oZvFyg5gGlFGM=";
      }}",
      auto_save = "${pkgs.fetchgit {
        url = "https://github.com/Pocco81/auto-save.nvim";
        rev = "2c7a2943340ee2a36c6a61db812418fca1f57866";
        sha256 = "sha256-keK+IAnHTTA5uFkMivViMMAkYaBvouYqcR+wNPgN3n0=";
      }}",
      undotree = "${undotree}",
      ts_rainbow = "${nvim-ts-rainbow}",
      diffview = "${diffview-nvim}",
      filetype = "${pkgs.fetchgit {
        url = "https://github.com/nathom/filetype.nvim/";
        rev = "b522628a45a17d58fc0073ffd64f9dc9530a8027";
        sha256 = "sha256-B+VvgQj8akiKe+MX/dV2/mdaaqF8s2INW3phdPJ5TFA=";
      }}",
      navigator = "${Navigator-nvim}",
      illuminate = "${vim-illuminate}",
      hlslens = "${nvim-hlslens}",
      pretty_fold = "${pkgs.fetchgit {
        url = "https://github.com/anuvyklack/pretty-fold.nvim/";
        rev = "a7d8b424abe0eedf50116c460fbe6dfd5783b1d5";
        sha256 = "sha256-PQPZw0qXwMtpVE4uSxR3xUvkHE9iG4T+ZwgV6H9pUjo=";
      }}",
      pretty_fold_preview = "${pkgs.fetchgit {
        url = "https://github.com/anuvyklack/fold-preview.nvim";
        rev = "33c24101dc1b2be29876ee3354de98bb8bd14cb0";
        sha256 = "sha256-kMjm+q9Jm+YRrdxZp7yGsQSB0BmJzBrowbJzl6LHkqs=";
      }}",
      keymap_amend = "${pkgs.fetchgit {
        url = "https://github.com/anuvyklack/keymap-amend.nvim";
        rev = "41964a7230b6a787d3121bf8d2d06c08dabe9449";
        sha256 = "sha256-Zqos5LwjDzVQDpxKpWJVeZjmQ2+tKtub0f4wm6LFPOs=";
      }}",
      ts_autotag = "${nvim-ts-autotag}",
      lspsaga = "${lspsaga-nvim-original}",
      virtual_types_nvim = "${virtual-types-nvim}",

      mason_nvim = "${mason-nvim}",
      dap = "${nvim-dap}",
      dap_ui = "${nvim-dap-ui}",
      dap_virtual_text = "${nvim-dap-virtual-text}",
      persistent_breakpoints_nvim = "${pkgs.fetchgit {
        url = "https://github.com/Weissle/persistent-breakpoints.nvim";
        rev = "0dee5374c68950a89d2739f8d59be2350a8503c7";
        sha256 = "sha256-uHvxAfz2hYDRu6ST/PsqtJ/LQitdLNhnwg5aoFJqW88=";
      }}",
      vscode_lldb = ${if enable-debuger then ''"${pkgs.unstable.vscode-extensions.vadimcn.vscode-lldb}"'' else "false" },

      fterm = "${FTerm-nvim}",
      mini = "${mini-nvim}",
      session_manager = "${pkgs.fetchgit {
        url = "https://github.com/Shatur/neovim-session-manager";
        rev = "6604857365b13bfbcaa7ef377d4e60d2acb0be02";
        sha256 = "sha256-2UJep54dq6EAa7CL7uE0oeHNgMfXFK9SmG6z4L167JE=";
      }}",
      firenvim = "${pkgs.fetchgit {
        url = "https://github.com/glacambre/firenvim";
        rev = "56a49d79904921a8b4405786e12b4e12fbbf171b";
        sha256 = "sha256-aFRrOJr34newCyJ5glqd15Xz0vxRGR6XIRFz1Zy39XI=";
      }}",
      dressing_nvim = "${dressing-nvim}",
      trouble_nvim = "${trouble-nvim}",
      cmake = "${pkgs.fetchgit {
        url = "https://github.com/Shatur/neovim-cmake";
        rev = "92f009b029d95ecaf95c260bec66d06733cda37b";
        sha256 = "sha256-MoBGf7osmJOkOKjo7om3L6X4mSvtsDSTHyRD9ntp+LU=";
      }}",
     crates_nvim = "${crates-nvim}",
     perfanno_nvim = "${pkgs.fetchgit {
       url = "https://github.com/t-troebst/perfanno.nvim";
       rev = "3c7ee6e97f4995c064ebd3f26f08300898941904";
       sha256 = "sha256-wcjrRGc/wVs8qCto7plx/GRQIdVXK6QQDFz74xV8KFk=";
     }}",
     hop_nvim = "${hop-nvim}",
     distant = "${pkgs.fetchgit {
       url = "https://github.com/chipsenkbeil/distant.nvim";
       rev = "887fc16bdae59bd1865e0776b427ca521987f7fe";
       sha256 = "sha256-hHRHH4ycQkI1FQ6GhkbnXIxXnNAer4WxU5y1D7qZP0g=";
     }}",
    }
    return setmetatable({},{
    __index = function(o,field)
    local value = M[field]
    assert (value ~= nil, "not field with name " .. field .. " in core.gen")
    return value
    end
    })
    EOF
    export LUA_PATH="$out/lua/?.lua;$out/lua/?/init.lua;${lazy-nvim}/lua/?.lua;${lazy-nvim}/lua/?/init.lua;;"
    HOME=. ${pkgs.unstable.neovim-unwrapped}/bin/nvim -u $out/init.lua "+Lazy! install" --headless +qa
    makeWrapper ${pkgs.unstable.neovim-unwrapped}/bin/nvim $out/bin/wnvim --add-flags '-u' --add-flags "$out/init.lua" \
        --set LUA_PATH "$LUA_PATH"
    ln -s ${pkgs.tree-sitter}/bin/tree-sitter $out/bin/tree-sitter
    cp $out/bin/wnvim $out/bin/nvim
    cp $out/bin/wnvim $out/bin/vim
    cp $out/bin/wnvim $out/bin/vi
    cp $out/bin/wnvim $out/bin/v
  '';
}
