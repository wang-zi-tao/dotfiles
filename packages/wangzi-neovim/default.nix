{
  stdenvNoCC,
  lib,
  fetchFromGitHub,
  pkgs,
  makeWrapper,
  vimPlugins,
  neovim-unwrapped,
  neovim-remote,
  autoPatchelfHook,
  zlib,
  lttng-ust_2_12,
  gcc,
  unzip,
  fetchurl,
  enable-all ? true,
  enable-debuger ? enable-all && (pkgs.system == "x86_64-linux"),
  enable-markdown-preview ? enable-all,
  enable-tabnine ? enable-all && (pkgs.system == "x86_64-linux"),
}:
with pkgs.master;
with pkgs.master.vimPlugins;
let
  pkg = pkgs.master.neovim-unwrapped;
  vscode-cpptools = stdenvNoCC.mkDerivation {
    name = "vscode-cpptools";
    version = "v1.20.5";
    src = fetchurl {
      url = "https://github.com/microsoft/vscode-cpptools/releases/download/v1.20.5/cpptools-linux.vsix";
      sha256 = "16dwik9yigc433gsbvqjnpa57wy72a7d6js7lgl9q0qnfnvw3d3a";
    };
    nativeBuildInputs = [ unzip ];
    unpackPhase = ''
      unzip $src -d .
    '';
    installPhase = ''
      mkdir $out
      cp -r ./* $out
      chmod +x "$out/extension/debugAdapters/bin/OpenDebugAD7"
    '';
  };
  vars = {
    packer = packer-nvim;
    plenary_nvim = pkgs.vimPlugins.plenary-nvim;
    impatient_nvim = impatient-nvim;
    promise_async = promise-async;

    # utils
    profile_nvim = pkgs.fetchgit {
      url = "https://github.com/stevearc/profile.nvim";
      rev = "0ee32b7aba31d84b0ca76aaff2ffcb11f8f5449f";
      sha256 = "sha256-usyy1kST8hq/3j0sp7Tpf/1mld6RtcVABPo/ygeqzbU=";
    };
    nix_develop_nvim = nix-develop-nvim;
    bigfile = bigfile-nvim;
    tiny_inline_diagnostic = tiny-inline-diagnostic-nvim;

    # theme
    onedark_nvim = onedark-nvim;
    nvim_base16 = base16-nvim;
    nvim_web_devicons = nvim-web-devicons;
    tokyonight_nvim = tokyonight-nvim;

    # buffer
    indent_blankline_nvim = indent-blankline-nvim;
    nvim_colorizer_lua = nvim-colorizer-lua;
    baleia_nvim = baleia-nvim;
    nvim_treesitter =
      if enable-all then
        nvim-treesitter.withAllGrammars
      else
        nvim-treesitter.withPlugins (
          p: with p; [
            nix
            cpp
            c
            java
            kotlin
            rust
            typescript
            python
            javascript
          ]
        );
    nvim_treesitter_textobjects = nvim-treesitter-textobjects;

    # git
    gitsigns_nvim = gitsigns-nvim;
    vgit_nvim = pkgs.fetchgit {
      url = "https://github.com/tanvirtin/vgit.nvim";
      rev = "ee9081c304b44509b2f4267f1f7addc303f9fb9b";
      sha256 = "sha256-ucjVbpIwJ9eNBEQKVxN0a09GB773M2lTMdzYcLQ88QM=";
    };
    neogit = neogit;

    # AI
    gen_nvim = pkgs.fetchgit {
      url = "https://github.com/David-Kunz/gen.nvim";
      rev = "b1230ce2993b2be38a1e22606750d05a94307380";
      sha256 = "sha256-z03a2au40RIcpDUTRSWlWAbo1E+MgEgVaobFWV8hIaI=";
    };
    copilot_vim = copilot-vim;
    codecompanion = pkgs.fetchgit {
      url = "https://github.com/olimorris/codecompanion.nvim";
      rev = "331d95cd86627e2ed998a2ca012d6fed01080486";
      sha256 = "sha256-njyu7a8SH2Yd+gOZgjY7la6DeYZBru5vLGXd8wN5uQY=";
    };

    # LSP
    nvim_lspconfig = nvim-lspconfig;
    lsp_signature_nvim = lsp_signature-nvim;
    lspsaga = lspsaga-nvim;
    symbol_usage = pkgs.fetchgit {
      url = "https://github.com/Wansmer/symbol-usage.nvim";
      rev = "d9578780b760ca22cbe7dd618cc880b155b0e84a";
      sha256 = "sha256-Vy4IPYuMkjxK1Gf5JCnSzrrrOp3vZRrGQtVOpAewm7s=";
    };
    dr_lsp = pkgs.fetchgit {
      url = "https://github.com/chrisgrieser/nvim-dr-lsp";
      rev = "ce2c674970785d37e7ddc8896ec9b3a02da69dd1";
      sha256 = "sha256-1/V1M6TL79uWGxfcFOozl0V7xw7MJWc7/hCNkL3odpE=";
    };
    navic = nvim-navic;
    navbuddy = nvim-navbuddy;
    symbols_outline = symbols-outline-nvim;
    rustaceanvim = rustaceanvim;
    rust_tools = rust-tools-nvim;
    none_ls = none-ls-nvim;
    clangd_extensions_nvim = clangd_extensions-nvim;
    haskell_tools_nvim = haskell-tools-nvim;
    jdtls = nvim-jdtls;
    iron_nvim = iron-nvim;
    lsp_format_nvim = lsp-format-nvim;
    refactoring_nvim = refactoring-nvim;
    garbage_day = pkgs.fetchgit {
      url = "https://github.com/Zeioth/garbage-day.nvim";
      rev = "4a1160bfffb2f499fb55a54333f29d160ab3c8a1";
      sha256 = "sha256-qvdjsq41P03IYs2/3Hio/L2acso5+q+rPJLTanzB95Q=";
    };
    obsidian_nvim = obsidian-nvim;
    neotest = neotest;
    aerial_nvim = aerial-nvim;
    codeql = pkgs.fetchgit {
      url = "https://github.com/pwntester/codeql.nvim";
      rev = "772fca7306a6302acdf5098387d755d30bd5876a";
      sha256 = "sha256-qMHFIakhBJ1vohOgFvINRKNRlnRsm/ciQrVrvTI120Y=";
    };
    mason_lspconfig = mason-lspconfig-nvim;

    vim_matchup = vim-matchup;
    better_escape_nvim = better-escape-nvim;

    # cmp
    friendly_snippets = friendly-snippets;
    luasnip = luasnip;

    nvim_cmp = nvim-cmp;
    cmp_luasnip = cmp_luasnip;
    cmp_nvim_lua = cmp-nvim-lua;
    cmp_nvim_lsp = cmp-nvim-lsp;
    cmp_buffer = cmp-buffer;
    cmp_path = cmp-path;
    cmp_tabnine = if enable-tabnine then cmp-tabnine else false;
    cmp_spell = cmp-spell;
    cmp_cmdline = cmp-cmdline;
    cmp_cmdline_history = cmp-cmdline-history;
    cmp_zsh = pkgs.fetchgit {
      url = "https://github.com/tamago324/cmp-zsh";
      rev = "1d8133e5637c73b3eb392682ae9661d521738268";
      sha256 = "sha256-49MghjnaVzrOlbawP+WM7nQvkGH2nWt/AFdiT4ijQgQ=";
    };
    cmp_git = cmp-git;
    nvim_cmp_lsp_rs = pkgs.fetchgit {
      url = "https://github.com/zjp-CN/nvim-cmp-lsp-rs";
      rev = "d9ebeca9ea07ba2fd57f997b2d6a8bc7da51abed";
      sha256 = "sha256-NYbNj7vdhmJaz/H2Fm7eWZxrM4GFAw4hpYR3y38ye10=";
    };

    nvim_autopairs = nvim-autopairs;
    dashboard_nvim = dashboard-nvim;
    comment_nvim = comment-nvim;

    # telescope
    telescope_nvim = telescope-nvim;
    telescope_ui_select = telescope-ui-select-nvim;
    telescope_dap_nvim = telescope-dap-nvim;
    telescope_project_nvim = telescope-project-nvim;
    telescope_live_grep_args_nvim = telescope-live-grep-args-nvim;
    telescope_fzf_native_nvim = telescope-fzf-native-nvim;
    telescope_frecency_nvim = telescope-frecency-nvim;
    telescope_file_browser_nvim = telescope-file-browser-nvim;
    telescope_sg = telescope-sg;
    telescope_smart_open = smart-open-nvim;

    libsqlite = "${sqlite.out}/lib/libsqlite3.so";
    sqlite = pkgs.fetchgit {
      url = "https://github.com/kkharji/sqlite.lua";
      rev = "53cac3fdb5f5e4e63e243232b6eccf3c764ae18a";
      sha256 = "sha256-F4xfIDZfhpQvsvS4Qwmp5qJQmDYWacIqRxdO06RZ42I=";
    };

    # project
    session_manager = pkgs.fetchgit {
      url = "https://github.com/Shatur/neovim-session-manager";
      rev = "6604857365b13bfbcaa7ef377d4e60d2acb0be02";
      sha256 = "sha256-2UJep54dq6EAa7CL7uE0oeHNgMfXFK9SmG6z4L167JE=";
    };
    sqlite_lua = sqlite-lua;

    which_key = which-key-nvim;
    markdown_preview = if enable-markdown-preview then "markdown-preview-nvim" else "false";
    auto_save = pkgs.fetchgit {
      url = "https://github.com/Pocco81/auto-save.nvim";
      rev = "979b6c82f60cfa80f4cf437d77446d0ded0addf0";
      sha256 = "sha256-bWGil73YiCKZEaY7IuUOIU4Q7k7qCMjSeQ4I+cAVe44=";
    };

    # keymap
    flash_nvim = flash-nvim;

    # navigator
    navigator = Navigator-nvim;
    harpoon2 = harpoon2;
    hop_nvim = hop-nvim;
    arrow_nvim = arrow-nvim;
    trailblazer = pkgs.fetchgit {
      url = "https://github.com/LeonHeidelbach/trailblazer.nvim";
      rev = "674bb6254a376a234d0d243366224122fc064eab";
      sha256 = "sha256-DWlacXJUzuHbyLLbO13zGV2dFPZrt+oZvFyg5gGlFGM=";
    };

    # buffer UI
    scrollbar = nvim-scrollbar;
    rainbow_delimiters = rainbow-delimiters-nvim;
    hlslens = nvim-hlslens;
    filetype = pkgs.fetchgit {
      url = "https://github.com/nathom/filetype.nvim/";
      rev = "b522628a45a17d58fc0073ffd64f9dc9530a8027";
      sha256 = "sha256-B+VvgQj8akiKe+MX/dV2/mdaaqF8s2INW3phdPJ5TFA=";
    };

    # window UI
    neo_tree = neo-tree-nvim;
    nvim_tree_lua = nvim-tree-lua;
    diffview = diffview-nvim;

    # UI
    noice_nvim = noice-nvim;
    nvim_window_picker = nvim-window-picker;
    nui_nvim = nui-nvim;
    undotree = undotree;
    edgy = pkgs.fetchgit {
      url = "https://github.com/folke/edgy.nvim";
      rev = "de79b7d92a5979cd71a9a1d8b6282515345e5055";
      sha256 = "sha256-OgvrHDU+PcRsf3NtIglYFYIvnd8pAT481+w2bllZtXw=";
    };
    focus = pkgs.fetchgit {
      url = "https://github.com/nvim-focus/focus.nvim";
      rev = "3d9df42aa4f9b572348418207b752f81adea09a5";
      sha256 = "sha256-MpGDxBJ0IMMAIxuzFkxIgKtAn56NvpjfTNMVhnBhhsE=";
    };
    illuminate = vim-illuminate;
    pretty_fold = pretty-fold-nvim;
    pretty_fold_preview = pkgs.fetchgit {
      url = "https://github.com/anuvyklack/fold-preview.nvim";
      rev = "33c24101dc1b2be29876ee3354de98bb8bd14cb0";
      sha256 = "sha256-kMjm+q9Jm+YRrdxZp7yGsQSB0BmJzBrowbJzl6LHkqs=";
    };
    keymap_amend = pkgs.fetchgit {
      url = "https://github.com/anuvyklack/keymap-amend.nvim";
      rev = "41964a7230b6a787d3121bf8d2d06c08dabe9449";
      sha256 = "sha256-Zqos5LwjDzVQDpxKpWJVeZjmQ2+tKtub0f4wm6LFPOs=";
    };
    ts_autotag = nvim-ts-autotag;
    virtual_types_nvim = virtual-types-nvim;
    alpha_nvim = alpha-nvim;
    lualine_nvim = lualine-nvim;
    heirline = heirline-nvim;
    heirline_components = pkgs.fetchgit {
      url = "https://github.com/Zeioth/heirline-components.nvim";
      rev = "1ca5d501134d5c85b9e43dd0504447a1dceb90fa";
      sha256 = "sha256-cCT6n/dASx5+/mKOVCEoEn4+w6h12dMqJMjaiat0vq4=";
    };
    bufferline_nvim = bufferline-nvim;
    notify_nvim = nvim-notify;
    FixCursorHold_nvim = FixCursorHold-nvim;
    nvim_nio = nvim-nio;
    nvim_ufo = nvim-ufo;
    fidget_nvim = fidget-nvim;

    # debug
    mason_nvim = mason-nvim;
    dap = nvim-dap;
    dap_ui = nvim-dap-ui;
    dap_virtual_text = nvim-dap-virtual-text;
    persistent_breakpoints_nvim = pkgs.fetchgit {
      url = "https://github.com/Weissle/persistent-breakpoints.nvim";
      rev = "0dee5374c68950a89d2739f8d59be2350a8503c7";
      sha256 = "sha256-uHvxAfz2hYDRu6ST/PsqtJ/LQitdLNhnwg5aoFJqW88=";
    };
    vscode_lldb =
      if enable-debuger then "pkgs.unstable.vscode-extensions.vadimcn.vscode-lldb" else "false";
    OpenDebugAD7 = "${vscode-cpptools}/extension/debugAdapters/bin/OpenDebugAD7";
    nio = nvim-nio;
    one_small_step_for_vimkind = pkgs.fetchgit {
      url = "https://github.com/jbyuki/one-small-step-for-vimkind";
      rev = "5d2edc8937978585881d97a8fec4c2903fa4d72c";
      sha256 = "sha256-TF8My2/5fFuH/lYkzgpSngjpy+jl+/nN7v5/8GeqyMI=";
    };

    # terminal
    fterm = FTerm-nvim;
    toggleterm_nvim = toggleterm-nvim;
    mini = mini-nvim;
    firenvim = pkgs.fetchgit {
      url = "https://github.com/glacambre/firenvim";
      rev = "56a49d79904921a8b4405786e12b4e12fbbf171b";
      sha256 = "sha256-aFRrOJr34newCyJ5glqd15Xz0vxRGR6XIRFz1Zy39XI=";
    };
    dressing_nvim = dressing-nvim;
    trouble_nvim = trouble-nvim;
    xmake = pkgs.fetchgit {
      url = "https://github.com/Mythos-404/xmake.nvim";
      rev = "778ba49655645aee2a46a8c7f2ac15f4fe77b743";
      sha256 = "sha256-CZX5TGvr4vOpOFa2Id3ec3MpNnfNmXrmzaDCi0zHLXc=";
    };
    cmake = pkgs.fetchgit {
      url = "https://github.com/Shatur/neovim-cmake";
      rev = "92f009b029d95ecaf95c260bec66d06733cda37b";
      sha256 = "sha256-MoBGf7osmJOkOKjo7om3L6X4mSvtsDSTHyRD9ntp+LU=";
    };
    crates_nvim = crates-nvim;
    perfanno_nvim = pkgs.fetchgit {
      url = "https://github.com/t-troebst/perfanno.nvim";
      rev = "b138718bf4289b429dc81cadaf80ace8221c647b";
      sha256 = "sha256-7xEyXOGVG8AVRpO4QopfyDfMyKExUDuuf7SCoSd+SiU=";
    };
    distant = pkgs.fetchgit {
      url = "https://github.com/chipsenkbeil/distant.nvim";
      rev = "887fc16bdae59bd1865e0776b427ca521987f7fe";
      sha256 = "sha256-hHRHH4ycQkI1FQ6GhkbnXIxXnNAer4WxU5y1D7qZP0g=";
    };
  };
in
stdenvNoCC.mkDerivation {
  pname = "wangzi-neovim";
  version = "1.0.0";

  src = ./.;

  nativeBuildInputs = [
    autoPatchelfHook
    makeWrapper
    unzip
  ];
  buildInputs = [
    pkg
    neovim-remote
    gcc
    zlib
    lttng-ust_2_12
  ];

  VARS = lib.strings.concatStrings (
    lib.mapAttrsToList (
      name: value:
      let
        ty = builtins.typeOf value;
        valueString =
          if ty == "string" || ty == "set" then
            ''"'' + (builtins.toString value) + ''"''
          else
            builtins.toString value;
      in
      ''
        ${name} = ${valueString},
      ''
    ) vars
  );

  installPhase = ''
    mkdir -p $out/
    mkdir -p $out/bin

    rm default.nix
    cp * -r $out/
    mkdir -p $out/bin
    rm $out/lua/core/gen.lua

    cat << EOF >> $out/lua/core/gen.lua
    local M = {
        core = "$out",
        $VARS
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
    HOME=. ${pkg}/bin/nvim -u $out/init.lua "+Lazy! install" --headless +qa
    makeWrapper ${pkg}/bin/nvim $out/bin/wnvim --add-flags '-u' --add-flags "$out/init.lua" \
        --set LUA_PATH "$LUA_PATH" \
        --prefix PATH : ${luarocks}:${sqlite.out}/lib

    ln -s ${pkgs.tree-sitter}/bin/tree-sitter $out/bin/tree-sitter
    cp $out/bin/wnvim $out/bin/wangzi-neovim
    cp $out/bin/wnvim $out/bin/nvim
    cp $out/bin/wnvim $out/bin/wangzi-neovim
    cp $out/bin/wnvim $out/bin/vim
    cp $out/bin/wnvim $out/bin/vi
    cp $out/bin/wnvim $out/bin/v
  '';
}
