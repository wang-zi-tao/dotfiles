{
  stdenvNoCC,
  lib,
  pkgs,
  stdenv,
  makeWrapper,
  neovim-remote,
  zlib,
  gcc,
  unzip,
  fetchurl,
  fetchgit,
  buildNpmPackage,
  enable-all ? true,
  enable-debuger ? enable-all && (pkgs.system == "x86_64-linux"),
  enable-markdown-preview ? false,
  enable-tabnine ? enable-all && (pkgs.system == "x86_64-linux"),
}:
with pkgs.unstable;
with pkgs.unstable.vimPlugins;
let
  pkg = pkgs.unstable.neovim-unwrapped;
  vscode-cpptools = stdenvNoCC.mkDerivation {
    name = "vscode-cpptools";
    version = "v1.29.2";
    src = fetchurl {
      url = "https://github.com/microsoft/vscode-cpptools/releases/download/v1.29.2/cpptools-linux-x64.vsix";
      sha256 = "05hrwryawx5qfz9135711cm18nwj67gygv78p47h4srdfnkh7jcp";
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
  mcp-hub = buildNpmPackage {
    pname = "mcp-hub";
    version = "3.2.0";
    src = fetchgit {
      url = "https://github.com/ravitemer/mcp-hub/";
      rev = "f1abd40eff97cc33e2cdeb902bebef325928a959";
      sha256 = "sha256-jZRyWXLwWajc1UQ/O/Er4rANXVPT2Mg/OXfOZIN8mQQ=";
    };
    inherit nodejs;

    nativeBuildInputs = [ nodejs ];
    npmDepsHash = "sha256-HngZpJdLApzHNQ36GaFlNG5/A+SCFjdPNShu3nf78NA=";
  };
  vars = {
    packer = packer-nvim;
    plenary_nvim = plenary-nvim;
    impatient_nvim = impatient-nvim;
    promise_async = promise-async;

    # utils
    profile_nvim = fetchgit {
      url = "https://github.com/stevearc/profile.nvim";
      rev = "0ee32b7aba31d84b0ca76aaff2ffcb11f8f5449f";
      sha256 = "sha256-usyy1kST8hq/3j0sp7Tpf/1mld6RtcVABPo/ygeqzbU=";
    };
    nix_develop_nvim = nix-develop-nvim;
    bigfile = bigfile-nvim;
    tiny_inline_diagnostic = tiny-inline-diagnostic-nvim;
    logger_nvim = fetchgit {
      url = "https://github.com/rmagatti/logger.nvim";
      rev = "63dd10c9b9a159fd6cfe08435d9606384ff103c5";
      sha256 = "sha256-4xQFk7+3NWEx1XUZApy4Ldi2xdsna+HdkOmq9vWP3B0=";
    };
    template = fetchgit {
      url = "https://github.com/nvimdev/template.nvim";
      rev = "59955db23613985e031d340756d5c01aebd583a3";
      sha256 = "sha256-SsTqdOve0uAP9fApBSVIUj0JIOjneQD02CXbA0dRCWo=";
    };

    # theme
    onedark_nvim = onedark-nvim;
    nvim_base16 = base16-nvim;
    nvim_web_devicons = nvim-web-devicons;
    tokyonight_nvim = tokyonight-nvim;

    # buffer
    indent_blankline_nvim = indent-blankline-nvim;
    nvim_colorizer_lua = fetchgit {
      url = "https://github.com/catgoose/nvim-colorizer.lua";
      rev = "517df88cf2afb36652830df2c655df2da416a0ae";
      sha256 = "sha256-yOPUgqHe0WT437aX8kp0P/reNgjZUNNRG7hvDgaeXT0=";
    };
    baleia_nvim = baleia-nvim;
    hbac = fetchgit {
      url = "https://github.com/axkirillov/hbac.nvim";
      rev = "2c85485ea28e5e3754650829e0bca612960e1b73";
      sha256 = "sha256-A+C9N7xorS7DV0w8N5TjyD7OvWdxUQ4PJaKW3kwkQS0=";
    };

    # treesitter
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
    nvim_treesitter_context = nvim-treesitter-context;

    # git
    gitsigns_nvim = gitsigns-nvim;
    vgit_nvim = fetchgit {
      url = "https://github.com/tanvirtin/vgit.nvim";
      rev = "47474d5edd46b55100daa9d4ce502c7d3d825bde";
      sha256 = "sha256-BmyKQL1Md976aUgPNVTmJ37nM5ofUFVFvlEi7QXvpWY=";
    };
    neogit = neogit;

    # AI
    copilot_vim = copilot-vim;
    codecompanion = codecompanion-nvim;
    vectorcode = fetchgit {
      url = "https://github.com/Davidyz/VectorCode";
      rev = "810ad4188be9389578ff75b08dfd18c152141d82";
      sha256 = "sha256-CznHsUYwqTWanRU548bAaFLaBm7YMwb5dwc9umPt/sk=";
    };
    mcphub = fetchgit {
      url = "https://github.com/ravitemer/mcphub.nvim";
      rev = "9f3e1b80aab5cc24668117c2bd63ff701df774fb";
      sha256 = "sha256-XOnlLgK67mOzAdm+Y+8oR6TY9q7EvUT7MQfk3fLKAqM=";
    };
    mcp_hub = mcp-hub;

    # LSP
    nvim_lspconfig = nvim-lspconfig;
    lsp_signature_nvim = lsp_signature-nvim;
    lspsaga = lspsaga-nvim;
    symbol_usage = fetchgit {
      url = "https://github.com/Wansmer/symbol-usage.nvim";
      rev = "0f9b3da014b7e41559b643e7461fcabb2a7dc83a";
      sha256 = "sha256-vNVrh8MV7KZoh2MtP+hAr6Uz20qMMMUcbua/W71lRn0=";
    };
    dr_lsp = fetchgit {
      url = "https://github.com/chrisgrieser/nvim-dr-lsp";
      rev = "162c915eaf61d40ca1b4eec079037f42e9fdefcf";
      sha256 = "sha256-c5cDqUHgT5QAEV4hXJMfiALZdzp46nrfcKiPmWWwHoQ=";
    };
    navic = nvim-navic;
    navbuddy = nvim-navbuddy;
    # symbols_outline = symbols-outline-nvim;
    rustaceanvim = rustaceanvim;
    none_ls = none-ls-nvim;
    clangd_extensions_nvim = clangd_extensions-nvim;
    haskell_tools_nvim = haskell-tools-nvim;
    jdtls = nvim-jdtls;
    iron_nvim = iron-nvim;
    lsp_format_nvim = lsp-format-nvim;
    refactoring_nvim = refactoring-nvim;
    garbage_day = fetchgit {
      url = "https://github.com/Zeioth/garbage-day.nvim";
      rev = "750ef08ae6031ee3683014c5349144340c08ead6";
      sha256 = "sha256-XBt1EMbd3QTwfxHZcTVO1Rd0rocATrv8HMEBPxbs60w=";
    };
    obsidian_nvim = obsidian-nvim;
    neotest = neotest;
    aerial_nvim = aerial-nvim;
    codeql = fetchgit {
      url = "https://github.com/pwntester/codeql.nvim";
      rev = "772fca7306a6302acdf5098387d755d30bd5876a";
      sha256 = "sha256-qMHFIakhBJ1vohOgFvINRKNRlnRsm/ciQrVrvTI120Y=";
    };
    mason_lspconfig = mason-lspconfig-nvim;
    lazydev = lazydev-nvim;
    cmake_tools = cmake-tools-nvim;
    overseer = overseer-nvim;

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
    cmp_zsh = cmp-zsh;
    cmp_git = cmp-git;
    cmp_ai = cmp-ai;

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

    libsqlite =
      if stdenv.isLinux then "${sqlite.out}/lib/libsqlite3.so" else "${sqlite.out}/lib/libsqlite3.dylib";
    sqlite = fetchgit {
      url = "https://github.com/kkharji/sqlite.lua";
      rev = "b487fcc8937b683942a1f7d9662fcf50ca5acd58";
      sha256 = "sha256-K92mDGvrqo6nU2uWJjhd231hpSDXicwsuokgW21k7zE=";
    };

    # project
    session_manager = fetchgit {
      url = "https://github.com/Shatur/neovim-session-manager";
      rev = "270e235b014f0c37bf362eb1e8913d66bba33a2e";
      sha256 = "sha256-0CB7/hqj3zEZPQUOQoaETcNzFJcQyKt3k7hIXoArhZg=";
    };
    sqlite_lua = sqlite-lua;

    which_key = which-key-nvim;
    auto_save = fetchgit {
      url = "https://github.com/Pocco81/auto-save.nvim";
      rev = "979b6c82f60cfa80f4cf437d77446d0ded0addf0";
      sha256 = "sha256-bWGil73YiCKZEaY7IuUOIU4Q7k7qCMjSeQ4I+cAVe44=";
    };

    # keymap
    flash_nvim = flash-nvim;

    # navigator
    navigator = Navigator-nvim;
    zellij_nav = zellij-nav-nvim;
    harpoon2 = harpoon2;
    hop_nvim = hop-nvim;
    arrow_nvim = arrow-nvim;
    trailblazer = fetchgit {
      url = "https://github.com/LeonHeidelbach/trailblazer.nvim";
      rev = "674bb6254a376a234d0d243366224122fc064eab";
      sha256 = "sha256-DWlacXJUzuHbyLLbO13zGV2dFPZrt+oZvFyg5gGlFGM=";
    };

    # buffer UI
    scrollbar = nvim-scrollbar;
    rainbow_delimiters = rainbow-delimiters-nvim;
    hlslens = nvim-hlslens;

    # window UI
    neo_tree = neo-tree-nvim;
    nvim_tree_lua = nvim-tree-lua;
    diffview = diffview-nvim;

    # UI
    snacks = snacks-nvim;
    noice_nvim = noice-nvim;
    nvim_window_picker = nvim-window-picker;
    nui_nvim = nui-nvim;
    undotree = undotree;
    edgy = edgy-nvim;
    focus = focus-nvim;
    illuminate = vim-illuminate;
    pretty_fold = pretty-fold-nvim;
    pretty_fold_preview = fold-preview-nvim;
    keymap_amend = fetchgit {
      url = "https://github.com/anuvyklack/keymap-amend.nvim";
      rev = "41964a7230b6a787d3121bf8d2d06c08dabe9449";
      sha256 = "sha256-Zqos5LwjDzVQDpxKpWJVeZjmQ2+tKtub0f4wm6LFPOs=";
    };
    ts_autotag = nvim-ts-autotag;
    # virtual_types_nvim = virtual-types-nvim;
    alpha_nvim = alpha-nvim;
    lualine_nvim = lualine-nvim;
    heirline = heirline-nvim;
    heirline_components = fetchgit {
      url = "https://github.com/Zeioth/heirline-components.nvim";
      rev = "935f29dabd86f2669e0b3c8dd283b2d3b1cfaee7";
      sha256 = "sha256-M2muEW4RFQxdaJjZaXMXosy0M7Zj4MlbITRpRWpinwo=";
    };
    bufferline_nvim = bufferline-nvim;
    notify_nvim = nvim-notify;
    FixCursorHold_nvim = pkgs.vimPlugins.FixCursorHold-nvim;
    nvim_nio = nvim-nio;
    nvim_ufo = nvim-ufo;
    fidget_nvim = fidget-nvim;
    goto_preview = fetchgit {
      url = "https://github.com/rmagatti/goto-preview";
      rev = "d1faf6ea992b5bcaaaf2c682e1aba3131a01143e";
      sha256 = "sha256-WPY0lRkbCEdmnRaOOOCGJEl9a6/GtQqBBdcCy7arAuc=";
    };

    markdown_preview = if enable-markdown-preview then "markdown-preview-nvim" else false;
    render_markdown = render-markdown-nvim;

    # debug
    mason_nvim = mason-nvim;
    dap = nvim-dap;
    dap_ui = nvim-dap-ui;
    dap_virtual_text = nvim-dap-virtual-text;
    persistent_breakpoints_nvim = fetchgit {
      url = "https://github.com/Weissle/persistent-breakpoints.nvim";
      rev = "4b199b1dcfd136cac8b0fa9c8dbbdeb81463f7a9";
      sha256 = "sha256-euwc9XD02g8W52Z8SzjSInLnatS3aGLY44Frvd+yDTc=";
    };
    vscode_lldb =
      if enable-debuger then pkgs.vscode-extensions.vadimcn.vscode-lldb else "false";
    OpenDebugAD7 = "${vscode-cpptools}/extension/debugAdapters/bin/OpenDebugAD7";
    vscode_js_debug = vscode-js-debug;
    nio = nvim-nio;
    one_small_step_for_vimkind = one-small-step-for-vimkind;
    dap_disasm = fetchgit {
      url = "https://github.com/Jorenar/nvim-dap-disasm";
      rev = "543939e2572c4291f1978737d687977385a9e669";
      sha256 = "sha256-U7oPbml4qy0ISCgkr8Di16u2jDiW52IW5g+P+cwAlmE=";
    };
    dap_exception_breakpoints = fetchgit {
      url = "https://github.com/lucaSartore/nvim-dap-exception-breakpoints";
      rev = "6bc8d52fb48d20c8cd61920a8c1a4b487eff3afd";
      sha256 = "sha256-TnFFRurZb5yP/1sWsf53TJyOJH0lvUyu7TyZyoesqNY=";
    };

    # terminal
    fterm = FTerm-nvim;
    toggleterm_nvim = toggleterm-nvim;
    mini = mini-nvim;
    firenvim = firenvim;
    dressing_nvim = dressing-nvim;
    trouble_nvim = trouble-nvim;
    xmake = fetchgit {
      url = "https://github.com/Mythos-404/xmake.nvim";
      rev = "594dfe0c73f133848c917c0c848f6bb3f3400345";
      sha256 = "sha256-4PmuXIjeBV+DFhTUXuOvKrH41C+DKrf5GPmPWaFEX7w=";
    };
    crates_nvim = crates-nvim;
    perfanno_nvim = fetchgit {
      url = "https://github.com/t-troebst/perfanno.nvim";
      rev = "8640d6655f17a79af8de3153af2ce90c03f65e86";
      sha256 = "sha256-AfmmeLeUwYY9c3ISwt6/EHwCE4uhzKCvVoFwze7VJ4E=";
    };
    distant = distant-nvim;
  };
in
stdenvNoCC.mkDerivation {
  pname = "wangzi-neovim";
  version = "1.0.0";

  src = ./.;

  nativeBuildInputs = [
    makeWrapper
    unzip
  ];
  buildInputs = [
    pkg
    neovim-remote
    gcc
    zlib
  ];

  VARS = lib.strings.concatStrings (
    lib.mapAttrsToList (
      name: value:
      let
        ty = builtins.typeOf value;
        valueString =
          if ty == "string" || ty == "set" then
            ''"'' + (builtins.toString value) + ''"''
          else if ty == "bool" then
            if value then "true" else "false"
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

    cat > $out/bin/wnvim << EOF
    #!${stdenv.shell}
    export LUA_PATH="\$LUA_PATH;$LUA_PATH"
    export LD_LIBRARY_PATH="\$LD_LIBRARY_PATH:${luarocks}:${sqlite.out}/lib"
    ${pkg}/bin/nvim -u $out/init.lua "\$@"
    EOF
    chmod +x $out/bin/wnvim

    ln -s ${pkgs.tree-sitter}/bin/tree-sitter $out/bin/tree-sitter
    cp $out/bin/wnvim $out/bin/wangzi-neovim
    cp $out/bin/wnvim $out/bin/nvim
    cp $out/bin/wnvim $out/bin/wangzi-neovim
    cp $out/bin/wnvim $out/bin/vim
    cp $out/bin/wnvim $out/bin/vi
    cp $out/bin/wnvim $out/bin/v
  '';
}
