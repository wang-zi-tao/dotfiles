{ lib, pkgs, config, ... }:
with lib;
let cfg = config.neovim;
in {
  options.neovim = {
    IDE = mkOption {
      type = types.bool;
      default = false;
    };
  };
  config = {
    programs.neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
      withNodeJs = true;
      withPython3 = true;
      plugins = with pkgs.vimPlugins;
        ([
          coc-spell-checker
          coc-highlight
          coc-git
          coc-fzf
          coc-yaml
          coc-json
          coc-explorer
          # onedark-vim
          fzf-vim
          direnv-vim
          ranger-vim
          vim-tmux-clipboard
          vim-tmux-focus-events
          vim-tmux-navigator
          surround
          vim-fugitive
        ] ++ (if cfg.IDE then [
          coc-tabnine
          coc-go
          coc-nvim
          coc-java
          coc-html
          coc-cmake
          coc-pyright
          coc-clangd
          coc-tsserver
          coc-snippets
          coc-rust-analyzer
          vim-cpp-enhanced-highlight
          vim-devicons
          vim-nerdtree-syntax-highlight
          rainbow
          vimspector
        ] else
          [ ]));
      extraConfig = ''
        execute 'source' '${pkgs.spacevim}/SpaceVim/init.vim'
      '' + (if cfg.IDE then
        let
          lombok = builtins.fetchurl {
            url =
              "https://projectlombok.org/downloads/lombok.jar";
            sha256 =
              "sha256:1c3lvz7315id7w0z2lw8ik4i9y15d0mv9wwfpvxmkrzvqbk7p56f";
          };
        in ''
          let $JAVA_TOOL_OPTIONS="-javaagent:${lombok} -Xbootclasspath/a:${lombok}"
        ''
      else
        "") + builtins.readFile ./init.vim;
    };
    home.packages = with pkgs; [
      neovim-remote
      python2
    ];
    home.file.".SpaceVim.d/task.toml".text =''
      [make]
        command = 'make'
    '';
    home.file.".SpaceVim.d/init.toml".text =
      (builtins.readFile ./Spacevim.d.init.toml) + (if cfg.IDE then ''
        [[layers]]
          name = "lang#toml"
        [[layers]]
          name = "lang#markdown"
        [[layers]]
          name = "test"
        [[layers]]
          name = "lang#java"
          java_formatter_jar = "${
            builtins.fetchurl {
              url =
                "https://github.com/google/google-java-format/releases/download/v1.11.0/google-java-format-1.11.0-all-deps.jar";
              sha256 =
                "sha256:1ixpg8ljg819fq94mxyypknmslva3rkifphbnq3ic71b7iip6lia";
            }
          }"
        [[layers]]
          name = "lang#python"
        [[layers]]
           name = "debug"
        [[layers]]
          name = "lang#go"
        [[layers]]
          name = "lang#lua"
        [[layers]]
          name = "lang#javascript"
        [[layers]]
          name = "lang#c"
          enable_clang_syntax_highlight = true
        [[layers]]
          name = "lang#rust"
        [[layers]]
          name = "lang#typescript"
        [[layers]]
          name = "lang#nix"
        [[layers]]
          name = "lang#haskell"
      '' else
        "");
    home.file."coc-settings" = {
      source = ./coc-settings.json;
      target = ".config/nvim/coc-settings.json";
    };
  };
}
