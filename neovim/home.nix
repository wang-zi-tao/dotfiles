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
          onedark-vim
        ] ++ (if cfg.IDE then [
          #pkgs.vimPlugins.spacevim
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
        ] else
          [ ]));
      extraConfig = ''
        execute 'source' '${pkgs.spacevim}/SpaceVim/init.vim'
      '' + builtins.readFile ./init.vim;

    };
    home.file.".SpaceVim.d/init.toml".text =
      builtins.readFile ./Spacevim.d.init.toml;
    home.file."coc-settings" = {
      source = ./coc-settings.json;
      target = ".config/nvim/coc-settings.json";
    };
  };
}
