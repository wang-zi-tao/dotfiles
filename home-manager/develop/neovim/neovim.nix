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
      package = pkgs.unstable.neovim-unwrapped;
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;
      withNodeJs = true;
      withPython3 = true;
      plugins = with pkgs.vimPlugins; [ packer-nvim pkgs.nvchad ];
      extraConfig = ''
        source ${pkgs.nvchad}/init.lua
      '';
    };
    home.packages = with pkgs; [ neovim-remote python2 zig ];
    home.file.".config/nvim/lua/custom/nix-plugins.lua".text = with pkgs.unstable.vimPlugins; ''
      return {
        packer = "${packer-nvim}",
        which_key = "${which-key-nvim}",
        null_ls = "${null-ls-nvim}",
        symbols_outline = "${symbols-outline-nvim}",
        rust_tools = "${rust-tools-nvim}",
        cmp_tabnine = "${cmp-tabnine}",
        cmp_spell = "${cmp-spell}",
        markdown_preview = "${markdown-preview-nvim}",
        marks = "${marks-nvim}",
        vim_surround = "${vim-surround}",
        auto_save = "${pkgs.fetchgit {
          url = "https://github.com/Pocco81/AutoSave.nvim/";
          rev = "3d342d6fcebeede15b6511b13a38a522c6f33bf8";
          sha256 = "sha256-1tAYnd4/hGgG2NG8n9hZi9zWM+v1OTh0YBlG8kEZeXI=";
        }}",
        undotreim = "${undotree}",
        ts_rainbow = "${nvim-ts-rainbow}",
        vim_repeat = "${vim-repeat}",
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
      }
    '';
    home.file.".config/nvim/lua" = {
      recursive = true;
      source = ./lua;
    };
  };
}
