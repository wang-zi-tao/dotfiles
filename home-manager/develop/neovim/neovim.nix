{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.neovim;
  plugins = with pkgs.unstable.vimPlugins;{
    inherit 
      plenary-nvim
      impatient-nvim
      packer-nvim
      nvim-base16
      nvim-web-devicons
      feline-nvim
      bufferline-nvim
      indent-blankline-nvim
      nvim-colorizer-lua
      nvim-treesitter
      gitsigns-nvim
      nvim-lspconfig
      lsp-signature-nvim
      vim-matchup
      better-escape-nvim
      friendly-snippets
      nvim-cmp
      luasnip
      cmp-luasnip
      cmp-nvim-lua
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      nvim-autopairs
      dashboard-nvim
      comment-nvim
      nvim-tree-lua
      telescope-nvim

      which-key-nvim
      null-ls-nvim
      symbols-outline-nvim
      rust-tools-nvim
      cmp-tabnine
      cmp-spell
      markdown-preview-nvim
      marks-nvim
      vim-surround
      undotree
      nvim-ts-rainbow
      vim-repeat
      diffview-nvim
      vim-illuminate
      nvim-hlslens;
    auto-save = pkgs.fetchgit {
        url = "https://github.com/Pocco81/AutoSave.nvim/";
        rev = "3d342d6fcebeede15b6511b13a38a522c6f33bf8";
        sha256 = "sha256-1tAYnd4/hGgG2NG8n9hZi9zWM+v1OTh0YBlG8kEZeXI=";
      };
    filetype = pkgs.fetchgit {
        url = "https://github.com/nathom/filetype.nvim/";
        rev = "4d2c0d4488a05f9b0d18a7e2004c0182e350bb45";
        sha256 = "sha256-dzrJ8ddUnj7zRNLWlSoknXtSB0LT9VUuYnHSsBJpgGQ=";
      };
    navigator = pkgs.fetchgit {
        url = "https://github.com/numToStr/Navigator.nvim/";
        rev = "f7b689d72649e1d5132116c76ac2ad8b97c210d4";
        sha256 = "sha256-OLQuIepNbCB+xog184Ist7Pb6ogbUL4bclT7pPVhzp8=";
      };
  };
in
{
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
      plugins = with pkgs.vimPlugins;[
          pkgs.nvchad
      ] ++ builtins.attrValues {};
      extraConfig = ''
        source ${pkgs.nvchad}/init.lua
      '';
    };
    home.packages = with pkgs; [ neovim-remote python2 zig ];
    home.file.".config/nvim/lua/custom/nix-plugins.lua".text = with plugins; ''
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
        auto_save = "${auto-save}",
        undotreim = "${undotree}",
        ts_rainbow = "${nvim-ts-rainbow}",
        vim_repeat = "${vim-repeat}",
        diffview = "${diffview-nvim}",
        filetype = "${filetype}",
        navigator = "${navigator}",
        illuminate = "${vim-illuminate}",
        hlslens = "${nvim-hlslens}",
      }
    '';
    home.file.".config/nvim/lua" = {
      recursive = true;
      source = ./lua;
      onChange = ''
        echo nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerCompile'
      '';
    };
  };
}
