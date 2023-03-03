{ pkgs, config, lib, ... }: {
  imports = [
    ./tmux/tmux.nix
    ./zsh/zsh.nix
    ./procs/procs.nix
    ./htop.nix
    ./ranger/ranger.nix
    ../develop/git.nix
  ];
  options = with lib;with lib.types;{
    lazyPackage = mkOption {
      type = listOf package;
      default = [ ];
    };
    neovim.pkg = mkOption {
      type = package;
      default = pkgs.wangzi-neovim.override { enable-all = config.neovim.full; };
    };
    neovim.full = mkOption {
      type = bool;
      default = false;
    };
  };
  config = {
    xdg = {
      /* enable = true; */
    };
    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
    };
    programs.fzf = {
      enable = true;
      enableZshIntegration = true;
    };
    manual.manpages.enable = true;
    home.sessionVariables = with pkgs; {
      EDITOR = "nvim";
      VISUAL = "nvim";
      NIX_AUTO_RUN = "1";
      NIXPKGS_ALLOW_UNFREE = "1";
      LIBVIRT_DEFAULT_URI = "qemu:///system";
      VIRSH_DEFAULT_CONNECT_URI = "qemu:///system";
    };
    home.activation.neovim = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      if [[ -e /run/secrets/shell/${config.home.username} ]]; then
        source /run/secrets/shell/${config.home.username}
      fi
      [[ -f $HOME/.cache/nvim/luacache ]] && rm $HOME/.cache/nvim/luacache || true
      [[ -f $HOME/.cache/nvim/luacache_chunks ]] && rm $HOME/.cache/nvim/luacache_chunks || true
      [[ -f $HOME/.cache/nvim/luacache_modpaths ]] && rm $HOME/.cache/nvim/luacache_modpaths || true
      if [[ ! -e $HOME/.cache/nix-index/files ]]; then
       mkdir $HOME/.cache/nix-index/ -p || true
       nix-index &
      fi
    '';
    lazyPackage = with pkgs;[ nmap ];
    home.packages = with pkgs;
      scripts ++ (builtins.map (pkg: let name = pkg.pname; in pkgs.writeShellScriptBin name "nix-shell -p ${name} --run ${name} $@") config.lazyPackage) ++ [
        rnix-lsp
        nixfmt
        neovim-remote
        config.neovim.pkg

        lm_sensors
        nix-tree
        nload
        killall
        atool
        bat
        choose
        #nmap
        curl
        direnv
        (nix-direnv.override { enableFlakes = true; })
        duf
        exa
        fd
        jq
        tldr
        just
        lsof
        pistol
        ps
        ripgrep
        silver-searcher
        choose
        sd
        trash-cli
        wget
        xh
        zip
        unzip
        xclip
        pfetch
        watchexec
        neofetch
        gzip
        openssh
      ];
    home.file.".config/nvim/parser/nix.so".source = lib.mkDefault "${pkgs.unstable.tree-sitter.builtGrammars.tree-sitter-nix}/parser";
    home.file.".code-server/bin/node" = { source = "${pkgs.nodejs-16_x}/bin/node"; executable = true; };
    home.file.".config/direnv/direnvrc".text = ''
      use_flake() {
        watch_file flake.nix
        watch_file flake.lock
        eval "$(nix print-dev-env --profile "$(direnv_layout_dir)/flake-profile")"
      }
    '';
    home.file.".config/nix/nix.conf".text = ''
      experimental-features = nix-command flakes
    '';
  };
}
