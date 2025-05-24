{
  pkgs,
  config,
  lib,
  ...
}:
{
  imports = [
    ./tmux/tmux.nix
    ./zsh/zsh.nix
    ./procs/procs.nix
    ./htop.nix
    ./ranger/ranger.nix
    ../develop/git.nix
    ./zellij.nix
  ];
  options =
    with lib;
    with lib.types;
    {
      lazyPackage = mkOption {
        type = listOf (oneOf [
          package
          str
        ]);
        default = [ ];
      };
      neovim.pkg = mkOption {
        type = package;
        default = pkgs.wangzi-neovim.override {
          enable-all = config.neovim.full;
        };
      };
      neovim.full = mkOption {
        type = bool;
        default = false;
      };
    };
  config = {
    xdg = {
      # enable = true;
    };
    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
      enableNushellIntegration = true;
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
      NIXPKGS_ALLOW_BROKEN = "1";
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
    lazyPackage = with pkgs; [
      "/nixfs/flake/str/nixpkgs#btop/bin/btop"
      nmap
      gitui
      lazygit
    ];
    home.packages =
      with pkgs;
      scripts
      ++ (builtins.map (
        pkg:
        if (builtins.typeOf pkg == "string") then
          if (lib.strings.hasPrefix "/" pkg) then
            pkgs.writeShellScriptBin (lib.lists.last (lib.strings.splitString "/" pkg)) ''exec ${pkg} $@''
          else
            pkgs.writeShellScriptBin (lib.lists.last (lib.strings.splitString "." pkg)) ''nix run nixpkgs#"${pkg}" -- $@''
        else
          let
            name = if (builtins.hasAttr "pname" pkg) then pkg.pname else pkg.name;
          in
          pkgs.writeShellScriptBin name ''nix run nixpkgs#"${name}" -- $@''
      ) config.lazyPackage)
      ++ [
        neovim-remote
        config.neovim.pkg

        nix-tree
        killall
        atool
        bat
        choose
        #nmap
        curl
        direnv
        nix-direnv
        duf
        eza
        fd
        jq
        xq
        tldr
        just
        lsof
        pistol
        ps
        ripgrep
        silver-searcher
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
        perl
        dnsutils

        github-copilot-cli
      ]
      ++ (lib.optionals (pkgs.system == "x86_64-linux") [
        nload
        lm_sensors
      ]);
    home.file.".config/nvim/parser/nix.so".source = lib.mkDefault "${pkgs.unstable.tree-sitter.builtGrammars.tree-sitter-nix}/parser";
    # home.file.".config/nvim/parser/rust.so".source = lib.mkDefault "${pkgs.unstable.tree-sitter.builtGrammars.tree-sitter-rust}/parser";
    # home.file.".code-server/bin/node" = { source = "${pkgs.nodejs-16_x}/bin/node"; executable = true; };
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
