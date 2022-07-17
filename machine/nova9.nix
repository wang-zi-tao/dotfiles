{ pkgs, nixpkgs, home-manager, nix-on-droid, ... }:
let
  hostname = "wangzi-phone";
  system = "aarch64-linux";
in
nix-on-droid.lib.nixOnDroidConfiguration {
  config = { pkgs, config, ... }: {
    environment.packages = with pkgs;[
      nix
      gcc
      nix-zsh-completions
    ];
    environment.etcBackupExtension = ".bak";
    home-manager.config = { lib, ... }: {
      imports = [
        ../home-manager/terminal/terminal.nix
      ];
      programs.zsh.enableCompletion = false;
      home.activation.termux-font = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        if [[ -f $HOME/.termux/font.ttf ]]; then
          mkdir .termux || true
          cp ${pkgs.iosevka-nerd}/share/fonts/truetype/Iosevka Nerd Font Complete.ttf $HOME/.termux/font.ttf || true
        fi
      '';
    };
    home-manager.useGlobalPkgs = true;
  };
  extraModules = [
    # import source out-of-tree modules like:
    # flake.nixOnDroidModules.module
    ../home-manager/sshd.nix
  ];
  extraSpecialArgs = {
    # arguments to be available in every nix-on-droid module
  };
  /* user.shell = "${pkgs.zsh}/bin/zsh"; */
  pkgs = pkgs system;
  inherit system;
}
