{ pkgs, nixpkgs, home-manager, nix-on-droid, ... }:
let
  hostname = "wangzi-phone";
  system = "aarch64-linux";
in
nix-on-droid.lib.nixOnDroidConfiguration {
  config = { pkgs, config, lib, ... }: {
    environment.packages = with pkgs;[
      nix
      zig
      nix-zsh-completions
    ];
    environment.etcBackupExtension = ".bak";
    home-manager.config = { ... }: {
      imports = [ ../home-manager/terminal/terminal.nix ];
      programs.zsh.enableCompletion = false;
      home.activation.neovim = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        if [[ -f $HOME/.termux/font.ttf ]]; then
          mkdir .termux
          cp ${pkgs.iosevka-nerd}/share/fonts/truetype/Iosevka Nerd Font Complete.ttf $HOME/.termux/font.ttf
        fi
      '';
    };
    home-manager.useGlobalPkgs = true;
  };
  extraModules = [
    # import source out-of-tree modules like:
    # flake.nixOnDroidModules.module
  ];
  extraSpecialArgs = {
    # arguments to be available in every nix-on-droid module
  };
  pkgs = pkgs system;
  inherit system;
}
