{ pkgs, nixpkgs, home-manager, nix-on-droid, ... }:
let
  hostname = "wangzi-phone";
  system = "aarch64-linux";
in
nix-on-droid.lib.nixOnDroidConfiguration {
  config = { pkgs, config, ... }: {
    environment.packages = with pkgs;[
      nix 
      zig
      nix-zsh-completions
    ];
    environment.etcBackupExtension = ".bak";
    home-manager.config = { ... }: {
      imports = [ ../home-manager/terminal/terminal.nix ];
      programs.zsh.enableCompletion = false;
      home.file.".termux/font.ttf".source = "${pkgs.iosevka-nerd}/share/fonts/truetype/Iosevka Nerd Font Complete.ttf";
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
  /* user.shell = "${pkgs.zsh}/bin/zsh"; */
  pkgs = pkgs system;
  inherit system;
}
