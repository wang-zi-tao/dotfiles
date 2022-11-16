inputs@{ pkgs-template, nixpkgs, home-manager, nix-on-droid, ... }:
let
  hostname = "wangzi-nova9";
  system = "aarch64-linux";
  pkgs = pkgs-template system;
in
nix-on-droid.lib.nixOnDroidConfiguration {
  config = { pkgs, config, ... }: {
    environment.packages = with pkgs;[ ];
    environment.etcBackupExtension = ".bak";
    home-manager.config = { ... }: (import ../../home-manager/profiles/wangzi-mini.nix (inputs // { inherit pkgs; })).configuration;
    home-manager.useGlobalPkgs = true;
  };
  extraModules = [
    # import source out-of-tree modules like:
    # flake.nixOnDroidModules.module
  ];
  extraSpecialArgs = {
    # arguments to be available in every nix-on-droid module
  };
  inherit pkgs system;
}
