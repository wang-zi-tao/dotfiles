{ pkgs, nixpkgs, home-manager, nix-on-droid, ... }:
let
  hostname = "wangzi-phone";
  system = "aarch64-linux";
in
nix-on-droid.lib.nixOnDroidConfiguration {
  config = { pkgs, config, ... }: {
    environment.packages = with pkgs;[ ];
    environment.etcBackupExtension = ".bak";
    home-manager.config = { ... }: import ../home-manager/profiles/wangzi-mini.nix inputs;
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
