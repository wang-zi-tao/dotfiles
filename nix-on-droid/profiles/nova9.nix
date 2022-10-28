inputs@{ pkgs, nixpkgs, home-manager, nix-on-droid, system, ... }:
let hostname = "wangzi-phone"; in
{
  config = { pkgs, config, ... }: {
    environment.packages = with pkgs;[ ];
    environment.etcBackupExtension = ".bak";
    home-manager.config = { ... }: (import ../../home-manager/profiles/wangzi-mini.nix inputs).configuration;
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
