inputs@{
  nix-darwin,
  pkgs-template,
  home-manager,
  ...
}:
let
  hostname = "macos";
  system = "x86_64-darwin";
  pkgs = pkgs-template system;
in
nix-darwin.lib.darwinSystem {
  specialArgs = inputs;
  inherit pkgs system;
  modules = [
    home-manager.darwinModules.home-manager
    (
      { pkgs, lib, ... }:
      {
        homebrew.enable = true;
        hom-manager = {
          backupFileExtension = ".backup";
          useGlobalPkgs = true;
          useUserPackages = true;
          users.macos = import ../home-manager/profiles/wangzi-mac.nix;
        };
      }
    )
  ];
}
