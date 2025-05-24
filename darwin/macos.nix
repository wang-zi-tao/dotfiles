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
        system.stateVersion = 5;
        homebrew.enable = true;
        networking.hostName = hostname;
        users.users.macos = {
          home = /Users/macos;
        };
        services.openssh = {
          enable = true;
        };
        home-manager = {
          backupFileExtension = ".backup";
          useGlobalPkgs = true;
          useUserPackages = true;
          users.macos = import ../home-manager/profiles/wangzi-mac.nix;
        };
      }
    )
  ];
}
