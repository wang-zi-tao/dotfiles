{ pkgs, home-manager, ... }:
home-manager.lib.homeManagerConfiguration {
  configuration = {
    imports = [ ../home-manager/terminal/terminal.nix ];
    nixpkgs.config = { modules = [ ../module/nixos.nix ]; };
  };
  system = "x86_64-linux";
  username = "root";
  homeDirectory = "/root";
}
