{ nixpkgs, home-manager, pkgs, ... }:
let system = "x86_64-linux"; in
{
  pkgs = pkgs;
  system = system;
  username = "root";
  homeDirectory = "/root";
  configuration.imports = [
    ../terminal/terminal.nix
  ];
}
