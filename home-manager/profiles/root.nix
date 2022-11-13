{ nixpkgs, home-manager, pkgs, ... }: {
  pkgs = pkgs;
  system = pkgs.system;
  username = "root";
  homeDirectory = "/root";
  configuration.imports = [
    ../terminal/terminal.nix
  ];
}
