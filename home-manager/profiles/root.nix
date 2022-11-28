{ lib, config, pkgs, ... }: {
  imports = [
    ../terminal/terminal.nix
  ];
  home.stateVersion = "22.11";
  home.username = "root";
  home.homeDirectory = "/root";
}
