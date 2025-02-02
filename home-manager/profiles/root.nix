{
  lib,
  config,
  pkgs,
  ...
}:
{
  imports = [ ../terminal/terminal.nix ];
  home.stateVersion = "24.11";
  home.username = "root";
  home.homeDirectory = "/root";
}
