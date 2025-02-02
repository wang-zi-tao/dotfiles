{
  lib,
  config,
  pkgs,
  ...
}:
{
  imports = [
    ../desktop/desktop.nix
    ../terminal/terminal.nix
  ];
  home.stateVersion = "24.11";
  home.username = "wangzi";
  home.homeDirectory = "/home/wangzi";
  neovim.full = true;
}
