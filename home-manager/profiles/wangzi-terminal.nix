{
  lib,
  config,
  pkgs,
  ...
}:
{
  imports = [ ../terminal/terminal.nix ];
  home.stateVersion = "24.11";
  home.username = "wangzi";
  home.homeDirectory = "/home/wangzi";
  neovim.full = true;
}
