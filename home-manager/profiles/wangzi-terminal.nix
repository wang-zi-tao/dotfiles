{
  lib,
  config,
  pkgs,
  ...
}:
{
  imports = [ ../terminal/terminal.nix ];
  home.stateVersion = "23.05";
  home.username = "wangzi";
  home.homeDirectory = "/home/wangzi";
  neovim.full = true;
}
