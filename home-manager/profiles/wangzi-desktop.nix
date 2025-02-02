{
  lib,
  config,
  pkgs,
  ...
}:
{
  imports = [
    ../application/application.nix
    ../desktop/desktop.nix
    ../terminal/terminal.nix
    ../develop/develop.nix
  ];
  home.stateVersion = "24.11";
  home.username = "wangzi";
  home.homeDirectory = "/home/wangzi";
  neovim.full = true;
  services.rustdesk.enable = true;
}
