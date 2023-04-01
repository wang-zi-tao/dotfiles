{ lib, config, pkgs, ... }: {
  imports = [
    ../desktop/desktop.nix
    ../terminal/terminal.nix
  ];
  home.stateVersion = "22.11";
  home.username = "wangzi";
  home.homeDirectory = "/home/wangzi";
  neovim.full = true;
}
