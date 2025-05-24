{
  lib,
  config,
  pkgs,
  ...
}:
{
  imports = [
    ../terminal/terminal.nix
    ../develop/cpp.nix
    ../develop/develop.nix
  ];
  home.stateVersion = "24.11";
  home.username = "macos";
  home.homeDirectory = /Users/macos;
  neovim.full = true;
  programs.git.userName = pkgs.lib.mkForce "wangzitao";
  programs.git.userEmail = pkgs.lib.mkForce "wangzitao@kingsoft.com";
}
