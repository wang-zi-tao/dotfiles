{
  lib,
  config,
  pkgs,
  ...
}:
{
  imports = [
    ../terminal/terminal.nix
    ../develop/develop.nix
    ../develop/cpp.nix
    ../platform/wsl.nix
  ];
  home.stateVersion = "24.11";
  home.username = "wangzi";
  home.homeDirectory = "/home/wangzi";
  neovim.full = true;
  programs.git.userName = pkgs.lib.mkForce "wangzitao";
  programs.git.userEmail = pkgs.lib.mkForce "wangzitao@kingsoft.com";
}
