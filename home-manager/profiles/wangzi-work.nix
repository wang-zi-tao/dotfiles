{ lib, config, pkgs, ... }: {
  imports = [
    ../application/application.nix
    ../develop/cpp.nix
    ../terminal/terminal.nix
    ../develop/develop.nix
  ];
  home.stateVersion = "22.11";
  home.username = "wangzi";
  home.homeDirectory = "/home/wangzi";
  neovim.full = true;
  programs.git.userName = lib.mkForce "wangzitao";
  programs.git.userEmail = lib.mkForce "wangzitao@kingsoft.com";
}
