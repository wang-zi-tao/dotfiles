{ lib, config, pkgs, ... }: {
  imports = [
    ../terminal/terminal.nix
  ];
  home.stateVersion = "22.11";
  home.username = "wangzi";
  home.homeDirectory = "/home/wangzi";
  programs.zsh.enableCompletion = lib.mkForce false;
}
