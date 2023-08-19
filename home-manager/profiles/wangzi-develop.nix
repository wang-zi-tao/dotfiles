{ lib, config, pkgs, ... }: {
  imports = [
    ../application/application.nix
    ../terminal/terminal.nix
    ../develop/develop.nix
    ../develop/cpp.nix
    ../desktop/desktop.nix
  ];
  home.stateVersion = "22.11";
  home.username = "wangzi";
  home.homeDirectory = "/home/wangzi";
  neovim.full = true;
  programs.git.userName = pkgs.lib.mkForce "wangzitao";
  programs.git.userEmail = pkgs.lib.mkForce "wangzitao@kingsoft.com";
  systemd.user.services.barrier.Service.enable = pkgs.lib.mkForce false;
}
