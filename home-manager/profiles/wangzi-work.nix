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
  neovim.pkg = pkgs.wangzi-neovim.override {
    enable-all = config.neovim.full;
    enable-tabnine = false;
  };
  programs.git.userName = lib.mkForce "wangzitao";
  programs.git.userEmail = lib.mkForce "wangzitao@kingsoft.com";
}
