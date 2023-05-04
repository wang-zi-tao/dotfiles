{ lib, config, pkgs, ... }: {
  imports = [
    ../terminal/terminal.nix
  ];
  home.stateVersion = "22.11";
  home.username = "nix-on-droid";
  home.homeDirectory = "/data/data/com.termux.nix/files/home";
  programs.zsh.enableCompletion = lib.mkForce false;
}
