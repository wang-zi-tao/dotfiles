{ lib, config, pkgs, ... }: {
  imports = [
    ../terminal/terminal.nix
  ];
  home.stateVersion = "22.11";
  programs.zsh.enableCompletion = lib.mkForce false;
}
