{
  lib,
  config,
  pkgs,
  ...
}:
{
  imports = [ ../terminal/terminal.nix ];
  home.stateVersion = "24.11";
  home.username = "nix-on-droid";
  home.homeDirectory = "/data/data/com.termux.nix/files/home";
  programs.zsh.enableCompletion = lib.mkForce false;
  programs.atuin.enable = lib.mkForce false;
}
