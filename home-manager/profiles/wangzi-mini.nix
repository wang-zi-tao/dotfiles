{ nixpkgs, home-manager, pkgs, ... }:
let lib = pkgs.lib; in
{
  pkgs = pkgs;
  system = pkgs.system;
  username = "wangzi";
  homeDirectory = "/home/wangzi";
  configuration = {
    imports = [
      ../terminal/terminal.nix
    ];
    programs.zsh.enableCompletion = lib.mkForce false;
  };
}
