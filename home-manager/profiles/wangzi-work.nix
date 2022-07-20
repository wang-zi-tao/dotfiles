{ nixpkgs, home-manager, ... }:
let
  system = "x86_64-linux";
  pkgs = (import nixpkgs) (pkgs-args system);
  lib = pkgs.lib;
in
{
  pkgs = pkgs;
  system = system;
  username = "wangzi";
  homeDirectory = "/home/wangzi";
  configuration = {
    imports = [
      ../application/application.nix
      ../desktop/desktop.nix
      ../terminal/terminal.nix
      ../develop/develop.nix
    ];
    programs.git.userName = lib.mkForce "wangzitao";
    programs.git.userEmail = lib.mkForce "wangzitao@kingsoft.com";
  };
}
