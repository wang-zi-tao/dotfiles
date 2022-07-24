{ nixpkgs, home-manager, pkgs-args, ... }:
let
  system = "x86_64-linux";
  pkgs = (import nixpkgs) (pkgs-args system);
in
home-manager.lib.homeManagerConfiguration {
  pkgs = pkgs;
  system = system;
  username = "wangzi";
  homeDirectory = "/home/wangzi";
  configuration = {
    imports = [
      ../terminal/terminal.nix
      ../develop/develop.nix
    ];
    neovim.full = true;
  };
}
