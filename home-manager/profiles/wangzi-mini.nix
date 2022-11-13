{ nixpkgs, home-manager, pkgs, ... }: {
  pkgs = pkgs;
  system = pkgs.system;
  username = "wangzi";
  homeDirectory = "/home/wangzi";
  configuration = {
    imports = [
      ../terminal/terminal.nix
    ];
  };
}
