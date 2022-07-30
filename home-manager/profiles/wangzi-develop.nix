{ nixpkgs, home-manager, pkgs, system, ... }: {
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
