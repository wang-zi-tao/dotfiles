{ nixpkgs, home-manager, pkgs, ... }: {
  pkgs = pkgs;
  system = pkgs.system;
  username = "wangzi";
  homeDirectory = "/home/wangzi";
  configuration = {
    imports = [
      ../application/application.nix
      ../desktop/desktop.nix
      ../terminal/terminal.nix
      ../develop/develop.nix
    ];
    neovim.full = true;
  };
}
