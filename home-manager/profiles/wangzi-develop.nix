{ nixpkgs, home-manager, pkgs, system, ... }: {
  pkgs = pkgs;
  system = system;
  username = "wangzi";
  homeDirectory = "/home/wangzi";
  configuration = {
    imports = [
      ../application/application.nix
      ../terminal/terminal.nix
      ../develop/develop.nix
      ../develop/cpp.nix
      ../desktop/desktop.nix
    ];
    neovim.full = true;
    programs.git.userName = pkgs.lib.mkForce "wangzitao";
    programs.git.userEmail = pkgs.lib.mkForce "wangzitao@kingsoft.com";
  };
}
