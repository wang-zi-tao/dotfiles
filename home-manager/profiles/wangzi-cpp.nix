{ nixpkgs, home-manager, pkgs, system, ... }: {
  pkgs = pkgs;
  system = system;
  username = "wangzi";
  homeDirectory = "/home/wangzi";
  configuration = {
    imports = [
      ../terminal/terminal.nix
      ../develop/cpp.nix
    ];
    neovim.full = true;
    programs.git.userName = lib.mkForce "wangzitao";
    programs.git.userEmail = lib.mkForce "wangzitao@kingsoft.com";
  };
}
