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
    programs.git.userName = pkgs.lib.mkForce "wangzitao";
    programs.git.userEmail = pkgs.lib.mkForce "wangzitao@kingsoft.com";
    fonts.fontconfig.enable = true;
    home.packages = with pkgs;[ noto-fonts-cjk-sans ];
  };
}
