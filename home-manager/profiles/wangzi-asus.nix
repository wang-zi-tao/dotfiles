{ lib, config, pkgs, ... }: {
  imports = [
    ../application/application.nix
    ../desktop/desktop.nix
    ../terminal/terminal.nix
    ../develop/develop.nix
  ];
  home.stateVersion = "23.05";
  home.username = "wangzi";
  home.homeDirectory = "/home/wangzi";
  neovim.full = true;
  services.rustdesk.enable = true;
  home.sessionVariables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
    _JAVA_OPTIONS = "-Dsun.java2d.uiScale=2";
  };
}
