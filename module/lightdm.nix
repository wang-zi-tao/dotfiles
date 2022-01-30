{ config, pkgs, lib, ... }: {
  config = lib.mkIf config.cluster.nodeConfig.guiServer.enable {
    services.xserver.displayManager.lightdm = {
      enable = true;
      greeter.enable = true;
      greeters.gtk.theme.package = pkgs.unstable.orchis-theme;
      greeters.gtk.theme.name = "Orchis-light";
      greeters.gtk.iconTheme.package = pkgs.tela-icon-theme;
      greeters.gtk.iconTheme.name = "Tela";
      # greeters.gtk.cursorTheme.package = pkgs.unstable.quintom-cursor-theme;
      # greeters.gtk.cursorTheme.name = "Quintom_Snow";
      greeters.gtk.cursorTheme.package = pkgs.layan-cursor-theme;
      greeters.gtk.cursorTheme.name = "Layan-white Cursors";
      background = ../static/login-background.png;
    };
  };
}
