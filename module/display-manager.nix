{ config, pkgs, lib, ... }:
let nodeConfig = config.cluster.nodeConfig; in
{
  config = lib.mkIf nodeConfig.guiServer.enable {
    services.xserver.displayManager.lightdm = {
      enable = true;
      greeter.enable = true;
      greeters.gtk.theme.package = pkgs.orchis-theme;
      greeters.gtk.theme.name = "Orchis-light";
      greeters.gtk.iconTheme.package = pkgs.tela-icon-theme;
      greeters.gtk.iconTheme.name = "Tela";
      greeters.gtk.cursorTheme.package = pkgs.layan-cursor-theme;
      greeters.gtk.cursorTheme.name = "Layan-white Cursors";
      background = "${pkgs.resources}/share/backgrounds/大鱼海棠8.jpg";
    };
    services.xserver.displayManager.gdm = {
      enable = false;
      wayland = false;
      # nvidiaWayland = true;
    };
    # hardware.nvidia.modesetting.enable = true;
    services.touchegg.enable = true;
  };
}
