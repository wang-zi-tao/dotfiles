{ config, pkgs, lib, ... }:
let inherit (config.cluster) nodeConfig; in
{
  config = lib.mkIf nodeConfig.guiClient.enable {
    services.xserver.displayManager.lightdm = {
      enable = true;
      greeter.enable = true;
      greeters.gtk.theme.package = pkgs.orchis-theme;
      greeters.gtk.theme.name = "Orchis";
      greeters.gtk.iconTheme.package = pkgs.tela-icon-theme;
      greeters.gtk.iconTheme.name = "Tela-bule";
      greeters.gtk.cursorTheme.package = pkgs.layan-cursor-theme;
      greeters.gtk.cursorTheme.name = "Layan-white Cursors";
      greeters.gtk.indicators = [
        "~spacer"
        "~clock"
        "~spacer"
        "~session"
        "~a11y"
        "~power"
      ];
      greeters.gtk.clock-format = "%H:%M:%S";
      background = "${pkgs.resources}/share/backgrounds/locked_wallpaper.png";
    };
    services.xserver.displayManager.gdm = {
      enable = false;
      wayland = true;
      debug = true;
    };
    # hardware.nvidia.modesetting.enable = true;
    services.xserver.displayManager.sddm.enable = false;
    # services.touchegg.enable = true;
  };
}
