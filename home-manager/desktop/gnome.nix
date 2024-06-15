{ pkgs, config, ... }:
{
  home.packages = with pkgs; [
    flat-remix-gnome
    gnomeExtensions.net-speed
    gnomeExtensions.gsconnect
    gnomeExtensions.gesture-improvements
    gnomeExtensions.blur-my-shell
    gnomeExtensions.x11-gestures
    gnomeExtensions.dash-to-dock
    gnomeExtensions.system-monitor
    touchegg
  ];
}
