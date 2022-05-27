{ pkgs, lib, config, ... }:
let
  dark = {
    foreground = "#ffffff";
    foreground1 = "#D8DEE9";
    foreground2 = "#C8CED9";
    background = "#1c1b22";
    background1 = "#282c34";
    background2 = "#3C3B47";
  };
  light = {
    foreground = "#000000";
    foreground1 = "#181c14";
    foreground2 = "#888888";
    background = "#EDEFF1";
    background1 = "#DDDFE1";
    background2 = "#CDCFD1";
  };
  theme = dark // {
    pink = "#D35D6E";
    red = "#BF616A";
    yellow = "#EBCB8B";
    green = "#A3BE8C";
    sky = "#88C0D0";
    blue = "#81A1C1";
    purple = "#B48EAD";
  };
in
with lib; {
  options.theme = mapAttrs
    (name: value:
      mkOption {
        type = types.str;
        default = value;
      })
    theme;
  config = {
    home.packages = with pkgs;
      [ lxappearance lightlocker playerctl scrot imagemagickBig i3lock-fancy ];
    xsession = {
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = hp: with hp; [ dbus monad-logger xmonad-contrib ];
        config = ./xmonad.hs;
      };
    };
    services = {
      # blueman-applet.enable = true;
      network-manager-applet.enable = true;
    };
    # services.random-background = {
    # enable = true;
    # imageDirectory = "${pkgs.backgrounds}/share/backgrounds";
    # };
    home.file.".xmonad/wallpapers/".source = "${pkgs.resources}/share/backgrounds/";
    home.file.".xmonad/background.jpg".source = "${pkgs.resources}/share/backgrounds/大鱼海棠16.jpg";
  };
}
