{ pkgs, lib, config, ... }:
with lib; {
  options.theme = {

    foreground = mkOption {
      type = types.str;
      default = "#ffffff";
    };
    foreground1 = mkOption {
      type = types.str;
      default = "#D8DEE9";
    };
    background = mkOption {
      type = types.str;
      default = "#1c1b22";
    };
    background1 = mkOption {
      type = types.str;
      default = "#2B2A33";
    };
    background2 = mkOption {
      type = types.str;
      default = "#3C3B47";
    };
    pink = mkOption {
      type = types.str;
      default = "#D35D6E";
    };
    red = mkOption {
      type = types.str;
      default = "#BF616A";
    };
    yellow = mkOption {
      type = types.str;
      default = "#EBCB8B";
    };
    green = mkOption {
      type = types.str;
      default = "#A3BE8C";
    };
    sky = mkOption {
      type = types.str;
      default = "#88C0D0";
    };
    blue = mkOption {
      type = types.str;
      default = "#81A1C1";
    };
    purple = mkOption {
      type = types.str;
      default = "#B48EAD";
    };
  };
  imports = [ ./picom.nix ./rofi.nix ./dunst.nix ./eww/home.nix ];
  config = {
    home.packages = [ pkgs.lxappearance pkgs.lightlocker pkgs.playerctl pkgs.scrot ];
    xsession = {
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = hp: with hp; [ dbus monad-logger xmonad-contrib ];
        config = ./xmonad.hs;
      };
    };
    home.file.".xmonad/xmobar.hs" = { source = ./xmobar.hs; };
  };
}
