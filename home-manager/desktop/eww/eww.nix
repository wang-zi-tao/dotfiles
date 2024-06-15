{ config, pkgs, ... }:
let
  inherit (config) theme;
  eww = "${pkgs.eww}/bin/eww";
in
{
  home.packages = [
    pkgs.unstable.eww
    pkgs.jq
    pkgs.i3lock
    pkgs.wmctrl
    pkgs.brightnessctl
    pkgs.alsa-utils
    pkgs.networkmanager
  ];
  home.file.".config/eww/images".source = ./images;
  home.file.".config/eww/eww.yuck".text = ''
    ${builtins.readFile ./util.yuck}
    ${builtins.readFile ./dock.yuck}
    ${builtins.readFile ./panel.yuck}
    ${builtins.readFile ./mainpage.yuck}
  '';
  home.file.".config/eww/eww.scss".text = ''
    $theme-pink: ${theme.pink};
    $theme-red: ${theme.red};
    $theme-yellow: ${theme.yellow};
    $theme-green: ${theme.green};
    $theme-sky: ${theme.sky};
    $theme-blue: ${theme.blue};
    $theme-purple: ${theme.purple};
    $theme-foreground: ${theme.foreground};
    $theme-foreground1: ${theme.foreground1};
    $theme-foreground2: ${theme.foreground2};
    $theme-background: ${theme.background};
    $theme-background1: ${theme.background1};
    $theme-background2: ${theme.background2};
    ${builtins.readFile ./eww.scss}
    ${builtins.readFile ./dock.scss}
    ${builtins.readFile ./panel.scss}
    ${builtins.readFile ./mainpage.scss}
  '';
}
