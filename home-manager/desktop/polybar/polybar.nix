{ config, pkgs, ... }:

let
  theme = config.theme;
  colors = builtins.readFile ./polybar-colors + ''
    BACKGROUND = ${theme.background}
    FOREGROUND = ${theme.foreground}

    BLACK = ${theme.background1}
    RED = ${theme.red}
    GREEN = ${theme.green}
    YELLOW = ${theme.yellow}
    BLUE = ${theme.blue}
    MAGENTA = ${theme.purple}
    CYAN = ${theme.sky}
    WHITE = ${theme.background1}
    ALTBLACK = #2D2D2D
    ALTRED = #99324B
    ALTGREEN = #4F894C
    ALTYELLOW = #9A7500
    ALTBLUE = #8FBCBB
    ALTMAGENTA = #97365B
    ALTCYAN = #398EAC
    ALTWHITE = #ECEFF4

    foreground-alt = ${theme.red}
  '';
in
{
  services.polybar = {
    enable = true;
    config = ./polybar-config;
    extraConfig = colors;
    script = ''
      # polybar icons &
    '';
  };
  home.packages = with pkgs; [ xmonad-log ];
}
