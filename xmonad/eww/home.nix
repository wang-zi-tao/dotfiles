{ pkgs, ... }:
let
in {
  home.packages = [ (pkgs.callPackage ../../packages/eww/default.nix { }) ];
  home.file.".config/eww/eww.scss" = { source = ./eww.scss; };
  home.file.".config/eww/scripts/weather_info" = {
    source = ./scripts/weather_info;
    executable = true;
  };
  home.file.".config/eww/scripts/trigger" = {
    source = ./scripts/trigger;
    executable = true;
  };
  home.file.".config/eww/eww.yuck".text = builtins.readFile ./eww.yuck + ''
    (deflisten music :initial ""
      "${pkgs.playerctl}/bin/playerctl --follow metadata --format '{{ title }}' || true")
    (deflisten workspaces :initial ""
      "${pkgs.xmonad-log}/bin/xmonad-log")

    (defpoll volume :interval "1s"
      "${pkgs.alsa-utils}/bin/amixer get Master | grep 'Left:' | awk -F'[][]' '{ print $2 }' | tr -d '%' | head -1")

    (defpoll time :interval "1s"
      "date '+%H:%M:%S %b %d'")
  '';
}
