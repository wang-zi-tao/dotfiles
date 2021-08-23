{ config, pkgs, ... }:

let
  mypolybar = pkgs.polybar.override {
    alsaSupport = true;
    githubSupport = true;
    mpdSupport = true;
    pulseSupport = true;
  };
  colors = builtins.readFile ./polybar-colors;
  xmonad = ''
    [module/xmonad]
    type = custom/script

    exec = ${pkgs.xmonad-log}/bin/xmonad-log
    tail = true
  '';
in {
  services.polybar = {
    enable = true;
    package = mypolybar;
    config = ./polybar-config;
    extraConfig = colors + xmonad;
    script = ''
      polybar top &
      polybar bottom &
    '';
  };
}
