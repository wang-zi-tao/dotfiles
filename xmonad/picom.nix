# This file is generated from "README.org"
{ pkgs, ... }:
let
  nur = import (builtins.fetchTarball
    "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
in {
  services.picom = {
    enable = false;
    package = nur.repos.reedrw.picom-next-ibhagwan;
    backend = "glx";
    experimentalBackends = true;
    opacityRule = [
      "90:class_g  = 'Zathura'"
      "90:class_g  = 'TelegramDesktop'"
      "90:class_g  = 'Discord'"
      "100:class_g = 'keynav'"
    ];
    extraOptions = ''
      detect-client-opacity = true;
      detect-rounded-corners = true;
      blur:
      {
          method = "kawase";
          strength = 8;
          background = true;
          background-frame = false;
          background-fixed = false;
      };
      blur-background-exclude = [
          "class_g = 'keynav'"
      ];
      corner-radius = 18;
      rounded-corners-exclude = [
          "window_type = 'dock'",
          "_NET_WM_STATE@:32a *= '_NET_WM_STATE_FULLSCREEN'",
          "_NET_WM_WINDOW_TYPE@:32a *= '_NET_WM_WINDOW_TYPE_DESKTOP'",
          "class_g = 'keynav'",
      ];
      round-borders = 1;
      round-borders-exclude = [
          "class_g = 'keynav'",
      ];
      fading = true;
      fade-out-step = 0.03;
      fade-in-step = 0.03;
      inactive-opacity = 0.90;
      active-opacity = 1;
    '';
  };
}
