# This file is generated from "README.org"
{ pkgs, ... }:
let
in {
  home.packages = [
    # pkgs.picom
    pkgs.nur.repos.reedrw.picom-next-ibhagwan
    # pkgs.nur.repos.arc.packages.picom-next
  ];
  home.file.".config/picom.conf".text = ''
    active-opacity   = 1.0;
    inactive-opacity = 0.9;
    inactive-dim     = 0.0;
    opacity-rule     = ["90:class_g  = 'Zathura'","90:class_g  = 'TelegramDesktop'","90:class_g  = 'Discord'","100:class_g = 'keynav'"];

    wintypes:
    {
      dock          = { shadow = false; };
      dnd           = { shadow = false; };
      popup_menu    = { opacity = 1.0; };
      dropdown_menu = { opacity = 1.0; };
    };

    backend = "glx";
    vsync = false;
    refresh-rate = 0;
    detect-client-opacity = true;
    detect-rounded-corners = true;
    blur:
    {
        method = "dual_kawase";
        strength = 8;
        background = true;
        background-frame = false;
        background-fixed = false;
    };
    blur-background-exclude = [
        "class_g = 'keynav'",
        "class_g = 'Polybar'"
    ];
    corner-radius = 12;
    rounded-corners-exclude = [
        "window_type = 'dock'",
        "_NET_WM_STATE@:32a *= '_NET_WM_STATE_FULLSCREEN'",
        "_NET_WM_WINDOW_TYPE@:32a *= '_NET_WM_WINDOW_TYPE_DESKTOP'",
        "class_g = 'keynav'",
        "class_g = 'rofi'",
    ];
    round-borders = 1;
    round-borders-exclude = [
        "class_g = 'keynav'",
        "class_g = 'rofi'",
    ];
    fading = true;
    fade-out-step = 0.1;
    fade-in-step = 0.1;
  '';
}
