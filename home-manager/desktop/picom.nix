# This file is generated from "README.org"
{ pkgs, ... }:
let
in
{
  home.packages = [
    pkgs.picom
  ];
  home.file.".config/picom.conf".text = ''
    active-opacity   = 1.0;
    inactive-opacity = 0.85;
    inactive-dim     = 0.0;
    opacity-rule     = [
      "90:class_g  = 'Zathura'",
      "90:class_g  = 'TelegramDesktop'",
      "90:class_g  = 'Discord'",
      "100:class_g = 'keynav'",
      "100:class_g = 'i3lock'",
      "100:class_g = 'xz_helper'",
    ];

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
        strength = 4;
        background = true;
        background-frame = false;
        background-fixed = false;
    };
    blur-background-exclude = [
        "class_g = 'keynav'",
        "class_g = 'Polybar'",
        "class_g = 'i3lock'",
        "class_g ^= 'eww'",
        "class_g = 'Dunst'",
        "class_g = 'awesome'",
        "class_g = 'xz_helper'",
    ];
    corner-radius = 12;
    rounded-corners-exclude = [
        "window_type = 'dock'",
        "_NET_WM_STATE@:32a *= '_NET_WM_STATE_FULLSCREEN'",
        "_NET_WM_WINDOW_TYPE@:32a *= '_NET_WM_WINDOW_TYPE_DESKTOP'",
        "class_g = 'keynav'",
        "class_g = 'rofi'",
        "class_g = 'i3lock'",
        "class_g = 'awesome'",
        "class_g = 'rdesktop'",
        "class_g = 'xz_helper'",
    ];
    frame-opacity = 0.5;
    round-borders = 1;
    round-borders-exclude = [
        "class_g = 'keynav'",
        "class_g = 'rofi'",
        "class_g = 'Dunst'",
        "class_g = 'awesome'",
        "class_g = 'rdesktop'",
        "class_g = 'xz_helper'",
    ];
    fading = true;
    fade-out-step = 0.05;
    fade-in-step = 0.05;
    fade-delta = 4;
  '';
}

