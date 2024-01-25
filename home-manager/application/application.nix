{ config, pkgs, lib, ... }:
{
  imports = [ ./firefox.nix ./alacritty.nix ./dconf.nix ];
  lazyPackage = with pkgs;[
    blender
    gimp
    inkscape
    krita
    drawio
    figma-linux
    vlc
    obs-studio
    kdenlive
  ];
  home.packages = with pkgs; [
    libinput
    seatd
    mesa
    udev
    alsaLib
    vulkan-loader
    xorg.libXcursor
    xorg.libXrandr
    xorg.libXi # To use x11 feature
    libxkbcommon
    wayland # To use wayland feature
    libglvnd
    dbus.lib
    fontconfig.lib
    freetype
    libglvnd
    glib
    gtk4
    gtk3.debug
    gnome2.pango
    gdk-pixbuf
    remarkable-toolchain
    xorg.libX11
    cairo
    graphene
    xorg.libxcb
    libsForQt5.qt5.qtbase
    harfbuzz
    gvfs

  ] ++ [

    mpdris2
    libmpc
    mpd
    mpv
    scrot
    brightnessctl
    pamixer
    polkit_gnome
    inotify-tools
    upower

    xiezuo
    # wpsoffice
    qq

    # libreoffice
    libsForQt5.kdeconnect-kde
    # gimp
    # slack
    # inkscape
    # krita
    # shotcut
    # drawio
    # lens

    unstable.wine
    unstable.winetricks

    # wewechat
    # deepin-wine-tim
    # icalingua
    # nur.repos.linyinfeng.icalingua

    xdotool
    meld
    nextcloud-client

    gnome.file-roller
    gnome.baobab
    gnome.cheese
    gnome.gedit
    gnome.gnome-calculator
    gnome.gnome-calendar
    # gnome.gnome-characters
    gnome.gnome-clocks
    # gnome.gnome-contacts
    # gnome.gnome-font-viewer
    # gnome.gnome-logs
    # gnome.gnome-maps
    gthumb
    gnome.gnome-music
    gnome.gnome-screenshot
    gnome.gnome-system-monitor
    gnome.nautilus
    # gnome-connections
    gnome.totem
    # gnome.gnome-software
    # dolphin

    gnome.gnome-nettool
    gnome.gnome-todo
    # google-chrome
    dconf
    gnome.dconf-editor
    gnome.gnome-tweaks
    gnome.gpaste
    gnome.sushi
    gnome.nautilus
    gnome.seahorse

    tilix
    adwaita-qt
    gnome.adwaita-icon-theme
    libadwaita

    weston
    waypipe
    x11docker
    inotify-tools
    beekeeper-studio
    kazam
    flameshot
    peek
  ];
}
