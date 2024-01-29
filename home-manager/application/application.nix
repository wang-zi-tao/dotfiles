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
    mpd
    brightnessctl
    pamixer
    upower
    unstable.wine
    unstable.winetricks
    xdotool
    meld
    nextcloud-client
    "gnome.baobab"
    "gnome.cheese"
    "gnome.gnome-nettool"
    dconf
    "gnome.dconf-editor"
    "gnome.gnome-tweaks"
    tilix
    weston
    waypipe
    x11docker
    beekeeper-studio
    peek
    remmina
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
    dbus.lib
    fontconfig.lib
    freetype
    glib
    gtk4
    gtk3.debug
    gnome2.pango
    gdk-pixbuf
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
    mpv
    scrot
    polkit_gnome
    inotify-tools

    # xiezuo
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


    # wewechat
    # deepin-wine-tim
    # icalingua
    # nur.repos.linyinfeng.icalingua


    gnome.file-roller
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

    gnome.gnome-todo
    # google-chrome
    gnome.gpaste
    gnome.sushi
    gnome.nautilus
    gnome.seahorse

    adwaita-qt
    gnome.adwaita-icon-theme
    libadwaita

    inotify-tools
    kazam
    flameshot
  ];
}
