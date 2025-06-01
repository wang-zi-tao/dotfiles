{
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ./firefox.nix
    ./alacritty.nix
    ./dconf.nix
  ];
  lazyPackage = with pkgs; [
    blender
    gimp
    inkscape
    krita
    drawio
    figma-linux
    vlc
    obs-studio
    kdePackages.kdenlive
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
    "/nixfs/flake/str/nixpkgs#geogebra/bin/geogebra"
    gcolor3
    gpick
  ];
  home.packages =
    with pkgs;
    [
      libinput
      seatd
      mesa
      udev
      alsa-lib
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
      pango
      gdk-pixbuf
      xorg.libX11
      cairo
      graphene
      xorg.libxcb
      libsForQt5.qt5.qtbase
      harfbuzz
      gvfs
      obsidian
    ]
    ++ [

      mpdris2
      libmpc
      mpv
      scrot
      polkit_gnome
      inotify-tools

      # xiezuo
      wpsoffice
      qq

      # libreoffice
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

      file-roller
      gedit
      gnome-calculator
      gnome-calendar
      # gnome.gnome-characters
      gnome-clocks
      # gnome.gnome-contacts
      # gnome.gnome-font-viewer
      # gnome.gnome-logs
      # gnome.gnome-maps
      gthumb
      gnome-music
      gnome-screenshot
      gnome-system-monitor
      nautilus
      # gnome-connections
      totem
      # gnome.gnome-software
      # dolphin

      endeavour
      # google-chrome
      gpaste
      sushi
      nautilus
      seahorse

      adwaita-qt
      adwaita-icon-theme
      libadwaita

      inotify-tools
      kazam
      flameshot
    ];
}
