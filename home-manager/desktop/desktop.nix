{ pkgs, config, ... }: {
  imports = [
    ./eww/eww.nix
    ./rofi/rofi.nix
    ./xmonad/xmonad.nix
    ./polybar/polybar.nix
    ./dunst.nix
    ./picom.nix
  ];
  services.unclutter = {
    enable = true;
    timeout = 8;
  };
  xresources.properties = { "Xft.dpi" = 96; };
  services.betterlockscreen = { enable = true; };
  home.file.".icons/default".source =
    "${pkgs.layan-cursor-theme}/share/icons/Layan-white Cursors";
  gtk = {
    enable = true;
    theme.name = "Orchis-light";
    theme.package = pkgs.unstable.orchis-theme;
    iconTheme.name = "Tela-blue";
    iconTheme.package = pkgs.tela-icon-theme;
    font.package = pkgs.iosevka;
    font.name = "Iosevka Terminal";
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = false;
      gtk-button-images = true;
      gtk-cursor-theme-name = "Layan-white-cursors";
      gtk-cursor-theme-size = 24;
      gtk-decoration-layout = "close,maximize,minimize:";
      gtk-enable-animations = true;
      gtk-enable-event-sounds = 0;
      gtk-enable-input-feedback-sounds = 0;
      # gtk-font-name=Noto Sans,  10;
      gtk-menu-images = true;
      gtk-modules = "gail:atk-bridge:colorreload-gtk-module";
      gtk-primary-button-warps-slider = false;
      gtk-toolbar-icon-size = "GTK_ICON_SIZE_LARGE_TOOLBAR";
      gtk-toolbar-style = 3;
      gtk-xft-antialias = 1;
      gtk-xft-hinting = 1;
      gtk-xft-hintstyle = "hintslight";
      gtk-xft-rgba = "rgb";
    };
  };
  qt = {
    enable = true;
    platformTheme = "gtk";
  };
  # services.nextcloud-client = {
  # enable = true;
  # startInBackground = true;
  # };
  services.kdeconnect = {
    enable = true;
    indicator = true;
  };
  manual.html.enable = true;
  news.display = "notify";
  home.sessionVariables = with pkgs; {
    XMODIFIERS = "@im=ibus";
    GTK_IM_MODULE = "ibus";
    QT_IM_MODULE = "ibus";
    http_proxy = "http://127.0.0.1:8889";
    https_proxy = "http://127.0.0.1:8889";
    HTTP_PROXY = "http://127.0.0.1:8889";
    HTTPS_PROXY = "http://127.0.0.1:8889";
    ALL_PROXY = "socks5://127.0.0.1:1089";
    NO_PROXY =
      "localhost,127.0.0.1,10.96.0.0/12,192.168.99.0/24,192.168.39.0/24";
    CURL_NIX_FLAGS = "-x $http_proxy";
    __NV_PRIME_RENDER_OFFLOAD = "1";
    __NV_PRIME_RENDER_OFFLOAD_PROVIDER = "NVIDIA-G0";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    __VK_LAYER_NV_optimus = "NVIDIA_only";
  };
  home.packages = with pkgs; [
    unstable.qv2ray
    v2ray
    unstable.wpsoffice
    # libreoffice
    libsForQt5.kdeconnect-kde
    # gimp
    # slack
    # inkscape
    # krita
    # shotcut
    drawio
    lens
    feh

    wewechat
    # deepin-wine-tim
    # icalingua
    # nur.repos.linyinfeng.icalingua

    xdotool
    meld
    nextcloud-client

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
    pkgs.gnome-connections
    gnome.totem
    gnome.gnome-software
    dolphin

    gnome.gnome-nettool
    pkgs.unstable.gnome.gnome-todo
    google-chrome
    dconf
    gnome.dconf-editor
    gnome.gnome-tweaks
    gnome.gpaste
    gnome.sushi
    gnome.nautilus
    gnome.seahorse

    adwaita-qt
    gnome.adwaita-icon-theme
    libadwaita
  ];
}
