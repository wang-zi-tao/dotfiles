{ pkgs, config, ... }: {
  imports = [
    # ./eww/eww.nix
    # ./rofi/rofi.nix
    # ./xmonad/xmonad.nix
    ./awesome/awesomewm.nix
    ./gnome.nix
    # ./polybar/polybar.nix
    # ./dunst.nix
    ./picom.nix
    ../develop/vscode.nix
  ];
  services.unclutter = {
    enable = true;
    timeout = 8;
  };
  xresources.properties = { "Xft.dpi" = 96; };
  home.file.".icons/default".source =
    "${pkgs.layan-cursor-theme}/share/icons/Layan-white Cursors";
  gtk =
    let
      extra_config = {
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
    in
    {
      enable = true;
      theme.name = "Orchis-Light";
      theme.package = pkgs.orchis-theme;
      iconTheme.name = "Tela-blue";
      iconTheme.package = pkgs.tela-icon-theme;
      font.package = pkgs.iosevka;
      font.name = "Iosevka Terminal";
      gtk3.extraConfig = extra_config;
      gtk4.extraConfig = extra_config;
    };
  qt = {
    enable = true;
    platformTheme = "gtk";
  };
  services.nextcloud-client = {
    enable = true;
    startInBackground = true;
  };
  services.kdeconnect = {
    enable = true;
    indicator = true;
  };
  manual.html.enable = true;
  news.display = "notify";
  home.sessionVariables = with pkgs; {
    XMODIFIERS = "@im=ibus";
    GTK_THEME = "Orchis-Light";
    GTK_IM_MODULE = "ibus";
    QT_IM_MODULE = "ibus";
    __NV_PRIME_RENDER_OFFLOAD = "1";
    __NV_PRIME_RENDER_OFFLOAD_PROVIDER = "NVIDIA-G0";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    __VK_LAYER_NV_optimus = "NVIDIA_only";
  };
  services.playerctld.enable = true;
  services.mpd.enable = true;
  home.file.".xinitrc-vgl".text = ''
    ${pkgs.xorg.xrdb}/bin/xrdb ~/.Xresources
    exec ${pkgs.slock}/bin/slock
  '';
}
