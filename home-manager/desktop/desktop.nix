{
  pkgs,
  lib,
  config,
  ...
}:
let
  dark = {
    foreground = "#ffffff";
    foreground1 = "#D8DEE9";
    foreground2 = "#C8CED9";
    background = "#1c1b22";
    background1 = "#282c34";
    background2 = "#3C3B47";
  };
  light = {
    foreground = "#000000";
    foreground1 = "#181c14";
    foreground2 = "#888888";
    background = "#EDEFF1";
    background1 = "#DDDFE1";
    background2 = "#CDCFD1";
  };
  theme = dark // {
    pink = "#D35D6E";
    red = "#BF616A";
    yellow = "#EBCB8B";
    green = "#A3BE8C";
    sky = "#88C0D0";
    blue = "#3b99f8";
    purple = "#B48EAD";
  };
in
with lib;
{
  options.theme = mapAttrs (
    name: value:
    mkOption {
      type = types.str;
      default = value;
    }
  ) theme;
  imports = [
    # ./eww/eww.nix
    ./rofi/rofi.nix
    # ./xmonad/xmonad.nix
    ./awesome/awesomewm.nix
    ./gnome.nix
    # ./polybar/polybar.nix
    # ./dunst.nix
    ./picom.nix
    ../develop/vscode.nix
  ];
  config = {
    services.unclutter = {
      enable = true;
      timeout = 8;
    };
    xresources.properties = {
      "Xft.dpi" = 96;
    };
    home.file.".icons/default".source = "${pkgs.layan-cursor-theme}/share/icons/Layan-white Cursors";
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
        font.name = "Iosevka Nerd Font";
        gtk3.extraConfig = extra_config;
        gtk4.extraConfig = extra_config;
      };
    qt = {
      enable = true;
      platformTheme.name = "gtk";
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
    };
    services.playerctld.enable = true;
    home.file.".xinitrc-vgl".text = ''
      ${pkgs.xorg.xrdb}/bin/xrdb ~/.Xresources
      exec ${pkgs.slock}/bin/slock
    '';
    home.file.".config/mimeapps.list".source = ./mineapps.list;
    fonts.fontconfig.enable = true;
    home.packages = with pkgs; [
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-emoji
      noto-fonts-extra
      source-han-sans
      source-han-sans-simplified-chinese
      source-han-serif
      source-han-serif-simplified-chinese
      # hack-font
      powerline-fonts
      powerline-symbols
      iosevka
      iosevka-nerd
      # nerdfonts
      fira-code-symbols

      arphic-uming
      source-han-mono
      sarasa-gothic

      wmctrl

      winapps
    ];
  };
}
