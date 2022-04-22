{ config, pkgs, lib, ... }: {
  config = lib.mkIf
    (config.cluster.nodeConfig.guiClient.enable
      || config.cluster.nodeConfig.guiClient.enable)
    {
      i18n.defaultLocale = "zh_CN.UTF-8";
      gtk = { iconCache.enable = true; };
      fonts = {
        fontconfig = {
          enable = true;
          defaultFonts.emoji = [ "Noto Color Emoji" ];
          defaultFonts.monospace = [ "Iosevka Custom Medium" "Powerline" ];
          defaultFonts.sansSerif =
            [ "Inter" "Liberation Sans" "Soruce Han Sans SC" "Powerline" ];
          defaultFonts.serif = [ "Liberation Serif" "Source Han Serif SC" "Powerline" ];
        };
        fonts = with pkgs; [
          noto-fonts
          noto-fonts-cjk
          noto-fonts-emoji
          noto-fonts-extra
          source-han-sans
          source-han-sans-simplified-chinese
          source-han-serif
          source-han-serif-simplified-chinese
          hack-font
          powerline-fonts
          powerline-symbols
          iosevka
          fira-code-symbols
        ];
        fontDir.enable = true;
      };
      sound.enable = true;
      xdg = {
        # autostart.enable = false;
        portal.enable = true;
        mime.defaultApplications = {
          "application/pdf" = "firefox.desktop";
          "image/png" = [ "gthumb.desktop" ];
          "text/xml" = [ "nvim.desktop" ];
        };
      };
      i18n.inputMethod = {
        enabled = "ibus";
        ibus = { engines = with pkgs.unstable.ibus-engines; [ libpinyin ]; };
        uim.toolbar = "qt4";
      };
      services = {
        mpd.enable = true;
        gnome.core-os-services.enable = true;
        gnome.sushi.enable = true;
        gvfs.enable = true;
        udisks2.enable = true;
        flatpak.enable = true;
        xserver = {
          enable = true;
          exportConfiguration = true;
          displayManager.startx.enable = true;
          windowManager.xmonad.enable = true;
          desktopManager.gnome.enable = true;
          displayManager.xpra = {
            enable = false;
            bindTcp = "0.0.0.0:10000";
            pulseaudio = true;
          };
          xkbOptions = "ctrl:nocaps";
        };
      };
      programs.dconf.enable = true;
      programs.gpaste.enable = true;
      environment.systemPackages = with pkgs; [
        appimage-run
        glxinfo
        xorg.xhost
        xorg.xbacklight
        vulkan-tools
        virtualgl
        turbovnc
        xpra
        qrcodegen
        cups
      ];
    };
}
