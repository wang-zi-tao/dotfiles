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
        xserver =
          {
            enable = true;
            exportConfiguration = true;
            displayManager.startx.enable = true;
            windowManager.xmonad.enable = true;
            windowManager.awesome = {
              enable = true;
              luaModules = with pkgs.luaPackages; [
                luarocks
                luadbi-mysql
              ];
            };
            desktopManager.gnome.enable = true;
            displayManager.xpra = {
              enable = false;
              bindTcp = "0.0.0.0:10000";
              pulseaudio = true;
            };
            xkbOptions = "ctrl:nocaps";
            modules = with pkgs.xorg; [ libXv libXtst libxcb xcbutilkeysyms xhost xbacklight ];
            extraConfig = ''
          '';
          };
      };
      environment.variables = {
        GI_TYPELIB_PATH = "${pkgs.playerctl}/lib/girepository-1.0:${pkgs.upower}/lib/girepository-1.0\${GI_TYPELIB_PATH:+:$GI_TYPELIB_PATH}";
      };
      programs.dconf.enable = true;
      programs.gpaste.enable = true;
      environment.systemPackages = with pkgs; [
        appimage-run
        glxinfo
        xorg.xhost
        xorg.xbacklight
        vulkan-tools

        libGLU
        libjpeg
        xorg.libXv
        xorg.libXtst
        ocl-icd
        opencl-info
        xorg.libxcb
        xorg.xcbutilkeysyms
        mesa.opencl
        mesa
        xorg.libXxf86vm
        glfw
        alsa-lib

        virtualgl
        tigervnc
        xpra
        qrcodegen
        cups
      ];
    };
}
