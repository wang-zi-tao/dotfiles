{ config, pkgs, lib, ... }: {
  config = lib.mkMerge [
    (lib.mkIf config.cluster.nodeConfig.guiClient.enable {
      services = {
        xserver =
          {
            enable = true;
            exportConfiguration = true;
            displayManager.startx.enable = true;
            # windowManager.xmonad.enable = true;
            windowManager.awesome.enable = true;
            desktopManager.gnome.enable = true;
            windowManager.default = "awesome";
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
    })
    (lib.mkIf (! config.cluster.nodeConfig.guiClient.enable) {
      networking.firewall.allowedTCPPorts = [ 10000 ];
      services = {
        xserver = {
          enable = true;
          # exportConfiguration = true;
          displayManager.xpra = {
            enable = true;
            bindTcp = "0.0.0.0:10000";
            pulseaudio = true;
          };
          # modules = with pkgs.xorg; [ libXv libXtst libxcb xcbutilkeysyms xhost xbacklight ];
          # extraConfig = '' '';
        };
      };
      systemd.services.display-manager.path = with pkgs;[ python39Full xdummy ];
      services.xserver.displayManager.job.execCmd = lib.mkForce ''
        export PULSE_COOKIE=/run/pulse/.config/pulse/cookie
        exec ${pkgs.xpra}/bin/xpra start \
          --daemon=off \
          --log-dir=/var/log \
          --log-file=xpra.log \
          --opengl=on \
          --clipboard=on \
          --notifications=on \
          --speaker=yes \
          --mdns=no \
          --pulseaudio=no \
          --sound-source=pulse \
          --socket-dirs=/run/xpra \
          --bind-tcp=0.0.0.0:10000 \
          --auth=pam \
          --xvfb="xdummy ${builtins.concatStringsSep " " config.services.xserver.displayManager.xserverArgs}" \
      '';
    })
    (lib.mkIf
      (config.cluster.nodeConfig.guiServer.enable
        || config.cluster.nodeConfig.guiClient.enable)
      {
        gtk = {
          iconCache.enable = true;
        };
        fonts = {
          fontconfig = {
            enable = true;
            defaultFonts.monospace = [ "Iosevka Custom Medium" ];
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
            iosevka-nerd
            # nerdfonts
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
        i18n.defaultLocale = "zh_CN.UTF-8";
        i18n.supportedLocales = [
          "en_US.UTF-8/UTF-8"
          "zh_CN.UTF-8/UTF-8"
        ];
        i18n.inputMethod = {
          enabled = "ibus";
          ibus = { engines = with pkgs.ibus-engines; [ libpinyin ]; };
          uim.toolbar = "qt4";
        };
        services = {
          mpd.enable = true;
          gnome.core-os-services.enable = true;
          gnome.sushi.enable = true;
          gvfs.enable = true;
          udisks2.enable = true;
          flatpak.enable = true;
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
          # xpraWithNvenc
          xpra
          xwayland
          weston
          qrcodegen
          cups
        ];
      })
  ];
}
