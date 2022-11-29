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
            windowManager.awesome.package = pkgs.nixpkgs-22-05.awesome;
            desktopManager.gnome.enable = true;
            displayManager.defaultSession = "none+awesome";
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
    (lib.mkIf (config.cluster.nodeConfig.guiServer.enable && ! config.cluster.nodeConfig.guiClient.enable) {
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
        services.xrdp.enable = true;
        services.xrdp.defaultWindowManager = "${pkgs.awesome}/bin/awesome";
        # services.xrdp.defaultWindowManager = "${pkgs.gnome-console}/bin/kgx";
        environment.etc."X11/Xwrapper.config".text = "allowed_users=anybody";
        environment.etc."xrdp/xrdp.ini".text = ''
          [xrdp8]
          name=Desktop
          lib=libvnc.so
          username=ask
          password=ask
          ip=0.0.0.0
          port=5900
        '';
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
          turbovnc
          # xpraWithNvenc
          xpra
          xpra-html5
          pulseaudioFull
          xwayland
          weston
          qrcodegen
          cups
        ];
      })
  ];
}
