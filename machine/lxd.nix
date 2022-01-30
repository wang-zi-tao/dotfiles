{ pkgs, nixpkgs, nixos-generators, home-manager, ... }:
let hostname = "lxd";
in nixos-generators.nixosGenerate {
  format = "lxc";
  # system = "x86_64-linux";
  modules = [
    ../module/cluster.nix
    home-manager.nixosModules.home-manager
    ({
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.users.root = { ... }: {
        imports = [ ../home-manager/terminal/terminal.nix ];
        inherit hostname;
      };
    })
    ({ pkgs, lib, config, modulesPath, ... }: {
      boot.isContainer = true;
      environment.noXlibs = false;
      systemd.suppressedSystemUnits = [
        "console-getty.service"
        "getty@.service"
        "systemd-udev-trigger.service"
        "systemd-udevd.service"
        "sys-fs-fuse-connections.mount"
        "sys-kernel-debug.mount"
        "dev-mqueue.mount"
      ];
      environment.systemPackages = with pkgs; [
        xpra
        virtualgl
        firefox
        # google-chrome-stable
        # vscode
        nix-tree
        qt5ct
        glxinfo
      ];
      networking = { interfaces.eth0.useDHCP = true; };
      nixpkgs.config.allowUnfree = true;
      i18n.defaultLocale = "zh_CN.UTF-8";
      gtk = { iconCache.enable = true; };
      fonts = {
        fontconfig = {
          enable = true;
          defaultFonts.emoji = [ "Noto Color Emoji" ];
          defaultFonts.monospace = [ "Iosevka Custom Medium" ];
          defaultFonts.sansSerif =
            [ "Inter" "Liberation Sans" "Soruce Han Sans SC" ];
          defaultFonts.serif = [ "Liberation Serif" "Source Han Serif SC" ];
        };
        fonts = with pkgs; [
          noto-fonts
          source-code-pro
          source-sans-pro
          source-han-sans
          source-han-sans-simplified-chinese
          powerline-fonts
          powerline-symbols
          fira-code-symbols
        ];
        fontDir.enable = true;
      };
      sound.enable = true;
      i18n.inputMethod = {
        enabled = "ibus";
        ibus = { engines = with pkgs.unstable.ibus-engines; [ libpinyin ]; };
        uim.toolbar = "qt4";
      };
      hardware = {
        opengl.enable = true;
        opengl.setLdLibraryPath = true;
        opengl.driSupport = true;
      };
    })
  ];
  inherit pkgs;
}
