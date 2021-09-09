{ config, pkgs, lib, ... }:
let
  nvidia-run = pkgs.writeShellScriptBin "nvidia-run" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';
in {
  imports = [ ./hardware-configuration.nix ./fs.nix ];

  hardware = {
    opengl.driSupport32Bit = true;
    opengl.enable = true;
    opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
    opengl.setLdLibraryPath = true;
    nvidia.prime = {
      # sync.enable = true;
      offload.enable = true;
      nvidiaBusId = "PCI:1:0:0";
      intelBusId = "PCI:0:2:0";
    };
    # nvidia.package=config.boot.kernelPackages.nvidiaPackages.vulkan_beta;
  };
  boot.loader = {
    systemd-boot.enable = true;
    systemd-boot.configurationLimit = 5;
    efi = {
      canTouchEfiVariables = false;
      efiSysMountPoint = "/boot/efi";
    };
    timeout = 1;
  };
  # boot.plymouth.enable = false;
  boot.kernelPackages = pkgs.linuxPackages_5_13.extend (self: super: {
    virtualbox = super.virtualbox.override { inherit (self) kernel; };
  });
  boot.extraModulePackages = with config.boot.kernelPackages; [
    virtualbox
    # perf
    acpi_call
  ];
  boot.kernelParams = [ "quite" ];
  nixpkgs.config.allowUnfree = true;
  networking.hostName = "wangzi-pc"; # Define your hostname.
  time.timeZone = "Asia/Shanghai";
  networking = {
    #useDHCP = true;
    firewall.enable = false;
    networkmanager = { enable = true; };
    wireguard = { enable = true; };
    proxy.default = "http://127.0.0.1:8889";
  };
  i18n.defaultLocale = "zh_CN.UTF-8";
  gtk = { iconCache.enable = true; };
  fonts = {
    fontconfig = {
      enable = true;
      defaultFonts.emoji = [ "Noto Color Emoji" ];
      defaultFonts.monospace = [ "Iosevka Terminal" "Source Code Pro Medium" "Hack" "Sarasa Mono SC" ];
      defaultFonts.sansSerif =
        [ "Inter" "Liberation Sans" "Soruce Han Sans SC" ];
      defaultFonts.serif = [ "Liberation Serif" "Source Han Serif SC" ];
    };
    fonts = with pkgs; [
      # wqy_zenhei
      # wqy_microhei
      nerdfonts
      # ubuntu_font_family
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      noto-fonts-extra
      source-code-pro
      source-sans-pro
      source-han-mono
      source-han-sans
      source-han-sans-simplified-chinese
      source-han-serif
      source-han-serif-simplified-chinese
      # arphic-ukai
      hack-font
      powerline-fonts
      powerline-symbols
      iosevka
      # terminus_font
      # terminus-nerdfont
      # terminus_font_ttf
    ];
    fontDir.enable = true;
  };
  services = {
    # power-profiles-daemon.enable = false;
    # tlp.enable = true;
    sshd.enable = true;
    flatpak.enable = true;
    xserver = {
      enable = true;
      exportConfiguration = true;
      videoDrivers = [
        # "intel"
        # "modesetting"
        "nvidia"
        # "nouveau"
      ];
      # deviceSection = ''
      # Option "TearFree" "true"
      # '';
      displayManager.gdm.enable = true;
      displayManager.gdm.wayland = false;
      # displayManager.lightdm.enable= true;
      desktopManager.gnome.enable = true;
      # desktopManager.plasma5.enable = true;
      # desktopManager.mate.enable = true;
      windowManager.xmonad.enable = true;
      # windowManager.i3.enable = true;
      libinput = {
        enable = true;
        mouse = { accelSpeed = "1.0"; };
        touchpad = {
          naturalScrolling = true;
          horizontalScrolling = true;
          accelSpeed = "1.0";
        };
      };
      modules = [ pkgs.xorg.xf86videointel pkgs.xorg.xf86inputlibinput ];
    };
    # snapper.snapshotInterval = "daily";
    # NetworkManager-wait-online.enable = false;
  };
  systemd.services.NetworkManager-wait-online.enable = false;
  systemd.services.touchegg = {
    enable = true;
    description = "Touchégg. The daemon.";
    wantedBy = [ "graphical.target" ];
    serviceConfig = {
      Type = "simple";
      Group = "input";
      Restart = "always";
      RestartSec = "5s";
      wants = [ "graphical.target" ];
      after = [ "graphical.target" ];
      ExecStart = "${pkgs.touchegg}/bin/touchegg --daemon";
    };
  };
  systemd.services.snapper-snapshot = {
    serviceConfig.Type = "oneshot";
    script =''
        for i in `ls /etc/snapper/configs/`
        do
          ${pkgs.snapper}/bin/snapper -c $i create -c timeline -d timeline
          ${pkgs.snapper}/bin/snapper -c $i cleanup timeline
        done
      '';
  };
  systemd.timers.snapper-snapshot = {
    wantedBy = [ "timers.target" ];
    partOf = [ "snapper-snapshot.service" ];
    timerConfig.OnCalendar = [ "*-*-* *:00:00" ];
  };
  systemd.services.n2n_edge = {
    enable = true;
    description = "wangzi n2n network";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = "5s";
      ExecStart =
        "${pkgs.unstable.n2n}/bin/edge -t 20000 -d n2n -a 192.168.0.3 -c n2n -A1 -l 139.9.235.87:49 -r -f";
    };
  };
  nixpkgs.overlays = (map (name: import (./overlays + "/${name}"))
    (builtins.attrNames (builtins.readDir ./overlays))) ++ [
      (final: prev: rec {
        touchegg = prev.callPackage ./packages/touchegg { };
      })
      # pkgs.fenix.overlay
    ];
  systemd.services.k3s = {
    enable = false;
    description = "k3s.wangzicloud.cn";
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.wireguard-tools pkgs.busybox pkgs.bash ];
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = "2s";
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      ExecStart =
        "${pkgs.k3s}/bin/k3s agent --node-taint mobile=true:NoSchedule --server https://139.9.235.87:6443 --token RvAN4PA/MRYqko6lTMGPnBGvx2kFNZ5yTTMnRy+IobwXbr2bSw8Ghe2RWqb9Jmtd ";
    };
  };
  programs.dconf.enable = true;
  programs.zsh.enable = true;
  programs.ssh.askPassword =
    "${pkgs.gnome.seahorse}/libexec/seahorse/ssh-askpass";
  environment.systemPackages = with pkgs; [
    nvidia-run
    home-manager
    busybox
    xorg.xhost
    glxinfo
    intel-gpu-tools
    appimage-run
    duperemove
    # config.boot.kernelPackages.perf
    perf-tools
    btrfs-progs
  ];
  i18n.inputMethod = {
    enabled = "ibus";
    ibus = { engines = with pkgs.ibus-engines; [ libpinyin ]; };
  };
  nix = {
    gc.automatic = true;
    gc.dates = "weekly";
    gc.options = "-d";
    buildCores = 10;
    optimise.automatic = true;
    # binaryCaches = lib.mkForce [ ];
    package = pkgs.nixFlakes;
    extraOptions = lib.optionalString (config.nix.package == pkgs.nixFlakes)
      "experimental-features = nix-command flakes";
  };
  services.gnome.sushi.enable = true;
  users.users.root = { shell = pkgs.zsh; };
  users.groups.wangzi = {
    gid = 1000;
    name = "wangzi";
  };
  users.users.wangzi = {
    isNormalUser = true;
    uid = 1000;
    shell = pkgs.zsh;
    group = "wangzi";
    description = "王子陶";
    extraGroups = [
      "wheel"
      "networkmanager"
      "vboxusers"
      "docker"
    ]; # Enable ‘sudo’ for the user.
  };

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  programs.gpaste.enable = true;

  virtualisation = {
    virtualbox = {
      host = {
        enable = true;
        enableHardening = true;
        enableExtensionPack = true;
      };
    };
    docker = {
      enable = true;
      enableNvidia = true;
      enableOnBoot = true;
      storageDriver = "btrfs";
    };
  };
  environment.etc."docker/daemon.json".text = ''
    {
      "registry-mirrors": [
        "https://registry.cn-hangzhou.aliyuncs.com",
        "https://mirror.ccs.tencentyun.com",
        "https://registry.docker-cn.com"
      ]
    }
  '';

  # system.stateVersion = "21.11"; # Did you read the comment?

}
