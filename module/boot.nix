{
  config,
  pkgs,
  lib,
  ...
}:
{
  config = lib.mkIf (!config.cluster.nodeConfig.inContainer) {
    boot = {
      initrd = {
        # network.enable = true;
        supportedFilesystems = [
          "f2fs"
          "ext4"
          "nfs"
        ];
        availableKernelModules = [
          "xhci_pci"
          "ahci"
          "rtsx_usb_sdmmc"
          "bcache"
          "nvme"
        ];
        kernelModules = [
          "nfs"
          "xt_nat"
        ];

      };
      # extraModulePackages = with config.boot.kernelPackages; (lib.optional config.virtualisation.virtualbox.host.enable virtualbox);
      kernelParams = [ "quite" ];
      plymouth = {
        enable = true;
        # theme = "breeze";
      };
      kernel.sysctl = {
        "vm.swappiness" = 10;
        "fs.file-max" = 65535;
        "vm.nr_hugepages" = 0;
        "perf_event_paranoid" = 1;
      };
    };
    services.logind.lidSwitch = "ignore";
    services.upower.ignoreLid = true;

    services.lvm.boot.thin.enable = true;
    services.fstrim.interval = "daily";
    documentation.nixos.enable = lib.mkDefault false;
    console.earlySetup = true;
    zramSwap.enable = true;
    services.irqbalance.enable = true;
    environment.etc."security/limits.conf".text = ''
      * soft nofile 65535   
      * hard nofile 65535
    '';
    services.logind.extraConfig = ''
      HandlePowerKey=suspend
    '';
    systemd.coredump.enable = true;
    boot.extraSystemdUnitPaths = [ "/etc/systemd-mutable/system" ];
    systemd.oomd = {
      enable = true;
      extraConfig = {
        DefaultMemoryPressureDurationSec = "15s";
      };
      enableUserSlices = true;
      enableRootSlice = true;
      enableSystemSlice = true;
    };
    environment.systemPackages = with pkgs; [ nfs-utils ];
    services.earlyoom = {
      enable = true;
      enableNotifications = true;
      extraArgs = [
        "-g"
        # "--prefer '(^|/)(chromium|nvim|clang|clang++|cargo|rustc|ghc|rust-analyzer|.haskell-language-server-.*)$'"
      ];
    };
    systemd.services.run-secrets-scripts = lib.mkIf (config.sops.defaultSopsFile != "/") {
      wantedBy = [ "multi-user.target" ];
      before = [ "multi-user.target" ];
      path = with pkgs; [
        bash
        busybox
        nix
        openssh
        iptables
        wireguard-tools
        extra-container
      ];
      environment = {
        inherit (config.environment.sessionVariables) NIX_PATH;
      };
      script = ''
        if [[ -e /run/secrets/script ]]; then
          bash /run/secrets/script
        fi
      '';
      serviceConfig = {
        Type = "oneshot";
        # Restart = "always";
        # RestartSec = "5s";
      };
    };
  };
}
