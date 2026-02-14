{
  config,
  pkgs,
  lib,
  ...
}:
let
  nodeConfig = config.cluster.nodeConfig;
in
{
  config = lib.mkIf (!nodeConfig.inContainer) {
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
      kernelParams = [
        "quite"
        "acpi_call"
      ];
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
      extraModulePackages = with config.boot.kernelPackages; [
        acpi_call
      ];

      kernelPackages = pkgs.linuxKernel.packages.linux_6_19;
    };
    services.logind.settings.Login.HandleLidSwitch = "ignore";
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
    services.logind.settings.Login = {
      HandlePowerKey = "suspend";
    };

    services.envfs = {
      enable = true;
    };

    programs.nix-ld = {
      enable = true;
    };

    services.acpid = {
      enable = true;
    };

    systemd.coredump.enable = true;
    boot.extraSystemdUnitPaths = [ "/etc/systemd-mutable/system" ];
    systemd.oomd = {
      enable = true;
      settings.OOM = {
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

    hardware.firmware = [ pkgs.linux-firmware ];

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
