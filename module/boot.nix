{ config, pkgs, lib, ... }: {
  config = lib.mkIf (!config.cluster.nodeConfig.inContainer) {
    boot = {
      initrd = {
        /* network.enable = true; */
        supportedFilesystems = [
          "f2fs"
        ];
      };
      # extraModulePackages = with config.boot.kernelPackages; (lib.optional config.virtualisation.virtualbox.host.enable virtualbox);
      kernelParams = [ "quite" ];
      plymouth = {
        enable = true;
        /* theme = "breeze"; */
      };
      kernel.sysctl = {
        "vm.swappiness" = 100;
      };
    };
    documentation.nixos.enable = lib.mkDefault false;
    console.earlySetup = true;
    zramSwap.enable = true;
    services.irqbalance.enable = true;
    boot.kernel.sysctl = { "fs.file-max" = 65535; };
    environment.etc."security/limits.conf".text = ''
      * soft nofile 65535   
      * hard nofile 65535
    '';
    services.logind.extraConfig = ''
      HandlePowerKey=suspend
    '';
    boot.extraSystemdUnitPaths = [ "/etc/systemd-mutable/system" ];
    systemd.services.run-secrets-scripts = lib.mkIf (config.sops.defaultSopsFile != "/") {
      wantedBy = [ "multi-user.target" ];
      before = [ "multi-user.target" ];
      path = with pkgs; [ busybox nix openssh ];
      environment = { inherit (config.environment.sessionVariables) NIX_PATH; };
      script = ''
        if [[ -e /run/secrets/script ]]; then
          /run/secrets/script
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
