{ config, pkgs, lib, ... }: {
  config = lib.mkIf (!config.cluster.nodeConfig.inContainer)
    {
      boot = {
        initrd = {
          /* network.enable = true; */
          supportedFilesystems = [
            "f2fs"
          ];
        };
        extraModulePackages = with config.boot.kernelPackages;
          (lib.optional config.virtualisation.virtualbox.host.enable virtualbox) ++ [
            acpi_call
          ];
        kernelParams = [ "quite" ];
      };
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
        path = with pkgs; [ busybox nix ];
        environment = { inherit (config.environment.sessionVariables) NIX_PATH; };
        script = ''
          if [[ -e /run/secrets/script ]];then
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
