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
    };
}
