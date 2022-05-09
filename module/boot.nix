{ config, pkgs, lib, ... }: {
  config = lib.mkIf (!config.cluster.nodeConfig.inContainer) {
    boot.extraModulePackages = with config.boot.kernelPackages; [
      virtualbox
      acpi_call
    ];
    boot.kernelParams = [ "quite" ];
    environment.systemPackages = with pkgs; [
      config.boot.kernelPackages.perf
      perf-tools
      criu
      bcache-tools
      bcachefs-tools
    ];
    programs.criu.enable = true;
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
