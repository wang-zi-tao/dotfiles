{ config, pkgs, lib, ... }: {
  config = lib.mkIf
    (config.cluster.nodeConfig.develop.enable)
    {
      environment.systemPackages = with pkgs; [
        config.boot.kernelPackages.perf
        perf-tools
        criu
      ];
      programs.criu.enable = true;
    };
}
