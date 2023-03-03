{ config, pkgs, lib, ... }: {
  config = lib.mkIf
    config.cluster.nodeConfig.develop.enable
    {
      environment.systemPackages = with pkgs; [
        config.boot.kernelPackages.perf
        perf-tools
        gperftools
        criu
      ];
      programs.criu.enable = true;
      networking.firewall.allowedTCPPorts = [ 1716 ];
      environment.etc.nixos = {
        source = ../.;
      };
    };
}
