{ pkgs, config, lib, ... }: {
  config = lib.mkIf config.cluster.nodeConfig.atuin.enable {
    services.atuin = {
      enable = true;
      port = 9005;
      host = "0.0.0.0";
      path = "/atuin";
    };
    networking.firewall.allowedTCPPorts = [ 6379 ];
  };
}
