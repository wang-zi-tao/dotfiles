{
  pkgs,
  config,
  lib,
  ...
}:
let
  networkConfig = config.cluster.network.edges.${config.cluster.nodeName}.config;
  sops-enable = config.sops.defaultSopsFile != "/";
in
{
  config = lib.mkMerge [
    (lib.mkIf config.cluster.nodeConfig.atuin.enable {
      services.atuin = {
        enable = true;
        port = 9005;
        host = "0.0.0.0";
        path = "/atuin";
        openRegistration = true;
      };
      services.caddy = {
        enable = true;
        virtualHosts = {
          "https://${builtins.toString networkConfig.publicIp}:9006" = {
            extraConfig = ''
              respond / 404
              respond /favicon.ico 404
              reverse_proxy http://localhost:9005
            '';
          };
        };
      };
      networking.firewall.allowedTCPPorts = [ 9006 ];
    })
    (lib.mkIf sops-enable {
      sops.secrets."atuin-key" = {
        sopsFile = config.cluster.ssh.publicKeySops;
        owner = "root";
        mode = "0555";
        neededForUsers = true;
      };
    })
  ];
}
