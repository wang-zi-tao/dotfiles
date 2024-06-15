{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.cluster;
  nodeConfig = cfg.nodes.${cfg.nodeName};
  networkConfig = config.cluster.network.nodes.${config.cluster.nodeName}.config;
in
with lib;
with builtins;
{
  config = lib.mkIf nodeConfig.webssh.enable {
    systemd.services = {
      webssh = {
        wantedBy = [ "multi-user.target" ];
        script = ''
          ${pkgs.webssh}/bin/wssh --port=64535 --log-to-stderr
        '';
      };
    };
    networking.firewall.allowedUDPPorts = [ 64536 ];
    networking.firewall.allowedTCPPorts = [ 64536 ];
    services.caddy = {
      enable = true;
      virtualHosts = {
        "https://${builtins.toString networkConfig.publicIp}:64536" = {
          extraConfig = ''
            @hostname {
              query hostname=*
              query username=*
            }
            @post {
              method POST
            }
            reverse_proxy @hostname http://localhost:64535
            reverse_proxy @post http://localhost:64535
            reverse_proxy /static/* http://localhost:64535
            reverse_proxy /ws http://localhost:64535
            # reverse_proxy http://localhost:64535
          '';
        };
      };
    };
  };
}
