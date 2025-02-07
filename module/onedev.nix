{ pkgs, config, lib, ... }:
let
  nodeConfig = config.cluster.nodes."${config.cluster.nodeName}";
  networkConfig =
    config.cluster.network.edges.${config.cluster.nodeName}.config;
in {
  config = lib.mkIf nodeConfig.OnedevServer.enable {
    virtualisation.oci-containers.containers.onedev = {
      image = "1dev/server";
      volumes =
        [ "onedev:/opt/onedev" "/var/run/docker.sock:/var/run/docker.sock" ];
      ports = [ "6610:6610" "6612:6611" ];
      environment = {
        initial_server_url =
          "https://${builtins.toString networkConfig.publicIp}/onedev/";
      };
    };
    services.caddy = lib.optionalAttrs nodeConfig.OnedevServer.enable {
      enable = true;
      virtualHosts = {
        "https://${builtins.toString networkConfig.publicIp}" = {
          extraConfig = ''
            route /onedev/* {
                uri strip_prefix /onedev
                reverse_proxy http://localhost:6610
            }
          '';
        };
        "https://${builtins.toString networkConfig.publicIp}:6613" = {
          extraConfig = ''
            tls internal
            reverse_proxy http://localhost:6610
          '';
        };
      };
    };
    virtualisation.docker = {
      enable = true;
      enableOnBoot = true;
    };
    networking.firewall.allowedUDPPorts = [ 6613 ];
    networking.firewall.allowedTCPPorts = [ 6612 6613 ];
  };
}
