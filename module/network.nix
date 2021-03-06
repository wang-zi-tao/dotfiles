{ pkgs, config, lib, ... }:
with builtins; with lib;with lib.types; with lib.attrsets; let
  hostname = config.networking.hostName;
  networkCluster = config.cluster.network.edges;
  network = networkCluster.${hostname};
  networkGraph = pkgs.graphType {
    nodeOption = { nodeName, nodeConfig, ... }: {
      hostname = mkOption { type = str; default = nodeName; };
      publicIp = mkOption { type = nullOr str; default = null; };
      ips = mkOption { type = listOf str; default = [ ]; };
    };
  };
in
{
  options = {
    cluster.network = mkOption {
      type = networkGraph.type;
      default = { };
    };
  };
  config = {
    cluster.network = networkGraph.function config.cluster.network;
    networking = {
      firewall = {
        enable = true;
        rejectPackets = true;
      };
      hosts = listToAttrs (concatLists (mapAttrsToList
        (nodeName: node:
          let publicIp = networkCluster.${nodeName}.config.publicIp; in
          (optional (publicIp != null) {
            name = publicIp;
            value = [ nodeName ];
          }))
        network.peers)
      );
    };
    boot.kernel.sysctl = {
      "net.ipv4.ip_forward" = 1;
      "net.ipv4.conf.all.proxy_arp" = 1;
    };
    services.caddy = lib.optionalAttrs
      (network.config.publicIp != null)
      {
        globalConfig = ''
          servers {
            protocol {
              experimental_http3
            }
          }
          http_port 89
          default_sni ${toString network.config.publicIp}
        '';
      };
  };
}

