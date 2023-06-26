{ pkgs, config, lib, ... }:
with builtins; with lib;with lib.types; with lib.attrsets; let
  hostname = config.networking.hostName;
  networkCluster = config.cluster.network.edges;
  network = networkCluster.${hostname};
  networkGraph = pkgs.graphType {
    nodeOption = { nodeName, nodeConfig, ... }: {
      hostname = mkOption { type = str; default = nodeName; };
      publicIp = mkOption { type = nullOr str; default = null; };
      localIp = mkOption { type = nullOr str; default = null; };
      ips = mkOption { type = listOf str; default = [ ]; };
      doh.enable = mkOption { type = bool; default = false; };
    };
  };
in
{
  options = {
    cluster.network = mkOption {
      inherit (networkGraph) type;
      default = { };
    };
  };
  config = lib.mkMerge [
    (lib.mkIf network.config.doh.enable {
      networking.nameservers = [ "127.0.0.1" "::1" ];
      services.dnscrypt-proxy2 = {
        enable = true;
        settings = {
          ipv6_servers = true;
          require_dnssec = true;
          sources.public-resolvers = {
            urls = [
              "https://raw.githubusercontent.com/DNSCrypt/dnscrypt-resolvers/master/v3/public-resolvers.md"
              "https://download.dnscrypt.info/resolvers-list/v3/public-resolvers.md"
            ];
            cache_file = "/var/lib/dnscrypt-proxy2/public-resolvers.md";
            minisign_key = "RWQf6LRCGA9i53mlYecO4IzT51TGPpvWucNSCh1CBM0QTaLn73Y7GFO3";
          };
        };
      };
      systemd.services.dnscrypt-proxy2.serviceConfig = {
        StateDirectory = "dnscrypt-proxy";
      };
    })
    {
      networking.nameservers = [ "8.8.8.8" "9.9.9.9" ];
      cluster.network = networkGraph.function config.cluster.network;
      networking = {
        firewall = {
          enable = true;
          rejectPackets = true;
          allowedTCPPortRanges = [{
            from = 8880;
            to = 8890;
          }];
        };
        hosts = listToAttrs
          (concatLists
            (mapAttrsToList
              (nodeName: node:
                let
                  inherit (networkCluster.${nodeName}.config) publicIp;
                  inherit (networkCluster.${nodeName}.config) localIp;
                in
                if (publicIp != null) then
                  [{
                    name = publicIp;
                    value = [ nodeName ];
                  }] else if (localIp != null) then
                  [{
                    name = localIp;
                    value = [ nodeName ];
                  }] else [ ])
              network.peers)
          ) // {
          "192.30.255.113" = [ "github.com" ];
        };
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
              }
            }
            http_port 89
            default_sni ${toString network.config.publicIp}
          '';
        };
    }
  ];
}

