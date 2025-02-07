{ pkgs, config, lib, ... }:
with builtins;
with lib;
with lib.types;
with lib.attrsets;
let
  hostname = config.networking.hostName;
  networkCluster = config.cluster.network.edges;
  network = networkCluster.${hostname};
  nodeConfig = config.cluster.nodes."${config.cluster.nodeName}";
  networkConfig = config.cluster.network.edges.${config.cluster.nodeName}.config;
  networkGraph = pkgs.graphType {
    nodeOption = { nodeName, nodeConfig, ... }: {
      hostname = mkOption {
        type = str;
        default = nodeName;
      };
      publicIp = mkOption {
        type = nullOr str;
        default = null;
      };
      localIp = mkOption {
        type = nullOr str;
        default = null;
      };
      ips = mkOption {
        type = listOf str;
        default = [ ];
      };
      doh.enable = mkOption {
        type = bool;
        default = false;
      };
    };
  };
in {
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
            minisign_key =
              "RWQf6LRCGA9i53mlYecO4IzT51TGPpvWucNSCh1CBM0QTaLn73Y7GFO3";
          };
        };
      };
      systemd.services.dnscrypt-proxy2.serviceConfig = {
        StateDirectory = "dnscrypt-proxy";
      };
    })
    {
      networking.nameservers = [ "240C::6666" "8.8.8.8" "9.9.9.9" ];
      cluster.network = networkGraph.function config.cluster.network;
      networking = {
        firewall = {
          enable = true;
          pingLimit = "--limit 1/minute --limit-burst 5";
          # extraCommands = ''
          #   DEV=ens3
          #   IP_DENY_SECOND=30
          #   INPUT="-A nixos-fw"
          #   PORT_SCAN_MAX=3
          #   IP_SET_MAX=$((100 * 1024 * 1024 / 8 / 60 * $IP_DENY_SECOND))
          #   ${pkgs.ipset}/bin/ipset create scanner-ip-set hash:ip timeout $IP_DENY_SECOND maxelem $IP_SET_MAX counters
          #   iptables -A trap-scan -m set --match-set scanner-ip-set src -j DROP
          #   iptables -A trap-scan -j SET --add-set scanner-ip-set src 
          #   iptables -A trap-scan -j DROP
          #   iptables $INPUT -p tcp --syn -m set ! --match-set pub-port-set dst -j trap-scan
          #   iptables $INPUT -p tcp --syn -m set ! --update-counters --match-set scanner-ip-set src --packets-gt $PORT_SCAN_MAX -j DROP
          #   iptables $INPUT -p tcp ! --syn -m conntrack ! --ctstate ESTABLISHED,RELATED -j DROP
          #   ip46tables -D INPUT -j nixos-fw
          # '';

          # extraStopCommands = ''
          #     ip46tables -D nixos-fw -j trap-scan
          # '';
        };
        hosts = listToAttrs (concatLists (mapAttrsToList (nodeName: node:
          let
            inherit (networkCluster.${nodeName}.config) publicIp;
            inherit (networkCluster.${nodeName}.config) localIp;
          in if (publicIp != null) then [{
            name = publicIp;
            value = [ nodeName ];
          }] else if (localIp != null) then [{
            name = localIp;
            value = [ nodeName ];
          }] else
            [ ]) network.peers)) 
        // {
            "127.0.0.1" = ["localhost.local" (hostname + ".local")];
        };
      };
      boot.kernel.sysctl = {
        "net.ipv4.ip_forward" = 1;
        "net.ipv4.conf.all.proxy_arp" = 1;
      };
      services.caddy = lib.optionalAttrs (network.config.publicIp != null) {
        globalConfig = ''
          servers {
          }
          http_port 89
          default_sni ${toString network.config.publicIp}
        '';
        virtualHosts = {
          "https://${builtins.toString networkConfig.publicIp}" = {
            extraConfig = ''
              log {
              	# output stderr
              	level DEBUG
              	format console
              }
              tls internal
            '';
          };
        };
      };
      networking.firewall.allowedTCPPorts = [ 80 443 ];
      environment.systemPackages = with pkgs; [ nss ];
    }
  ];
}
