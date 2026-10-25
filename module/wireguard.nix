{
  pkgs,
  config,
  lib,
  ...
}:
with builtins;
with lib;
with lib.types;
with lib.attrsets;
let
  hostname = config.networking.hostName;
  networkCluster = config.cluster.network.edges;
  network = networkCluster.${hostname};
  wireguardCluster = config.cluster.wireguard.edges;
  wireguard = wireguardCluster.${hostname};
  wireguardGraph = pkgs.graphType {
    nodeOption =
      { nodeName, nodeConfig, ... }:
      {
        index = mkOption { type = ints.u8; };
        clusterIp = mkOption {
          type = str;
          default = "192.168.16.${toString nodeConfig.config.index}";
        };
        clusterIps = mkOption {
          type = listOf str;
          default = [ ];
        };
        clusterIpRange = mkOption {
          type = str;
          default = "10.16.${toString nodeConfig.config.index}.0/24";
        };
        port = mkOption {
          type = nullOr ints.u16;
          default = null;
        };
        publicKey = mkOption { type = str; };
        gateway = mkOption {
          type = nullOr str;
          default = null;
        };
        gatewayServer = mkOption {
          type = bool;
          default = false;
        };
        iptables.enable = mkOption {
          type = bool;
          default = false;
        };
        nat = mkOption {
          type = nullOr str;
          default = null;
        };
        extraHostname = mkOption {
          type = listOf str;
          default = [ ];
        };
      };
  };
  enabled = hasAttr hostname wireguardCluster;
  mkIfWireguard = mkIf enabled;
in
{
  options = {
    cluster.keys.wireguard.sharedKeySops = mkOption { type = path; };
    cluster.wireguard = mkOption {
      inherit (wireguardGraph) type;
      default = { };
    };
  };
  imports = [
    { cluster.wireguard = wireguardGraph.function config.cluster.wireguard; }
    ./network.nix
  ];
  config = {
    networking.firewall.trustedInterfaces = mkIfWireguard [
      "wg0"
    ];
    lazyPackage = with pkgs; mkIfWireguard [ wireguard-tools ];
    networking.hosts = mkIfWireguard (
      listToAttrs (
        concatLists (
          mapAttrsToList (
            nodeName: node:
            let
              nodeWireguard = wireguardCluster.${nodeName};
            in
            optionals (hasAttr nodeName wireguardCluster) [
              {
                name = nodeWireguard.config.clusterIp;
                value = [ "${nodeName}.wg" ] ++ nodeWireguard.config.extraHostname;
              }
            ]
          ) config.cluster.network.edges.${hostname}.peers
        )
      )
    );
    sops.secrets."wireguard/private-key" = mkIfWireguard { };
    networking.firewall.allowedUDPPorts = mkIfWireguard [
      wireguard.config.port
      (wireguard.config.port + 1)
    ];
    networking.firewall.allowedUDPPortRanges = mkIfWireguard [
      {
        from = 50000;
        to = 55555;
      }
    ];

    boot.kernel.sysctl = mkIf (enabled && wireguard.config.iptables.enable) {
      "net.ipv4.conf.${wireguard.config.nat}.route_localnet" = 1;
    };

    networking.nat = mkIf (enabled && wireguard.config.iptables.enable) {
      enable = true;
      enableIPv6 = true;
      externalInterface = wireguard.config.nat;
      internalInterfaces = [
        "wg0"
        "wg1"
      ];
    };

    networking.wireguard = optionalAttrs enabled {
      enable = true;
      interfaces = {
        wg0 = {
          ips = [ wireguard.config.clusterIp ];
          listenPort = wireguard.config.port;
          privateKeyFile = config.sops.secrets."wireguard/private-key".path;
          mtu = 1200;
          peers = mapAttrsToList (
            otherNodeName: edge:
            let
              otherNodeNetworkConfig = networkCluster.${otherNodeName}.config;
              otherNodeWireguardConfig = wireguardCluster.${otherNodeName}.config;
            in
            {
              persistentKeepalive = if network.config.publicIp == null then 32 else 0;
              inherit (otherNodeWireguardConfig) publicKey;
              # WireGuard 的 endpoint 由 cluster-node 在运行时改写：开启 wgTunnel 后
              # 跨网段邻居会被指向 127.0.0.1:(peer.index + portBase)，走 h2 长连接。
              endpoint =
                if otherNodeNetworkConfig.publicIp != null then
                  "${otherNodeNetworkConfig.publicIp}:${toString otherNodeWireguardConfig.port}"
                else if otherNodeNetworkConfig.localIp != null then
                  "${otherNodeNetworkConfig.localIp}:${toString otherNodeWireguardConfig.port}"
                else
                  null;
              allowedIPs = [
                otherNodeWireguardConfig.clusterIp
                otherNodeWireguardConfig.clusterIpRange
              ]
              ++ otherNodeWireguardConfig.clusterIps
              ++ optionals (wireguard.config.gateway == otherNodeName) (
                concatLists (
                  mapAttrsToList (
                    nodeName: node:
                    optionals (!hasAttr nodeName wireguard.peers) [
                      node.config.clusterIp
                      node.config.clusterIpRange
                    ]
                  ) wireguardCluster
                )
              );
            }
          ) wireguard.peers;
        };
      };
    };
    sops.secrets."wg-shared-keys" = mkIfWireguard {
      sopsFile = config.cluster.keys.wireguard.sharedKeySops;
    };
  };
}
