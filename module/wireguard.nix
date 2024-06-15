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
        clusterIp2 = mkOption {
          type = str;
          default = "192.168.17.${toString nodeConfig.config.index}";
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
      };
    edgeOption = _: {
      tunnel = mkOption {
        type = bool;
        default = false;
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
      "wg1"
    ];
    environment.systemPackages = with pkgs; mkIfWireguard [ wireguard-tools ];
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
                value = [ "${nodeName}.wg" ];
              }
              {
                name = nodeWireguard.config.clusterIp2;
                value = [ "${nodeName}.wg1" ];
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
    networking.wireguard = optionalAttrs enabled {
      enable = true;
      interfaces = {
        wg0 = {
          postSetup = lib.optionalString wireguard.config.iptables.enable "${pkgs.iptables}/bin/iptables -A FORWARD -i wg0 -j ACCEPT; ${pkgs.iptables}/bin/iptables -A FORWARD -o wg0 -j ACCEPT; ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -o ens3 -j MASQUERADE";
          postShutdown = lib.optionalString wireguard.config.iptables.enable "${pkgs.iptables}/bin/iptables -D FORWARD -i wg0 -j ACCEPT; ${pkgs.iptables}/bin/iptables -D FORWARD -o wg0 -j ACCEPT; ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -o ens3 -j MASQUERADE";
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
              endpoint =
                if edge.tunnel then
                  "127.0.0.1:${toString (otherNodeWireguardConfig.index + 40000)}"
                else if otherNodeNetworkConfig.publicIp != null then
                  "${otherNodeNetworkConfig.publicIp}:${toString otherNodeWireguardConfig.port}"
                else if otherNodeNetworkConfig.localIp != null then
                  "${otherNodeNetworkConfig.localIp}:${toString otherNodeWireguardConfig.port}"
                else
                  null;
              allowedIPs =
                [
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
    environment.etc."wg_netmanager/peer.yaml" = mkIfWireguard {
      text = lib.generators.toYAML { } {
        wgInterface = "wg1";
        wgIp = wireguard.config.clusterIp2;
        name = network.config.hostname;
        existingInterface = false;
      };
    };
    environment.etc."wg_netmanager/network.yaml" = mkIfWireguard {
      text = lib.generators.toYAML { } {
        network = {
          subnet = "192.168.17.0/24";
        };
        peers = concatLists (
          mapAttrsToList (
            name: wireguardNode:
            let
              networkNodeConfig = networkCluster.${name}.config;
            in
            optional (networkNodeConfig.publicIp != null) {
              endPoint = "${networkNodeConfig.publicIp}:${toString (wireguardNode.config.port + 1)}";
              # if networkNodeConfig.publicIp != null then
              # "${networkNodeConfig.publicIp}:${toString (wireguardNode.config.port+1)}"
              # else
              # "${networkNodeConfig.localIp}:${toString (wireguardNode.config.port+1)}";
              adminPort = 52343;
              wgIp = wireguardNode.config.clusterIp2;
            }
          ) wireguardCluster
        );
      };
    };
    networking.firewall.allowedTCPPorts = mkIfWireguard (
      [ 52343 ]
      ++ concatLists (
        mapAttrsToList (
          otherNodeName: edge: (optional edge.tunnel (wireguardCluster.${otherNodeName}.config.index + 40000))
        ) wireguard.peers
      )
    );
    systemd.services = mkIfWireguard (
      listToAttrs (
        (optionals enabled [
          (nameValuePair "wg-netmanager" {
            enable = true;
            description = "Wireguard network manager wg1";
            wantedBy = [ "multi-user.target" ];
            after = [ "network.target" ];
            path = with pkgs; [
              wg-netmanager
              jq
              wireguard-tools
              iproute2
            ];
            script = ''
              TMPFILE=$(mktemp /tmp/wg-XXXXX)
              trap "rm -f $TMPFILE" EXIT
              jq ".network += {\"sharedKey\": \"$(cat ${config.sops.secrets.wg-shared-keys.path})\"}" /etc/wg_netmanager/network.yaml > $TMPFILE
              wg_netmanager ${
                if (network.config.publicIp == null) then "-H" else "-w ${toString (wireguard.config.port + 1)}"
              } -c $TMPFILE
            '';
            serviceConfig = {
              Type = "simple";
              Restart = "always";
              ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
              ExecStop = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
              ReadWritePaths = [ "/tmp" ];
            };
            restartTriggers = [
              "/etc/wg_netmanager/network.yaml"
              "/etc/wg_netmanager/peer.yaml"
            ];
          })
        ])
        ++ (concatLists (
          mapAttrsToList (
            otherNodeName: value:
            optional (value.tunnel && networkCluster.${otherNodeName}.config.publicIp != null) (
              nameValuePair "wireguard-tunnel-client-${otherNodeName}" {
                enable = true;
                wantedBy = [ "multi-user.target" ];
                before = [ "multi-user.target" ];
                path = with pkgs; [
                  busybox
                  openssh
                ];
                serviceConfig = {
                  Type = "simple";
                  Restart = "always";
                  RestartSec = "5s";
                  ExecStart = "${pkgs.udp2raw}/bin/udp2raw --fix-gro -k qMQ9rUOA --raw-mode faketcp --cipher-mode xor --auth-mode simple -c -l127.0.0.1:${
                    toString (wireguardCluster.${otherNodeName}.config.index + 40000)
                  } -r ${networkCluster.${otherNodeName}.config.publicIp}:${
                    toString (wireguard.config.index + 40000)
                  }";
                };
              }
            )
          ) wireguard.peers
        ))
        ++ (concatLists (
          mapAttrsToList (
            otherNodeName: value:
            optional (value.tunnel && network.config.publicIp != null) (
              nameValuePair "wireguard-tunnel-server-${otherNodeName}" {
                enable = true;
                wantedBy = [ "multi-user.target" ];
                before = [ "multi-user.target" ];
                path = with pkgs; [
                  busybox
                  openssh
                ];
                serviceConfig = {
                  Type = "simple";
                  Restart = "always";
                  RestartSec = "5s";
                  ExecStart = "${pkgs.udp2raw}/bin/udp2raw --fix-gro -k qMQ9rUOA --raw-mode faketcp --cipher-mode xor --auth-mode simple -s -l0.0.0.0:${
                    toString (wireguardCluster.${otherNodeName}.config.index + 40000)
                  } -r 127.0.0.1:${builtins.toString (wireguard.config.port)}";
                };
              }
            )
          ) wireguard.peers
        ))
      )
    );
  };
}
