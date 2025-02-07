{ pkgs, config, lib, ... }:
let
  nodeConfig = config.cluster.nodes."${config.cluster.nodeName}";
  networkConfig =
    config.cluster.network.edges.${config.cluster.nodeName}.config;
  wireguardCluster = config.cluster.wireguard.edges;
in {
  config = lib.mkIf nodeConfig.XpraProxy.enable {
    services.caddy = {
      enable = true;
      virtualHosts = {
        "https://${builtins.toString networkConfig.publicIp}:7800" = {
          extraConfig = ''
            tls internal
            handle_path /xpra/server/wangzi-nuc/* {
                rewrite * {path}
                reverse_proxy http://wangzi-nuc.wg:7000
            }
            handle_path /xpra/server/wangzi-asus/* {
                rewrite * {path}
                reverse_proxy http://wangzi-asus.wg:7000
            }
            handle_path /xpra/client/* {
                rewrite * {path}
                root * ${pkgs.xpra-html5}/install/
                file_server
            }
          '';
        };
      };
    };
    systemd.services.xpra-proxy = {
      serviceConfig = {
        Type = "simple";
        Restart = "always";
      };
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      script = ''
        ${pkgs.xpra}/bin/xpra proxy :1000 --tcp-auth=allow --bind-tcp=0.0.0.0:7000 --no-daemon
      '';
    };

    networking.nat.forwardPorts = builtins.concatLists (lib.mapAttrsToList
      (remoteHostName: cfg:
        let remotewireguard = wireguardCluster.${remoteHostName}.config;
        in lib.optionals cfg.guiClient.enable [
          {
            destination = "${remotewireguard.clusterIp}:7000";
            proto = "tcp";
            sourcePort = 7000 + remotewireguard.index;
          }
          {
            destination = "${remotewireguard.clusterIp}:6000";
            proto = "tcp";
            sourcePort = 6000 + remotewireguard.index;
          }
        ]) config.cluster.nodes);

    networking.firewall.allowedTCPPorts = [ 7000 7800 ];
  };
}
