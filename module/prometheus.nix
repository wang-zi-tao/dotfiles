{ pkgs, config, lib, ... }:
with builtins;
with lib;
let
  nodeConfig = config.cluster.nodeConfig;
  nodeList = (attrValues config.cluster.nodes);
  networkConfig = config.cluster.network.edges.${config.cluster.nodeName}.config;
  wireguardConfig = config.cluster.wireguard.edges.${config.cluster.nodeName}.config;
in
{
  imports = [ ./wireguard.nix ];
  config = {
    sops.secrets = optionalAttrs nodeConfig.prometheus.server {
      "prometheus/admin_password" = { };
    };
    services.grafana.settings = mkIf nodeConfig.prometheus.server {
      enable = true;
      port = 9002;
      addr = "0.0.0.0";
      domain = "grafana.wangzicloud.cn";
      #security.adminPasswordFile = config.sops.secrets."prometheus/admin_password".path;
    };
    services.caddy = lib.optionalAttrs nodeConfig.prometheus.server {
      enable = true;
      virtualHosts = {
        "https://${builtins.toString networkConfig.publicIp}:9003" = {
          extraConfig = ''
            reverse_proxy http://localhost:9002
          '';
        };
      };
    };
    services.prometheus = {
      enable = nodeConfig.prometheus.server;
      port = 9001;
      scrapeConfigs = (map
        (node: {
          job_name = "${node.hostname}";
          static_configs = [{
            targets = [ "${node.hostname}.wg:9100" ];
          }];
        })
        (filter (node: node.prometheus.nodeExporter) nodeList))
      ++ (map
        (node: {
          job_name = "${node.hostname}-wg";
          static_configs = [{
            targets = [ "${node.hostname}.wg:9586" ];
          }];
        })
        (filter (node: hasAttr node.hostname config.cluster.wireguard.edges) nodeList))
      ++ (map
        (node: {
          job_name = "${node.hostname}-weed";
          metrics_path = "/metrics";
          static_configs = [{
            targets = [ "${node.hostname}.wg:9101" ];
          }];
        })
        (filter (node: hasAttr node.hostname config.cluster.seaweedfs.edges) nodeList));
      exporters = {
        node = mkIf nodeConfig.prometheus.nodeExporter {
          enable = true;
          enabledCollectors = [ "systemd" ];
          listenAddress = wireguardConfig.clusterIp;
        };
        wireguard = mkIf nodeConfig.prometheus.nodeExporter {
          enable = true;
          listenAddress = wireguardConfig.clusterIp;
        };
      };
    };
    networking.firewall.allowedUDPPorts = [ 9003 ];
    networking.firewall.allowedTCPPorts = [ 9003 ];
  };
}
