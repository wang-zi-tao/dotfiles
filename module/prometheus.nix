{ pkgs, config, lib, ... }:
with builtins;
with lib;
let
  nodeConfig = config.cluster.nodeConfig;
  nodeList = (attrValues config.cluster.nodes);
in
{
  sops.secrets = optionalAttrs nodeConfig.prometheus.server {
    "prometheus/admin_password" = { };
  };
  services.grafana = mkIf nodeConfig.prometheus.server {
    enable = true;
    port = 9002;
    addr = "0.0.0.0";
    domain = "grafana.wangzicloud.cn";
    #security.adminPasswordFile = config.sops.secrets."prometheus/admin_password".path;
  };
  services.prometheus = {
    enable = nodeConfig.prometheus.server;
    port = 9001;
    scrapeConfigs = [
      {
        job_name = "${nodeConfig.hostname}";
        static_configs = [
          {
            targets = (map (node: "${node.hostname}.wg:9100") (filter (node: node.prometheus.nodeExporter) nodeList));
          }
          {
            targets = (map (node: "${node.hostname}.wg:9586") (filter (node: node.wireguard.enable) nodeList));
          }
        ];
      }
    ];
    exporters = {
      node = mkIf nodeConfig.prometheus.nodeExporter {
        enable = true;
        enabledCollectors = [ "systemd" ];
        listenAddress = nodeConfig.wireguard.clusterIp;
      };
      wireguard = mkIf nodeConfig.wireguard.enable {
        verbose = true;
        listenAddress = nodeConfig.wireguard.clusterIp;
      };
    };
  };
}
