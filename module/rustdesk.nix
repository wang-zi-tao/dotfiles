{
  pkgs,
  config,
  lib,
  ...
}:
let
  nodeConfig = config.cluster.nodes."${config.cluster.nodeName}";
  networkConfig = config.cluster.network.edges.${config.cluster.nodeName}.config;
  wireguardCluster = config.cluster.wireguard.edges;
in
{
  config = lib.mkMerge [
    (lib.mkIf nodeConfig.RustDeskServer.enable {
      services.rustdesk-server = {
        enable = true;
        openFirewall = true;
      };
    })
  ];
}
