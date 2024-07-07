{
  pkgs,
  lib,
  config,
  ...
}:
let
  nodeConfig = config.cluster.nodes."${config.cluster.nodeName}";
  networkConfig = config.cluster.network.edges.${config.cluster.nodeName}.config;
in
{
  config = lib.mkIf nodeConfig.NextCloudServer.enable {
    services.nextcloud = {
      enable = true;
      package = pkgs.nextcloud28;
      hostName = networkConfig.publicIp;
      caching.redis = true;
      appstoreEnable = true;
      enableImagemagick = true;
      config.adminuser = "wang-zi-tao";
      config.adminpassFile = config.sops.secrets."nextcloud/admin_password".path;
      autoUpdateApps.enable = true;
      https = true;
    };
    sops.secrets."nextcloud/admin_password" = {
      owner = "nextcloud";
      mode = "0500";
      restartUnits = [ "phpfpm.service" ];
    };
    services.caddy = lib.optionalAttrs nodeConfig.NextCloudServer.enable {
      enable = true;
      virtualHosts = {
        "https://${builtins.toString networkConfig.publicIp}" = {
          extraConfig = ''
            respond / 404
            respond /favicon.ico 404
            reverse_proxy http://localhost:80
          '';
        };
      };
    };
    networking.firewall.allowedUDPPorts = [ 443 ];
    networking.firewall.allowedTCPPorts = [
      80
      443
    ];
  };
}
