{ pkgs, lib, config, ... }:
let
  nodeConfig = config.cluster.nodes."${config.cluster.nodeName}";
in
{
  config = lib.mkIf nodeConfig.NextCloudServer.enable {
    services.nextcloud = {
      enable = true;
      package = pkgs.nextcloud24;
      hostName = nodeConfig.publicIp;
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
      mode = "0700";
      restartUnits = [ "phpfpm.service" ];
    };
    services.caddy = lib.optionalAttrs nodeConfig.NextCloudServer.enable {
      enable = true;
      virtualHosts = {
        "https://${builtins.toString nodeConfig.publicIp}" = {
          extraConfig = ''
            reverse_proxy http://localhost:80
          '';
        };
      };
    };
  };
}

