{ pkgs, lib, config, ... }:
let
  nodeConfig = config.cluster.nodes."${config.cluster.nodeName}";
in
{
  config = lib.mkIf nodeConfig.NextCloudServer.enable {
    services.nextcloud = {
      enable = true;
      hostName = nodeConfig.publicIp;
      caching.redis = true;
      appstoreEnable = true;
      enableImagemagick = true;
      config.adminuser = "wang-zi-tao";
      config.adminpassFile = config.sops.secrets."nextcloud/admin_password".path;
      autoUpdateApps.enable = true;
    };
    sops.secrets."nextcloud/admin_password" = {
      owner = "nextcloud";
      mode = "0700";
      restartUnits = [ "phpfpm.service" ];
    };
  };
}
