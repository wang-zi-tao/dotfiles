{
  pkgs,
  config,
  lib,
  ...
}:
{
  config = lib.mkIf config.cluster.nodeConfig.redis.enable {
    services.redis = {
      vmOverCommit = true;
      servers.default = {
        enable = true;
        bind = null;
        port = 6379;
        unixSocket = "/var/lib/redis-default/redis.sock";
        settings = {
          maxmemory = 512 * 1024 * 1024;
        };
      };
    };
    networking.firewall.allowedTCPPorts = [ 6379 ];
  };
}
