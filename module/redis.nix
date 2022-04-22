{ pkgs, config, lib, ... }: {
  config = lib.mkIf config.cluster.nodeConfig.redis.enable {
    services.redis = {
      enable = true;
      port = 6379;
      bind = null;
      vmOverCommit = true;
      logfile = "/var/lib/redis/redis.log";
      unixSocket = "/var/lib/redis/redis.sock";
      settings = { maxmemory = 512 * 1024 * 1024; };
    };
  };
}
