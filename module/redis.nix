{ pkgs, config, lib, ... }: {
  config = lib.mkIf config.cluster.nodeConfig.redis.enable {
    containers = {
      redis = {
        autoStart = true;
        forwardPorts = [{
          containerPort = 6379;
          hostPort = 6379;
          protocal = "tcp";
        }];
        bindMounts = {
          "/var/lib/redis" = {
            hostPath = "/srv/redis";
            isReadOnly = false;
          };
        };
        config = { config, pkgs, ... }: {
          boot.isContainer = true;
          services.redis = {
            enable = true;
            port = 6379;
            bind = null;
            vmOverCommit = true;
            logfile = "/var/lib/redis/redis.log";
            unixSocket = "/var/lib/redis/redis.sock";
            settings = { maxmemory = 1 * 1024 * 1024 * 1024; };
          };
        };
      };
    };
  };
}
