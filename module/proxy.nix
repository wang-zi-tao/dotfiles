{ pkgs, config, lib, ... }:
let nodeConfig = config.cluster.nodeConfig;
in
{
  config = lib.mkIf config.cluster.nodeConfig.proxy.enable {
    services.privoxy = {
      enable = true;
      settings.listen-address = "0.0.0.0:8889";
    };
  };
}
