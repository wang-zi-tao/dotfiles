{ pkgs, config, lib, ... }:
let nodeConfig = config.cluster.nodeConfig;
in
{
  config = lib.mkIf config.cluster.nodeConfig.proxy.enable { };
}
