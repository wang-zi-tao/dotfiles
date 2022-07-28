{ config, pkgs, lib, ... }:
let
  nodeConfig = config.cluster.nodes."${config.cluster.nodeName}";
in
{
  config = lib.mkIf config.cluster.nodeConfig.binary-cache.enable {
    services.nix-serve = {
      enable = true;
      openFirewall = true;
    };
  };
}
