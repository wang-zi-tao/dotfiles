{
  pkgs,
  config,
  lib,
  ...
}:
with builtins;
with lib;
with lib.types;
with lib.attrsets;
let
  nfsGraph = pkgs.graphType {
    nodeOption =
      { nodeName, nodeConfig, ... }:
      {
        server = {
          enable = mkOption {
            type = bool;
            default = true;
          };
          exports = mkOption {
            type = lines;
            default = "";
          };
        };
        client = {
          enable = mkOption {
            type = bool;
            default = true;
          };
          path = mkOption {
            type = path;
            default = "/tmp/weed-cache";
          };
          size = mkOption {
            type = int;
            default = 8192;
          };
          mount = mkOption {
            type = path;
            default = "/mnt/weed/mount";
          };
        };
      };
    directed = true;
    edgeOption =
      {
        nodeName,
        nodeConfig,
        name,
        config,
        ...
      }:
      let
        edgeName = name;
      in
      {
        mountDirs = mkOption {
          type = attrsOf (submodule {
            options = {
              ip = mkOption {
                type = str;
                default = "${edgeName}.wg";
              };
            };
          });
          default = { };
        };
      };
  };
  hostname = config.networking.hostName;
  enabled = hasAttr hostname config.cluster.nfs.edges;
  mkIfEnabled = mkIf enabled;
  nfsCluster = config.cluster.nfs.edges;
  nfsConfig = nfsCluster.${hostname}.config;
in
{
  options = {
    cluster.nfs = mkOption {
      inherit (nfsGraph) type;
      default = { };
    };
  };
  imports = [ { cluster.nfs = nfsGraph.function config.cluster.nfs; } ];
  config = {
    services.nfs.server = mkIfEnabled {
      inherit (nfsConfig.server) enable;
      inherit (nfsConfig.server) exports;
    };
  };
}
