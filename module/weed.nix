{ pkgs, config, lib, ... }:
with builtins; with lib;with lib.types; with lib.attrsets; let
  seaweedfsGraph = pkgs.graphType {
    nodeOption = { nodeName, nodeConfig, ... }: {
      server = {
        enable = mkOption { type = bool; default = true; };
        path = mkOption { type = path; default = "/mnt/weed/server"; };
      };
      client = {
        enable = mkOption { type = bool; default = true; };
        path = mkOption { type = path; default = "/tmp/weed-cache"; };
        size = mkOption { type = int; default = 8192; };
        mount = mkOption { type = path; default = "/mnt/weed/mount"; };
      };
    };
    directed = true;
    edgeOption = { nodeName, nodeConfig, name, config, ... }:
      let edgeName = name; in
      {
        syncDirs = mkOption {
          type = attrsOf
            (submodule {
              options = {
                ipA = mkOption { type = str; default = "${nodeName}.wg"; };
                ipB = mkOption { type = str; default = "${edgeName}.wg"; };
              };
            });
          default = { };
        };
        mountDirs = mkOption {
          type = attrsOf
            (submodule {
              options = {
                cacheSize = mkOption {
                  type = int;
                  default = nodeConfig.config.client.size;
                };
                ip = mkOption { type = str; default = "${edgeName}.wg"; };
              };
            });
          default = { };
        };
      };
  };
  hostname = config.networking.hostName;
  enabled = hasAttr hostname config.cluster.seaweedfs.edges;
  mkIfEnbled = mkIf enabled;
in
{
  options = {
    cluster.seaweedfs = mkOption {
      type = seaweedfsGraph.type;
      default = { };
    };
  };
  imports = [
    { cluster.seaweedfs = seaweedfsGraph.function config.cluster.seaweedfs; }
    ./wireguard.nix
  ];
  config = {
    environment.systemPackages = with pkgs; mkIfEnbled [
      seaweedfs
    ];
    system.activationScripts.weed-self = mkIfEnbled ''
      ln -sfn /mnt/weed/mount/${hostname} /mnt/weed/mount/self
    '';
    systemd.services =
      optionalAttrs enabled (
        let
          seaweedfs = config.cluster.seaweedfs.edges.${hostname};
          wireguardCluster = config.cluster.wireguard.edges;
          wireguardConfig = wireguardCluster.${hostname}.config;
          weed = seaweedfs.config;
        in
        listToAttrs ((
          optional seaweedfs.config.server.enable (nameValuePair "seaweedfs-server" {
            enable = true;
            description = "seaweedfs server";
            wantedBy = [ "multi-user.target" ];
            before = [ "multi-user.target" ];
            after = [ "seaweedfs-master.service" ];
            serviceConfig = {
              Type = "simple";
              Restart = "always";
              RestartSec = "5s";
              LimitNOFILE = 500000;
              LimitNPROC = 500000;
              ExecStartPre = "${pkgs.busybox}/bin/mkdir -p ${weed.server.path}/weed ${weed.server.path}/master ${weed.server.path}/volume";
              ExecStart =
                ''${pkgs.seaweedfs}/bin/weed server \
            -resumeState \
            -ip=${wireguardConfig.clusterIp} \
            -ip.bind=0.0.0.0 \
            -dir=${weed.server.path}/weed \
            -master=true \
            -master.port=301 \
            -master.dir=${weed.server.path}/master \
            -master.defaultReplication=000 \
            -master.volumePreallocate=false \
            -master.electionTimeout=1s \
            -volume.dir.idx=${weed.server.path}/volume \
            -filer \
            -filer.port=302 \
            -volume \
            -volume.index=leveldb \
            -volume.port=303 \
            -volume.max=256 \
            -volume.publicUrl=${wireguardConfig.clusterIp}:303 \
            -metricsPort=9101
          '';
            };
          })
        )
        ++ (optional seaweedfs.config.client.enable (nameValuePair "seaweedfs-mount-${hostname}" {
          enable = true;
          description = "mount seaweedfs";
          wantedBy = [ "multi-user.target" ];
          before = [ "multi-user.target" ];
          path = with pkgs; [ fuse ];
          serviceConfig = {
            Type = "simple";
            Restart = "always";
            RestartSec = "2s";
            LimitNOFILE = 500000;
            ExecStartPre = "${pkgs.busybox}/bin/mkdir -p ${weed.client.mount}/${hostname} ${weed.client.path}/${hostname}";
            ExecStart =
              ''${pkgs.seaweedfs}/bin/weed mount \
            -filer=${hostname}:302 \
            -dir=${weed.client.mount}/${hostname} \
            -dirAutoCreate \
            -cacheDir=${weed.client.path}/${hostname} \
            -cacheCapacityMB=${builtins.toString weed.client.size}
          '';
          };
          postStop = "${pkgs.util-linux}/bin/umount ${weed.client.mount}/${hostname} -l";
        }))
        ++ (concatLists
          (mapAttrsToList
            (remoteHostname: link: (mapAttrsToList
              (dir: mountConfig: nameValuePair "seaweedfs-mount-${remoteHostname}" {
                enable = true;
                description = "mount seaweedfs from ${remoteHostname}";
                wantedBy = [ "multi-user.target" ];
                before = [ "multi-user.target" ];
                path = with pkgs; [ fuse ];
                serviceConfig = {
                  Type = "simple";
                  Restart = "always";
                  RestartSec = "2s";
                  LimitNOFILE = 500000;
                  ExecStartPre = "${pkgs.busybox}/bin/mkdir -p ${weed.client.mount}/${remoteHostname} ${weed.client.path}/${remoteHostname}";
                  ExecStart = ''${pkgs.seaweedfs}/bin/weed mount \
              -filer=${mountConfig.ip}:302 \
              -dir=${weed.client.mount}/${remoteHostname} \
              -dirAutoCreate \
              -cacheDir=${weed.client.path}/${remoteHostname} \
              -cacheCapacityMB=${builtins.toString mountConfig.cacheSize} \
              -filer.path=/${remoteHostname}
            '';
                };
                postStop = "${pkgs.util-linux}/bin/umount ${weed.client.mount}/${remoteHostname} -l";
              })
              link.mountDirs) ++ (mapAttrsToList
              (dir: syncConfig: nameValuePair "seaweedfs-sync-${dir}-${remoteHostname}-${hostname}" {
                enable = true;
                description = "sync seaweedfs in ${dir} from ${remoteHostname} to ${hostname}";
                wantedBy = [ "multi-user.target" ];
                before = [ "multi-user.target" ];
                path = with pkgs; [ fuse ];
                serviceConfig = {
                  Type = "simple";
                  Restart = "always";
                  RestartSec = "2s";
                  LimitNOFILE = 500000;
                  ExecStart = ''${pkgs.seaweedfs}/bin/weed filer.sync \
                  -a ${syncConfig.ipA}:302 \
                  -a.path=/${dir} \
                  -b ${syncConfig.ipB}:302 \
                  -b.path=/${dir} \
                  # -a.debug -b.debug
                '';
                };
              })
              link.syncDirs))
            (optionalAttrs (hasAttr hostname wireguardCluster) seaweedfs.to)
          )))
      );
  };
}
