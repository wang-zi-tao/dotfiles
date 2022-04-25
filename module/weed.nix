{ pkgs, config, lib, ... }:
with lib;
with builtins;
let
  nodeConfig = config.cluster.nodeConfig;
  weed = nodeConfig.weed;
  collections = config.cluster.collections;
  server = nameValuePair "seaweedfs-server"
    {
      enable = true;
      description = "seaweedfs server";
      wantedBy = [ "multi-user.target" ];
      before = [ "multi-user.target" ];
      path = with pkgs; [ busybox seaweedfs fuse ];
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = "5s";
        ExecStartPre = "${pkgs.busybox}/bin/mkdir -p ${weed.server.path}/weed ${weed.server.path}/master ${weed.server.path}/volume";
        ExecStart =
          ''${pkgs.seaweedfs}/bin/weed server \
            -resumeState \
            -ip=${nodeConfig.wireguard.clusterIp} \
            -dir=${weed.server.path}/weed \
            -master.port=301 \
            -master.dir=${weed.server.path}/master \
            -master.defaultReplication=000 \
            -master.volumePreallocate=false \
            -volume.dir.idx=${weed.server.path}/volume \
            -filer \
            -filer.port=302 \
            -volume \
            -volume.index=leveldb \
            -volume.port=303 \
            -volume.max=256 \
            -volume.publicUrl=${nodeConfig.wireguard.clusterIp}:303
          '';
      };
    };
  webdav = nameValuePair "seaweedfs-webdav-${nodeConfig.hostname}"
    {
      enable = true;
      description = "mount seaweedfs";
      wantedBy = [ "multi-user.target" ];
      before = [ "multi-user.target" ];
      path = with pkgs; [ busybox seaweedfs fuse ];
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = "5s";
        ExecStartPre = "${pkgs.busybox}/bin/mkdir -p ${weed.client.path}/webdav-${nodeConfig.hostname}";
        ExecStart =
          ''${pkgs.seaweedfs}/bin/weed webdav \
            -filer=${nodeConfig.wireguard.clusterIp}:302 \
            -cacheDir=${weed.client.path}/webdav-${nodeConfig.hostname} \
            -cacheCapacityMB=${builtins.toString weed.client.size} \
            -port=304
          '';
      };
    };
  client =
    {
      enable = true;
      description = "mount seaweedfs";
      wantedBy = [ "multi-user.target" ];
      before = [ "multi-user.target" ];
      type = "fuse";
      what = "${pkgs.seaweedfs}/bin/weed#fuse";
      where = "${weed.client.mount}/${nodeConfig.hostname}";
      options = "x-systemd.mount-timeout=infinity,retry=65535,"
        + "filer='${nodeConfig.wireguard.clusterIp}:302',"
        + "dirAutoCreate,"
        + "cacheDir=${weed.client.path}/${nodeConfig.hostname},"
        + "cacheCapacityMB=${builtins.toString weed.client.size}";
      mountConfig = {
        TimeoutSec = "0";
      };
    };
  nas-client =
    map
      (nas:
        let nasNode = config.cluster.nodes."${nas}";
        in
        {
          enable = true;
          description = "mount seaweedfs from ${nasNode.hostname}";
          wantedBy = [ "multi-user.target" ];
          before = [ "multi-user.target" ];
          type = "fuse";
          what = "${pkgs.seaweedfs}/bin/weed#fuse";
          where = "${weed.client.mount}/${nasNode.hostname}";
          options = "x-systemd.mount-timeout=infinity,retry=65535,_netdev,"
            + "filer='${nasNode.hostname}.wg1:302',"
            + "dirAutoCreate,"
            + "cacheDir=${weed.client.path}/${nasNode.hostname},"
            + "cacheCapacityMB=${builtins.toString weed.client.size}";
          mountConfig = {
            TimeoutSec = "0";
          };
        })
      weed.nas.attach;
  collections-client =
    (map
      (collection:
        {
          enable = true;
          description = "mount seaweedfs from ${collection.name}";
          wantedBy = [ "multi-user.target" ];
          before = [ "multi-user.target" ];
          type = "fuse";
          what = "${pkgs.seaweedfs}/bin/weed#fuse";
          where = "${weed.client.mount}/${collection.name}";
          options = "x-systemd.mount-timeout=infinity,retry=65535,_netdev,"
            + "filer='${builtins.concatStringsSep "," (
if (any (client: client == nodeConfig.hostname) collection.clients)
then["${nodeConfig.hostname}:302"]
++(map (server: server+".wg1:302")
    (lib.lists.remove nodeConfig.hostname collection.clients))
else (map (server: server+".wg1:302")
    collections.clients)) }',"
            + "dirAutoCreate,"
            + "cacheDir=${weed.client.path}/${collection.name},"
            + "cacheCapacityMB=${builtins.toString weed.client.size}";
          mountConfig = {
            TimeoutSec = "0";
          };
        }
      )
      (filter
        (collection:
          any (client: client == nodeConfig.hostname) collection.clients)
        (attrValues collections)));
  collections-sync =
    (concatLists
      (map
        (collection: (map
          (sync:
            nameValuePair "seaweedfs-sync-${collection.name}-${sync.from}-${sync.to}" {
              enable = true;
              description = "sync seaweedfs in ${collection.name} from ${sync.from} to ${sync.to}";
              wantedBy = [ "multi-user.target" ];
              before = [ "multi-user.target" ];
              path = with pkgs; [ busybox seaweedfs fuse ];
              serviceConfig = {
                Type = "simple";
                Restart = "always";
                RestartSec = "2s";
                ExecStart = ''${pkgs.seaweedfs}/bin/weed filer.sync \
                  -a ${sync.from}.wg1:302 \
                  -a.collection=${collection.name} \
                  -a.path=/${collection.name} \
                  -b ${sync.to}.wg1:302 \
                  -b.collection=${collection.name} \
                  -b.path=/${collection.name}
                '';
              };
            }
          )
          (filter (sync: sync.from == nodeConfig.hostname) collection.sync)))
        (filter
          (collection:
            any (server: server == nodeConfig.hostname) collection.servers)
          (attrValues collections)))
    );
in
{
  systemd.services =
    (listToAttrs (lib.lists.optional weed.server.enable server))
    // (listToAttrs collections-sync);
  systemd.mounts =
    (lib.lists.optional weed.client.enable client)
    ++ nas-client
    ++ collections-client;
}
