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
  client = nameValuePair "seaweedfs-mount-${nodeConfig.hostname}"
    {
      enable = true;
      description = "mount seaweedfs";
      wantedBy = [ "multi-user.target" ];
      before = [ "multi-user.target" ];
      path = with pkgs; [ busybox seaweedfs fuse ];
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = "2s";
        ExecStartPre = "${pkgs.busybox}/bin/mkdir -p ${weed.client.mount}/${nodeConfig.hostname} ${weed.client.path}/${nodeConfig.hostname}";
        ExecStart =
          ''${pkgs.seaweedfs}/bin/weed mount \
            -filer=${nodeConfig.wireguard.clusterIp}:302 \
            -dir=${weed.client.mount}/${nodeConfig.hostname} \
            -dirAutoCreate \
            -cacheDir=${weed.client.path}/${nodeConfig.hostname} \
            -cacheCapacityMB=${builtins.toString weed.client.size} \
            -collection=${nodeConfig.hostname}
          '';
        ExecStop = "${pkgs.util-linux}/bin/umount ${weed.client.mount} -l";
      };
    };
  nas-client =
    map
      (nas:
        let nasNode = config.cluster.nodes."${nas}";
        in
        nameValuePair "seaweedfs-mount-${nasNode.hostname}" {
          enable = true;
          description = "mount seaweedfs from ${nasNode.hostname}";
          wantedBy = [ "multi-user.target" ];
          before = [ "multi-user.target" ];
          path = with pkgs; [ busybox seaweedfs fuse ];
          serviceConfig = {
            Type = "simple";
            Restart = "always";
            RestartSec = "2s";
            ExecStartPre = "${pkgs.busybox}/bin/mkdir -p ${weed.client.mount}/${nasNode.hostname} ${weed.client.path}/${nasNode.hostname}";
            ExecStart =
              ''${pkgs.seaweedfs}/bin/weed mount \
              -filer=${nasNode.hostname}.wg:302 \
              -dir=${weed.client.mount}/${nasNode.hostname} \
              -dirAutoCreate \
              -cacheDir=${weed.client.path}/${nasNode.hostname} \
              -cacheCapacityMB=${builtins.toString weed.client.size} \
              -collection=${nasNode.hostname} \
              -filer.path=/${nasNode.hostname}
            '';
            ExecStop = "${pkgs.util-linux}/bin/umount ${weed.client.mount}/${nasNode.hostname} -l";
          };
        })
      weed.nas.attach;
  collections-client =
    (map
      (collection:
        nameValuePair "seaweedfs-mount-${collection.name}" {
          enable = true;
          description = "mount seaweedfs from ${collection.name}";
          wantedBy = [ "multi-user.target" ];
          before = [ "multi-user.target" ];
          path = with pkgs; [ busybox seaweedfs fuse ];
          serviceConfig = {
            Type = "simple";
            Restart = "always";
            RestartSec = "2s";
            ExecStartPre = "${pkgs.busybox}/bin/mkdir -p ${weed.client.mount}/${collection.name} ${weed.client.path}/${collection.name}";
            ExecStart =
              ''${pkgs.seaweedfs}/bin/weed mount \
            -filer=${builtins.concatStringsSep "," (
if (any (client: client == nodeConfig.hostname) collection.clients)
then["${nodeConfig.wireguard.clusterIp}:302"]
++(map (server: server+".wg:302")
    (lib.lists.remove nodeConfig.hostname collection.clients))
else (map (server: server+".wg:302")
    collections.clients)) } \
            -dir=${weed.client.mount}/${collection.name} \
            -dirAutoCreate \
            -cacheDir=${weed.client.path}/${collection.name} \
            -cacheCapacityMB=${builtins.toString weed.client.size} \
            -collection=${collection.name} \
            -filer.path=/${collection.name}
          '';
            ExecStop = "${pkgs.util-linux}/bin/umount ${weed.client.mount}/${collection.name} -l";
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
                  -a ${sync.from}.wg:302 \
                  -a.collection=${collection.name} \
                  -a.path=/${collection.name} \
                  -b ${sync.to}.wg:302 \
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
    // (listToAttrs (lib.lists.optional weed.client.enable client))
    // (listToAttrs nas-client)
    // (listToAttrs collections-client)
    // (listToAttrs collections-sync);
}
