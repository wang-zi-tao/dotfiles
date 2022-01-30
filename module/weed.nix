{ pkgs, config, lib, ... }: {
  config = lib.mkIf config.cluster.nodeConfig.weedServer.enable {
    containers = {
      seaweedfs = {
        autoStart = true;
        nixpkgs = pkgs.path;
        forwardPorts = builtins.concatLists (builtins.map (port: [
          {
            containerPort = port;
            hostPort = port;
            protocal = "tcp";
          }
          {
            containerPort = port;
            hostPort = port;
            protocal = "udp";
          }
        ]) [ 8080 18080 8888 18888 9333 19333 ]);
        bindMounts = {
          "/data" = {
            hostPath = "/srv/seaweedfs";
            isReadOnly = false;
          };
        };
        config = { config, pkgs', ... }: {
          boot.isContainer = true;
          systemd.services.seaweedfs = {
            enable = true;
            description = "seaweedfs server";
            wantedBy = [ "multi-user.target" ];
            serviceConfig = {
              Type = "simple";
              Restart = "always";
              RestartSec = "5s";
              ExecStart =
                "${pkgs.seaweedfs}/bin/weed server -dir=/data/weed -master.dir=/data/master -master.defaultReplication=000 -master.volumePreallocate=false -volume.dir.idx=/data/volume -filer -volume.publicUrl=192.168.16.1:8080";
            };
          };
        };
      };
    };
  };
}
