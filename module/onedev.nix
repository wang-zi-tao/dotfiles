{ pkgs, config, lib, ... }: {
  config = lib.mkIf config.cluster.nodeConfig.OnedevServer.enable {
    virtualisation.oci-containers.containers.onedev = {
      image = "1dev/server";
      volumes = [
        "onedev:/opt/onedev"
        "/var/run/docker.sock:/var/run/docker.sock"
      ];
      ports = [ "6610:6610" "6611:6611" ];
    };
  };
}
