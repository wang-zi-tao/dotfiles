{
  pkgs,
  config,
  lib,
  ...
}:
let
  nodeConfig = config.cluster.nodes."${config.cluster.nodeName}";
  networkConfig = config.cluster.network.edges.${config.cluster.nodeName}.config;
in
{
  config = lib.mkIf nodeConfig.RustDeskServer.enable {
    virtualisation.oci-containers.containers.rustdesk-hbbs = {
      image = "rustdesk/rustdesk-server";
      cmd = [
        "hbbs"
        "-r"
        "${networkConfig.publicIp}:21117"
      ];
      volumes = [ "rustdesk:/root" ];
      ports = [
        "21115:21115"
        "21116:21116"
        "21116:21116/udp"
        "21118:21118"
      ];
      extraOptions = [ "--network=host" ];
    };
    virtualisation.oci-containers.containers.rustdesk-hbbr = {
      image = "rustdesk/rustdesk-server";
      cmd = [ "hbbr" ];
      volumes = [ "rustdesk:/root" ];
      ports = [
        "1117:21117"
        "21119:21119"
      ];
      extraOptions = [ "--network=host" ];
    };
    virtualisation.docker = {
      enable = true;
      enableOnBoot = true;
    };
    networking.firewall.allowedUDPPorts = [ 21116 ];
    networking.firewall.allowedTCPPorts = [
      21115
      21116
      21117
      21118
      21119
    ];
  };
}
