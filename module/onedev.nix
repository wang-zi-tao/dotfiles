{ pkgs, config, lib, ... }:
let nodeConfig = config.cluster.nodes."${config.cluster.nodeName}";
in
{
  config = lib.mkIf nodeConfig.OnedevServer.enable {
    virtualisation.oci-containers.containers.onedev = {
      image = "1dev/server";
      volumes = [
        "onedev:/opt/onedev"
        "/var/run/docker.sock:/var/run/docker.sock"
      ];
      ports = [ "6610:6610" "6611:6611" ];
    };
    services.caddy = lib.optionalAttrs nodeConfig.OnedevServer.enable {
      enable = true;
      virtualHosts = {
        "https://${builtins.toString nodeConfig.publicIp}:6613" = {
          extraConfig = ''
            reverse_proxy http://localhost:6610
          '';
        };
      };
    };
    networking.firewall.allowedUDPPorts = [ 6613 ];
    networking.firewall.allowedTCPPorts = [ 6610 6611 6613 ];
  };
}
