{ pkgs, config, lib, ... }:
let nodeConfig = config.cluster.nodes."${config.cluster.nodeName}";
in {
  networking = {
    firewall.enable = false;
    hosts = (builtins.listToAttrs (builtins.map ({ hostname, publicIp, ... }: {
      name = publicIp;
      value = [ hostname ];
    }) (builtins.filter ({ publicIp, ... }: publicIp != null)
      (builtins.attrValues config.cluster.nodes)))) // (builtins.listToAttrs
        (builtins.map ({ hostname, wireguard, ... }: {
          name = wireguard.clusterIp;
          value = [ "${hostname}.wg" ];
        }) (builtins.filter ({ wireguard, ... }: wireguard.enable)
          (builtins.attrValues config.cluster.nodes)))) // {
            "127.0.0.1" = [ config.networking.hostName ];
          };
  };
  sops.secrets."wireguard/private-key" =
    lib.mkIf nodeConfig.wireguard.enable { };
  networking.wireguard = lib.mkIf nodeConfig.wireguard.enable {
    enable = true;
    interfaces = {
      wg0 = {
        ips = [ nodeConfig.wireguard.clusterIp ];
        listenPort = nodeConfig.wireguard.port;
        privateKeyFile = config.sops.secrets."wireguard/private-key".path;
        peers = map (node:
          let w = node.wireguard;
          in {
            persistentKeepalive = 25;
            allowedIPs = [ "192.168.16.0/24" ];
            publicKey = w.publicKey;
            endpoint = if node.publicIp != null then
              "${node.publicIp}:${builtins.toString w.port}"
            else
              null;
          }) (builtins.filter (node: node.wireguard.enable)
            (builtins.attrValues config.cluster.nodes));
      };
    };
  };
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
    "net.ipv4.conf.all.proxy_arp" = 1;
  };
  environment.systemPackages = with pkgs; [
    wireguard
    wireguard-tools
    seaweedfs
  ];
}
