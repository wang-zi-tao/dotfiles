{ pkgs, config, lib, ... }: {
  imports = [ ../secret/secret.nix ];
  networking = {
    firewall.enable = false;
    hosts = (builtins.listToAttrs (builtins.map ({ host, ip }: {
      name = ip;
      value = [ host ("${host}.wangzicloud.cn") ];
    }) (config.secret.network-configs))) // {
      "127.0.0.1" = [
        config.networking.hostName
        "${config.networking.hostName}.wangzicloud.cn"
      ];
    };
  };
  networking.wireguard = {
    enable = true;
    interfaces = {
      wg0 = {
        ips = [
          "192.168.16.${
            builtins.toString config.secret.wireguard-config.index
          }/24"
        ];
        listenPort = config.secret.wireguard-config.port;
        privateKey = config.secret.wireguard-config.private-key;
        peers = map (w: {
          allowedIPs = [ "192.168.16.0/24" ];
          publicKey = w.public-key;
          endpoint = if w.endpoint-ip != null then
            "${w.endpoint-ip}:${builtins.toString w.port}"
          else
            null;
        }) config.secret.wireguard-configs;
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
