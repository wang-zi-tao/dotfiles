{ config, pkgs, lib, ... }: {
  networking = {
    #useDHCP = true;
    hostName = "wangzi-pc";
    networkmanager = {
      enable = true;
    };
    nat.forwardPorts."qemu-rdp" = {
      sourcePort = 3399;
      loopbackIPs = "192.168.122.238:3389";
    };
  };
  systemd.services.NetworkManager-wait-online.enable = false;
  networking.firewall.trustedInterfaces = [ "enp3s0" ];
}
