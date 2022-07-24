{ config, pkgs, lib, ... }: {
  networking = {
    #useDHCP = true;
    hostName = "wangzi-nuc";
    networkmanager = {
      enable = true;
    };
    nat.enable = true;
    interfaces = {
      eno1 = {
        ipv4.addresses = [{
          address = "192.168.32.1";
          prefixLength = 24;
        }];
        # useDHCP = false;
      };
      lan = {
        ipv4.addresses = [{
          address = "192.168.32.1";
          prefixLength = 24;
        }];
      };
    };
    vlans = {
      lan = {
        id = 20;
        interface = "eno1";
      };
    };
  };
  systemd.services.NetworkManager-wait-online.enable = false;
  services.dhcpd4 = {
    enable = true;
    interfaces = [ "eno1" ];
    extraConfig = ''
      ddns-update-style none;
      #option subnet-mask         255.255.255.0;
      one-lease-per-client true;
      subnet 192.168.32.0 netmask 255.255.255.0 {
        range 192.168.32.128 192.168.32.254;
        authoritative;
        # Allows clients to request up to a week (although they won't)
        # max-lease-time              "604800";
        # By default a lease will expire in 24 hours.
        # default-lease-time          "86400";
        option subnet-mask          255.255.255.0;
        option broadcast-address    192.168.32.255;
        option routers              192.168.32.1;
      }
    '';
    machines = [
      {
        ipAddress = "192.168.32.128";
        hostName = "wangzi-pc";
        ethernetAddress = "b0:25:aa:2f:3c:7d";
      }
    ];
  };
}
