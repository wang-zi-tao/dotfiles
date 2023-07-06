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
        range 192.168.32.192 192.168.32.254;
        authoritative;
        # Allows clients to request up to a week (although they won't)
        # max-lease-time              "604800";
        # By default a lease will expire in 24 hours.
        # default-lease-time          "86400";
        option subnet-mask          255.255.255.0;
        option broadcast-address    192.168.32.255;
      }
    '';
    machines = [
      {
        ipAddress = "192.168.32.129";
        hostName = "wangzi-asus";
        ethernetAddress = "08:bf:b8:c1:6e:ee";
      }
      {
        ipAddress = "192.168.32.128";
        hostName = "wangzi-pc";
        ethernetAddress = "b0:25:aa:2f:3c:7d";
      }
    ];
  };
  networking.firewall.trustedInterfaces = [ "eno1" "wlp3s0" "lan@eno1" ];
  systemd.network.networks.main.routes.wg.routeConfig = {
    Gateway = "192.168.16.2";
  };
  nix.settings.substituters = [
    # "ssh://root@aliyun-hk.wg"
    # "ssh://root@aliyun-ecs.wg"
  ];
  services.samba-wsdd.enable = true;
  services.samba = {
    enable = true;
    securityType = "user";
    extraConfig = ''
      workgroup = WORKGROUP
      server string = 192.168.16.11
      netbios name = wangzi-pc
      #use sendfile = yes
      #max protocol = smb2
      # note: localhost is the ipv6 localhost ::1
      hosts allow = 192.168.122. 127.0.0.1 localhost
      hosts deny = 0.0.0.0/0
      guest account = wangzi
      map to guest = bad user
      follow symlinks = yes
      wide links = yes
      allow insecure wide links = yes
    '';
    shares = {
      wangzi-home = {
        path = "/home/wangzi";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        "create mask" = "0644";
        "directory mask" = "0755";
        "follow symlinks" = "yes";
        "wide links" = "yes";
      };
      nix-store = {
        path = "/nix/store";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        "create mask" = "0644";
        "directory mask" = "0755";
        "follow symlinks" = "yes";
        "wide links" = "yes";
      };
    };
  };
}
