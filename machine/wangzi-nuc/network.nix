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
  services.kea.dhcp4 = {
    enable = true;
    settings = {
      interfaces-config.interfaces = [ "eno1" ];
      control-socket = {
        socket-type = "unix";
        socket-name = "/run/kea/kea4-ctrl-socket";
      };
      lease-database = {
        name = "/var/lib/kea/dhcp4.leases";
        persist = true;
        type = "memfile";
      };
      max-valid-lifetime = 7200;
      subnet4 = [{
        pools = [{
          pool = "192.168.32.192 - 192.168.32.254";
        }];
        subnet = "192.168.32.0/24";
        reservations = [
          {
            hostname = "wangzi-asus";
            hw-address = "08:bf:b8:c1:6e:ee";
            ip-address = "192.168.32.129";
          }
          {
            hostname = "wangzi-pc";
            hw-address = "b0:25:aa:2f:3c:7d";
            ip-address = "192.168.32.128";
          }
        ];
      }];
    };
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
      server string = 192.168.16.12
      netbios name = wangzi-nuc
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
