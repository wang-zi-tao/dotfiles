{
  config,
  pkgs,
  lib,
  ...
}:
{
  systemd.services.NetworkManager-wait-online.enable = false;
  networking = {
    networkmanager = {
      enable = true;
    };
    nat = {
        enable = true;
        enableIPv6 = true;
        internalInterfaces = ["enp56s0"];
        externalInterface = "wlo1";
    };
    interfaces = {
      enp56s0 = {
        ipv4.addresses = [{
          address = "192.168.32.129";
          prefixLength = 24;
        }];
        # useDHCP = false;
      };
    };
  };
  services.kea.dhcp4 = {
    enable = true;
    settings = {
      interfaces-config.interfaces = [ "enp56s0" ];
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
        id = 1;
        pools = [{ pool = "192.168.32.192 - 192.168.32.254"; }];
        subnet = "192.168.32.0/24";
        reservations = [
          {
            hostname = "wangzi-pc";
            hw-address = "b0:25:aa:2f:3c:7d";
            ip-address = "192.168.32.128";
          }
        ];
      }];
    };
  };
  networking.firewall.allowedTCPPorts = [ 5900 ];
}
