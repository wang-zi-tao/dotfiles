{ config, pkgs, lib, ... }: {
  networking = {
    #useDHCP = true;
    hostName = "wangzi-pc";
    networkmanager = { enable = true; };
    proxy.default = "http://192.168.16.2:8889";
  };
  systemd.services.NetworkManager-wait-online.enable = false;
}
