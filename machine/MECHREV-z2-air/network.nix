{ config, pkgs, lib, ... }: {
  networking = {
    #useDHCP = true;
    hostName = "wangzi-pc";
    networkmanager = { enable = true; };
    proxy.default = "http://127.0.0.1:8889";
  };
  systemd.services.NetworkManager-wait-online.enable = false;
}
