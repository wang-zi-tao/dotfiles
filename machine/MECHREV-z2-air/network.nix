{ config, pkgs, lib, ... }: {
  networking = {
    #useDHCP = true;
    hostName = "wangzi-pc";
    networkmanager = { enable = true; };
  };
  systemd.services.NetworkManager-wait-online.enable = false;
}

