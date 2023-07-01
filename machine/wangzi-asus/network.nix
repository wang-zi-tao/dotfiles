{ config, pkgs, lib, ... }: {
  systemd.services.NetworkManager-wait-online.enable = false;
  networking = {
    networkmanager = {
      enable = true;
    };
  };
  networking.defaultGateway.interface = "wlo1";
}
