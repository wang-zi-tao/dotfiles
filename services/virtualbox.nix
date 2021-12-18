{ config, pkgs, lib, ... }: {
  virtualisation.virtualbox = {
    host = {
      enable = true;
      enableHardening = true;
      enableExtensionPack = true;
    };
  };
}
