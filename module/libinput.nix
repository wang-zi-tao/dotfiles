{ config, pkgs, lib, ... }: {
  config = lib.mkIf config.cluster.nodeConfig.guiClient.enable {
    services.xserver.libinput = {
      enable = true;
      mouse = { accelSpeed = "1.0"; };
      touchpad = {
        naturalScrolling = true;
        horizontalScrolling = true;
        accelSpeed = "1.0";
      };
    };
  };
}
