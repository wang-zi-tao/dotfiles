{ config, pkgs, lib, ... }: {
  config = lib.mkIf config.cluster.nodeConfig.develop.enable {
    virtualisation.virtualbox = {
      host = {
        enable = true;
        # enableExtensionPack = true;
      };
    };
  };
}
