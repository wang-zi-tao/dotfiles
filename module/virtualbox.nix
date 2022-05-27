{ config, pkgs, lib, ... }: {
  config = lib.mkIf config.cluster.nodeConfig.virtualisation.enable
    {
      virtualisation = {
        virtualbox = {
          host = {
            enable = true;
            # enableExtensionPack = true;
          };
        };
        lxd = {
          enable = true;
          package = pkgs.unstable.lxd;
          recommendedSysctlSettings = true;
        };
        lxc.lxcfs.enable = true;
        libvirtd.enable = true;
        kvmgt.enable = true;
      };
      services.qemuGuest.enable = true;
    };
}
