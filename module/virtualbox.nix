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
          enable = false;
          package = pkgs.lxd;
          recommendedSysctlSettings = true;
        };
        lxc.lxcfs.enable = true;
        libvirtd.enable = true;
        kvmgt.enable = true;
      };
      environment.systemPackages = with pkgs; [ qemu virt-manager ];
      systemd.services.libvirtd.path = [ pkgs.unstable.virtiofsd pkgs.swtpm-tpm2 ];
    };
}
