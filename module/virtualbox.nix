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
      systemd.services.libvirtd = with pkgs; {
        path = [ virtiofsd swtpm-tpm2 virglrenderer ];
        environment.LD_LIBRARY_PATH = "${virglrenderer}/lib";
      };
    };
}
