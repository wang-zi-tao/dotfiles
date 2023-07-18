{ config, pkgs, lib, ... }: {
  config = lib.mkIf config.cluster.nodeConfig.virtualisation.enable {
    boot.kernelParams = [
      "elevator=deadline"
    ];
    virtualisation = {
      lxd = {
        package = pkgs.lxd;
        recommendedSysctlSettings = true;
      };
      lxc.lxcfs.enable = false;
      libvirtd = {
        enable = true;
        qemu.ovmf.enable = true;
        qemu.ovmf.packages = [ pkgs.OVMFFull ];
        qemu.swtpm.enable = true;
        onBoot = "ignore";
        onShutdown = "shutdown";
      };
      # spiceUSBRedirection.enable = true;
      kvmgt.enable = true;
      waydroid.enable = false;
      podman = {
        enable = true;
        defaultNetwork.settings.dns_enabled = true;
      };
    };
    hardware.ksm.enable = true;
    systemd.services.balloond = {
      enable = true;
      wantedBy = [ "libvirtd.service" ];
      environment = { RUST_LOG = "info"; };
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = "5s";
        ExecStart = "${pkgs.balloond}/bin/balloond -r 1600 -p 0.5 -d 2 -h 2";
        # ExecStart = "${pkgs.balloond}/bin/balloond -r 1600 -p 0.8 -d 1 -h 4";
      };
    };
    environment.etc."qemu/vhost-user".source = "${pkgs.qemu_full}/share/qemu/vhost-user";
    environment.systemPackages = with pkgs; [ virtiofsd podman-compose qemu virt-manager virt-viewer rdesktop ];
    systemd.services.libvirtd = with pkgs; {
      path = [ virtiofsd swtpm-tpm2 virglrenderer ];
      environment.LD_LIBRARY_PATH = "${virglrenderer}/lib";
    };
    boot.initrd.kernelModules = [
      "vfio_pci"
      "vfio"
      "vfio_iommu_type1"
      "vfio_virqfd"
    ];
    networking.firewall.trustedInterfaces = [ "virbr0" ];
  };
}
