{ config, pkgs, lib, ... }: {
  config = lib.mkIf config.cluster.nodeConfig.virtualisation.enable {
    virtualisation = {
      lxd = {
        enable = false;
        package = pkgs.lxd;
        recommendedSysctlSettings = true;
      };
      lxc.lxcfs.enable = true;
      libvirtd = {
        enable = true;
        qemu.ovmf.enable = true;
        qemu.ovmf.packages = [
          pkgs.OVMF
        ];
        qemu.package = pkgs.qemu_full;
        qemu.swtpm.enable = true;
      };
      kvmgt.enable = true;
      # waydroid.enable = true;
    };
    systemd.services.balloond = {
      enable = true;
      wantedBy = [ "libvirtd.service" ];
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = "5s";
        ExecStart = "${pkgs.balloond}/bin/balloond -unix /run/libvirt/libvirt-sock -freeAllowance=600000 -interval=5s -verbose";
      };
    };
    environment.etc."qemu/vhost-user".source = "${pkgs.qemu_full}/share/qemu/vhost-user";
    environment.systemPackages = with pkgs; [ qemu virt-manager virt-viewer rdesktop ];
    systemd.services.libvirtd = with pkgs; {
      path = [ virtiofsd swtpm-tpm2 virglrenderer ];
      environment.LD_LIBRARY_PATH = "${virglrenderer}/lib";
    };
    networking.firewall.trustedInterfaces = [ "virbr0" ];
  };
}
