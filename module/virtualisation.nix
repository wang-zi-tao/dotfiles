{ config, pkgs, lib, ... }: {
  config = lib.mkIf config.cluster.nodeConfig.virtualisation.enable {
    virtualisation = {
      lxd = {
        enable = true;
        package = pkgs.lxd;
        recommendedSysctlSettings = true;
      };
      lxc.lxcfs.enable = true;
      libvirtd = {
        enable = true;
        qemu.ovmf.enable = true;
        qemu.ovmf.package = pkgs.OVMFFull;
        qemu.package = pkgs.qemu_full;
        qemu.swtpm.enable = true;
      };
      kvmgt.enable = true;
      waydroid.enable = true;
    };
    systemd.services.balloond = {
      enable = true;
      wantedBy = [ "libvirtd.service" ];
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = "5s";
        ExecStart = "${pkgs.balloond}/bin/balloond -unix /run/libvirt/libvirt-sock -interval=5s -verbose";
      };
    };
    environment.etc."qemu/vhost-user".source = "${pkgs.qemu_full}/share/qemu/vhost-user";
    /* users.users.virtlyst.group = "virtlyst"; */
    /* users.groups.virtlyst = { }; */
    /* services.virtlyst.enable = true; */
    /* services.virtlyst.adminPassword = "wfn5l5VpRK1W5Q9f"; */
    environment.systemPackages = with pkgs; [ qemu virt-manager virt-viewer rdesktop ];
    systemd.services.libvirtd = with pkgs; {
      path = [ virtiofsd swtpm-tpm2 virglrenderer ];
      environment.LD_LIBRARY_PATH = "${virglrenderer}/lib";
    };
    networking.firewall.trustedInterfaces = [ "virbr0" ];
  };
}
