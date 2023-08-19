{ config, pkgs, lib, ... }:
let
  cfg = config.vm;
in
with builtins;{
  options = with lib;{
    vm.guest-reserved = mkOption { type = types.int; default = 400; };
    vm.host-reserved = mkOption { type = types.int; default = 800; };
    vm.guest-reserved-percent = mkOption { type = types.float; default = 0.0; };
  };
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
        ExecStart = "${pkgs.balloond}/bin/balloond -r ${toString cfg.guest-reserved} -R ${toString cfg.host-reserved} -p ${toString cfg.guest-reserved-percent} -d 1 -h 4";
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
