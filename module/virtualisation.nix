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
    boot.initrd.kernelModules = [
      "vfio_pci"
      "vfio"
      "vfio_iommu_type1"
      "vfio_virqfd"
    ];
    boot.kernelParams = [
      "intel_iommu=on"
      "amd_iommu=on"
      "elevator=deadline"
    ];
    boot.extraModprobeConfig = ''
      softdep nouveau       pre: vfio-pci
      softdep snd_hda_intel pre: vfio-pci
      softdep xhci_pci      pre: vfio-pci

      options vfio-pci ids=8086:a7a0
    '';
    virtualisation = {
      lxd = {
        package = pkgs.lxd;
        recommendedSysctlSettings = true;
      };
      lxc.lxcfs.enable = false;
      libvirtd = {
        enable = true;
        # qemu.ovmf.enable = true;
        # qemu.ovmf.packages = [ pkgs.OVMFFull ];
        # qemu.swtpm.enable = true;
        onBoot = "ignore";
        onShutdown = "shutdown";
        extraConfig = ''
          listen_tls = 0
          listen_tcp = 1
          tcp_port = "16509" 
          listen_addr = "0.0.0.0"
          auth_tcp = "none"
        '';
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
    systemd.tmpfiles.rules = [
      "f /dev/shm/looking-glass 0660 wangzi kvm -"
    ];
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
    environment.etc."qemu/vhost-user".source = "${pkgs.qemu}/share/qemu/vhost-user";
    environment.systemPackages = with pkgs; [ looking-glass-client virtiofsd podman-compose qemu virt-manager virt-viewer rdesktop ];
    systemd.services.libvirtd = with pkgs; {
      path = [ virtiofsd swtpm-tpm2 virglrenderer ];
      environment.LD_LIBRARY_PATH = "${virglrenderer}/lib";
    };
    networking.firewall.trustedInterfaces = [ "virbr0" ];
    networking.dhcpcd.denyInterfaces = [ "eno1" ];

    services.samba-wsdd.enable = true;
    services.samba = {
      enable = true;
      securityType = "user";
      extraConfig = ''
        workgroup = WORKGROUP
        server string = 192.168.122.1
        netbios name = ${config.networking.hostName}
        #use sendfile = yes
        #max protocol = smb2
        # note: localhost is the ipv6 localhost ::1
        hosts allow = 192.168.122. 127.0.0.1 localhost
        hosts deny = 0.0.0.0/0
        guest account = wangzi
        map to guest = bad user
        follow symlinks = yes
        wide links = yes
        allow insecure wide links = yes
      '';
      shares = {
        wangzi-home = {
          path = "/home/wangzi";
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "yes";
          "create mask" = "0644";
          "directory mask" = "0755";
          "follow symlinks" = "yes";
          "wide links" = "yes";
          "acl allow execute always" = "yes";
        };
        nix-store = {
          path = "/nix/store";
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "yes";
          "create mask" = "0644";
          "directory mask" = "0755";
          "follow symlinks" = "yes";
          "wide links" = "yes";
        };
      };
    };
  };
}
