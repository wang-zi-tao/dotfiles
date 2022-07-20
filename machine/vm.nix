{ pkgs, nixpkgs, home-manager, sops-nix, ... }:
let
  hostname = "default";
  system = "x86_64-linux";
in
nixpkgs.lib.nixosSystem {
  modules = [
    ../module/cluster.nix
    home-manager.nixosModules.home-manager
    sops-nix.nixosModules.sops
    ({ pkgs, lib, config, ... }: {
      networking.hostName = hostname;
      sops.defaultSopsFile = "/";
      cluster.nodes."${default}" = {
        wireguard.enable = false;
        guiClient.enable = true;
        guiServer.enable = true;
        develop.enable = true;
      };
      boot.initrd.availableKernelModules = [
        "virtio_net"
        "virtio_pci"
        "virtio_mmio"
        "virtio_blk"
        "virtio_scsi"
        "9p"
        "9pnet_virtio"
      ];
      boot.initrd.kernelModules =
        [ "virtio_balloon" "virtio_console" "virtio_rng" "nvme" ];

      boot.initrd.postDeviceCommands = ''
        # Set the system time from the hardware clock to work around a
        # bug in qemu-kvm > 1.5.2 (where the VM clock is initialised
        # to the *boot time* of the host).
        hwclock -s
      '';
      boot.loader.grub.device = "/dev/vda";
      fileSystems."/" = {
        device = "/dev/sda1";
        fsType = "ext4";
      };
      swapDevices = [{
        device = "/swapfile";
        size = (1024 * 2);
      }];
      systemd.services = {
        create-swapfile = {
          serviceConfig.Type = "oneshot";
          wantedBy = [ "swap-swapfile.swap" ];
          script = ''
            ${pkgs.coreutils}/bin/truncate -s 0 /swapfile
          '';
        };
      };
      networking = {
        dhcpcd.enable = true;
      };
      boot.cleanTmpDir = true;
      zramSwap.enable = true;
    })
  ];
  pkgs = pkgs system;
  inherit system;
}
