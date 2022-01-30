{ pkgs, nixpkgs, home-manager, sops-nix, ... }:
let hostname = "huawei-ecs";
in nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    ../module/cluster.nix
    home-manager.nixosModules.home-manager
    sops-nix.nixosModules.sops
    ({ pkgs, lib, config, ... }: {
      sops.defaultSopsFile = ../secrets/huawei-ecs.yaml;
      sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
      sops.age.keyFile = "/var/lib/sops-nix/key.txt";
      sops.age.generateKey = true;
      networking.hostName = hostname;
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
        [ "virtio_balloon" "virtio_console" "virtio_rng" ];

      boot.initrd.postDeviceCommands = ''
        # Set the system time from the hardware clock to work around a
        # bug in qemu-kvm > 1.5.2 (where the VM clock is initialised
        # to the *boot time* of the host).
        hwclock -s
      '';
      boot.loader.grub.device = "/dev/vda";
      fileSystems."/" = {
        device = "/dev/vda1";
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
        firewall.enable = false;
        dhcpcd.enable = true;
      };
    })
  ];
  inherit pkgs;
}