{
  pkgs-template,
  nixpkgs,
  home-manager,
  sops-nix,
  ...
}@inputs:
let
  hostname = "aliyun-hk";
  system = "x86_64-linux";
  pkgs = pkgs-template system;
in
nixpkgs.lib.nixosSystem {
  inherit pkgs system;
  specialArgs = inputs;
  modules = [
    sops-nix.nixosModules.sops
    home-manager.nixosModules.home-manager
    (
      { pkgs, config, ... }:
      let
        networkConfig = config.cluster.network.edges.${config.cluster.nodeName}.config;
      in
      {
        imports = [ ../module/cluster.nix ];
        sops.defaultSopsFile = ../secrets/aliyun-hk.yaml;
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
        boot.initrd.kernelModules = [
          "virtio_balloon"
          "virtio_console"
          "virtio_rng"
        ];

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
        swapDevices = [
          {
            device = "/swapfile";
            size = 1024 * 2;
          }
        ];
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
        sops.secrets."script" = {
          mode = "0500";
          restartUnits = [ "run-secrets-scripts" ];
        };
        networking.firewall.allowedTCPPortRanges = [
          {
            from = 8880;
            to = 8888;
          }
        ];
        services.caddy = {
          enable = true;
          virtualHosts = {
            "http://aliyun-hk.wg:11434" = {
              extraConfig = ''
                reverse_proxy http://wangzi-pc.wg:11434
              '';
            };
          };
        };
      }
    )
  ];
}
