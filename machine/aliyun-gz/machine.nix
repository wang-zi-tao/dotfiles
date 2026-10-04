{
  pkgs-template,
  nixpkgs,
  modules,
  ...
}@inputs:
let
  hostname = "aliyun-gz";
  system = "x86_64-linux";
  pkgs = pkgs-template system;
in
nixpkgs.lib.nixosSystem {
  inherit pkgs system;
  specialArgs = inputs;
  modules = modules ++ [
    (
      { pkgs, config, ... }:
      let
        networkConfig = config.cluster.network.edges.${config.cluster.nodeName}.config;
      in
      {
        imports = [ ../../module/cluster.nix ];
        sops.defaultSopsFile = ../../secrets/aliyun-gz.yaml;
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

        boot.loader.grub.device = "/dev/vda";
        services.rpcbind.enable = true;
        fileSystems."/mnt/aliyun_nas" = {
          device = "12a71948580-yts86.cn-hongkong.nas.aliyuncs.com:/";
          fsType = "nfs";
          options = [
            "x-systemd.automount"
            "noauto"
            "vers=3"
            "nolock"
            "proto=tcp"
            "rsize=1048576"
            "wsize=1048576"
            "hard"
            "timeo=600"
            "retrans=2"
            "noresvport"
          ];
        };
        services.nextcloud.datadir = "/mnt/aliyun_nas/nextcloud";
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
                tls internal
              '';
            };
          };
        };

        boot.loader = {
          systemd-boot.enable = true;
          systemd-boot.configurationLimit = 5;
          efi = {
            canTouchEfiVariables = true;
            efiSysMountPoint = "/boot/efi";
          };
          timeout = 1;
        };
        hardware.facter = {
          enable = true;
          reportPath = ./facter.json;
        };
        disko.enableConfig = true;
        disko.devices = {
          disk = {
            main = {
              device = "/dev/vda";
              type = "disk";
              content = {
                type = "gpt";
                partitions = {
                  root = {
                    size = "100%";
                    content = {
                      type = "filesystem";
                      format = "ext4";
                      mountpoint = "/";
                    };
                  };
                  swap = {
                    size = "8G";
                    content = {
                      type = "swap";
                    };
                  };
                  ESP = {
                    type = "EF00";
                    size = "500M";
                    content = {
                      type = "filesystem";
                      format = "vfat";
                      mountpoint = "/boot/efi";
                      mountOptions = [ "umask=0077" ];
                    };
                  };
                };
              };
            };
          };
        };
      }
    )
  ];
}
