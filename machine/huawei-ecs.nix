{ pkgs, nixpkgs, home-manager, ... }:
let hostname = "huawei-ecs";
in nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    ../module/nixos.nix
    ../module/terminal.nix
    ../services/sshd.nix
    ../services/container.nix
    ../module/network.nix
    home-manager.nixosModules.home-manager
    ({
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.users.root = { ... }: {
        imports = [ ../home-manager/terminal/terminal.nix ];
        inherit hostname;
      };
    })
    ({ pkgs, lib, config, ... }: {
      containers = {
        seaweedfs = {
          autoStart = true;
          nixpkgs = pkgs.path;
          forwardPorts = builtins.concatLists (builtins.map (port: [
            {
              containerPort = port;
              hostPort = port;
              protocal = "tcp";
            }
            {
              containerPort = port;
              hostPort = port;
              protocal = "udp";
            }
          ]) [ 8080 18080 8888 18888 9333 19333 ]);
          bindMounts = {
            "/data" = {
              hostPath = "/srv/seaweedfs";
              isReadOnly = false;
            };
          };
          config = { config, pkgs', ... }: {
            boot.isContainer = true;
            system.stateVersion = "21.05";
            systemd.services.seaweedfs = {
              enable = true;
              description = "seaweedfs server";
              wantedBy = [ "multi-user.target" ];
              serviceConfig = {
                Type = "simple";
                Restart = "always";
                RestartSec = "5s";
                ExecStart =
                  "${pkgs.seaweedfs}/bin/weed server -dir=/data/weed -master.dir=/data/master -master.defaultReplication=000 -master.volumePreallocate=false -volume.dir.idx=/data/volume -filer -volume.publicUrl=192.168.16.1:8080";
              };
            };
          };
        };
        redis = {
          autoStart = true;
          forwardPorts = [{
            containerPort = 6379;
            hostPort = 6379;
            protocal = "tcp";
          }];
          bindMounts = {
            "/var/lib/redis" = {
              hostPath = "/srv/redis";
              isReadOnly = false;
            };
          };
          config = { config, pkgs, ... }: {
            boot.isContainer = true;
            system.stateVersion = "21.05";
            services.redis = {
              enable = true;
              port = 6379;
              bind = null;
              vmOverCommit = true;
              logfile = "/var/lib/redis/redis.log";
              unixSocket = "/var/lib/redis/redis.sock";
              settings = { maxmemory = 1 * 1024 * 1024 * 1024; };
            };
          };
        };
        mysql = {
          autoStart = true;
          forwardPorts = [{
            containerPort = 3306;
            hostPort = 3306;
            protocal = "tcp";
          }];
          bindMounts = {
            "/var/lib/mysql" = {
              hostPath = "/srv/mysql";
              isReadOnly = false;
            };
            "/var/backup/mysql" = {
              hostPath = "/srv/backup-mysql";
              isReadOnly = false;
            };
            "/run/mysqld" = {
              hostPath = "/srv/mysql";
              isReadOnly = false;
            };
          };
          config = { config, pkgs, ... }: {
            boot.isContainer = true;
            system.stateVersion = "21.05";
            services.mysql = {
              enable = true;
              # Okq4ikoZVsxif1Q55GZauhASJQtEA1mS
              package = pkgs.mysql57;
              settings = {
                client = { default-character-set = "utf8mb4"; };
                mysql = { default-character-set = "utf8mb4"; };
                mysqld = {
                  character-set-client-handshake = "FALSE";
                  character-set-server = "utf8mb4";
                  collation-server = "utf8mb4_unicode_ci";
                  init_connect = "'SET NAMES utf8mb4'";
                };
              };
              ensureDatabases = [ "nextcloud" "mydb" "onedev" ];
              ensureUsers = [
                {
                  name = "nextcloud"; # jn4TjfrGOyKmjyWn
                  ensurePermissions = { "nextcloud.*" = "ALL PRIVILEGES"; };
                }
                {
                  name = "onedev"; # d1Ycb85VU6Q1iT41
                  ensurePermissions = { "onedev.*" = "ALL PRIVILEGES"; };
                }
                {
                  name = "backup";
                  ensurePermissions = { "*.*" = "SELECT, LOCK TABLES"; };
                }
              ];
            };
            services.mysqlBackup = {
              enable = true;
              databases = [ "nextcloud" "mydb" "onedev" ];
            };
          };
        };
      };
      users.users.root = { shell = pkgs.zsh; };
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
