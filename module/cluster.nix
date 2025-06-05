{ pkgs, config, lib, ... }:
let
  cfg = config.cluster;
  nodeConfig = cfg.nodes."${cfg.nodeName}";
in with lib;
with builtins; {
  options = with types; {
    cluster = {
      ssh.publicKeySops = mkOption { type = path; };
      nodeName = mkOption {
        type = str;
        default = config.networking.hostName;
      };
      nodeConfig = mkOption {
        type = attrs;
        default = nodeConfig;
      };
      nodes = mkOption {
        description = "nodes";
        type = attrsOf (submodule ({ name, config, ... }: {
          options = {
            arch = mkOption {
              type = str;
              default = "x86_64-linux";
            };
            hostname = mkOption {
              type = str;
              default = name;
            };
            inVM = mkOption {
              type = bool;
              default = false;
            };
            inContainer = mkOption {
              type = bool;
              default = false;
            };
            users = mkOption {
              type = attrsOf path;
              default = { root = ../home-manager/profiles/root.nix; };
            };
            sshd.enable = mkOption {
              type = bool;
              default = true;
            };
            prometheus = {
              server = mkOption {
                type = bool;
                default = false;
              };
              nodeExporter = mkOption {
                type = bool;
                default = true;
              };
            };
            NextCloudServer.enable = mkOption {
              type = bool;
              default = false;
            };
            MySQL.enable = mkOption {
              type = bool;
              default = false;
            };
            redis.enable = mkOption {
              type = bool;
              default = false;
            };
            OnedevServer.enable = mkOption {
              type = bool;
              default = false;
            };
            RustDeskServer.enable = mkOption {
              type = bool;
              default = false;
            };
            XpraProxy.enable = mkOption {
              type = bool;
              default = false;
            };
            CodeServer.enable = mkOption {
              type = bool;
              default = false;
            };
            webssh.enable = mkOption {
              type = bool;
              default = false;
            };
            binary-cache.enable = mkOption {
              type = bool;
              default = false;
            };
            wayland.enable = mkOption {
              type = bool;
              default = false;
            };
            guiServer.enable = mkOption {
              type = bool;
              default = false;
            };
            guiClient.enable = mkOption {
              type = bool;
              default = false;
            };
            shell.enable = mkOption {
              type = bool;
              default = true;
            };
            develop.enable = mkOption {
              type = bool;
              default = true;
            };
            container.enable = mkOption {
              type = bool;
              default = false;
            };
            virtualisation.enable = mkOption {
              type = bool;
              default = false;
            };
            cockpitServer.enable = mkOption {
              type = bool;
              default = false;
            };
            atuin.enable = mkOption {
              type = bool;
              default = false;
            };
            buildNode.enable = mkOption {
              type = bool;
              default = false;
            };
            ollama = {
              enable = mkOption {
                type = bool;
                default = false;
              };
            };
            k3s = {
                enable = mkOption {
                    type = bool;
                    default = false;
                };
                kind = mkOption {
                    type = str;
                    default = "agent";
                };
                taint = mkOption {
                  type = nullOr str;
                  default = null;
                };
            };
          };
        }));
      };
    };
  };
  imports = [
    ./nixos.nix
    ./terminal.nix
    ./gui.nix
    ./libinput.nix
    ./network.nix
    ./wireguard.nix
    ./boot.nix
    ./user.nix

    ./display-manager.nix
    ./sshd.nix
    ./container.nix
    ./virtualisation.nix
    ./develop.nix
    ./atuin.nix

    ./binary-cache.nix
    ./mySQL.nix
    ./nextcloud.nix
    ./onedev.nix
    ./code.nix
    ./webssh.nix
    ./redis.nix
    ./weed.nix
    ./nfs.nix
    ./prometheus.nix
    ./rustdesk.nix
    ./xpra.nix
    ./ai.nix
  ];
  config = {
    boot.isContainer = nodeConfig.inContainer;
    cluster = {
      network = {
        nodes = {
          wangzi-pc.config = { localIp = "192.168.1.145"; };
          wangzi-nuc.config = { localIp = "192.168.32.1"; };
          wangzi-asus.config = { localIp = "192.168.32.129"; };
          huawei-ecs.config = { publicIp = "139.9.235.87"; };
          aliyun-hk.config = { publicIp = "47.83.14.140"; };
          aliyun-ecs.config = { publicIp = "116.62.23.116"; };
          lxd = { };
          nova9 = { };
          M6 = { };
        };
        peersBlackList = [ ];
      };
      wireguard = {
        nodes = {
          wangzi-pc.config = {
            index = 11;
            port = 16538;
            publicKey = "jh1sHn85aq4Hkb3/s8AeQwfzpQ5PtNU7p0dqyeUOTWQ=";
            gateway = "aliyun-hk";
          };
          wangzi-pc.peers.wangzi-nuc = { };
          wangzi-asus.peers.wangzi-nuc = { };
          # wangzi-asus.peers.wangzi-nuc = { };
          wangzi-pc.peers.aliyun-hk = { tunnel = true; };
          wangzi-nuc.config = {
            index = 12;
            port = 16538;
            publicKey = "Vk2vw8TbtI7GgktauuppvfhKAAxyEeNC8+/nxt10t1s=";
            gateway = "aliyun-hk";
          };
          wangzi-asus.config = {
            index = 13;
            port = 16538;
            publicKey = "1O+by7g8ZKgNEy+SmWHRcX6QsIQvq4bjxBY9rm4v6CA=";
            gateway = "aliyun-hk";
          };
          wangzi-asus.peers.aliyun-hk = { tunnel = true; };
          wangzi-nuc.peers.aliyun-hk = { tunnel = true; };
          huawei-ecs.config = {
            index = 1;
            port = 30806;
            publicKey = "IfOIL06xMk/4r2QQvhNbLUFDLh97sXdHO3WIyOP4Sj4=";
          };
          aliyun-hk.config = {
            index = 2;
            port = 21176;
            publicKey = "kY4n/K6zHjRNq/5f1yId2156zyfO/cVAwQddasPqjE8=";
            iptables.enable = true;
            gatewayServer = true;
            nat = "ens5";
          };
          aliyun-ecs.config = {
            index = 3;
            port = 39690;
            publicKey = "riRxoNrTrJYZQyjAtiMk/aqiZ8ZoLnzw2e9maV18Tlk=";
          };
          nova9.config = {
            index = 21;
            port = 53555;
            publicKey = "Xy1ofNbrxk2Gm8Q29hzCxtu5djfAvv9EFg6yOkbkBhw=";
            gateway = "aliyun-hk";
          };
          M6.config = {
            index = 22;
            port = 53555;
            publicKey = "2/GD91ji4EU1eT4Jbdv4j4cLjK70xwaz/54lqrqhD1I=";
            gateway = "aliyun-hk";
          };
        };
        peersWhiteList = [ "aliyun-hk" "aliyun-ecs" "huawei-ecs" ];
      };
      keys.wireguard.sharedKeySops = ../secrets/public-key.yaml;
      seaweedfs = {
        nodes = {
          wangzi-pc.config = { client.size = 4 * 1024; };
          wangzi-pc.to.aliyun-hk = { syncDirs = { "Cluster" = { }; }; };
          wangzi-pc.to.wangzi-nuc = {
            mountDirs = {
              "wangzi-nuc" = {
                # ip = "192.168.32.1";
                cacheSize = 4096;
              };
            };
            syncDirs = { "wangzi" = { }; };
          };
          wangzi-asus.to.wangzi-nuc = {
            mountDirs = {
              "wangzi-nuc" = {
                ip = "192.168.32.1";
                cacheSize = 4096;
              };
            };
            syncDirs = {
              "wangzi" = {
                # ipA = "192.168.32.129";
                # ipB = "192.168.32.1";
              };
            };
          };
          wangzi-nuc.config = {
            server.path = "/srv/weed-server";
            client.size = 8 * 1024;
          };
          wangzi-nuc.to.aliyun-hk = { syncDirs = { "Cluster" = { }; }; };
          wangzi-asus.config = {
            server.path = "/srv/weed-server";
            client.size = 8 * 1024;
          };
          wangzi-asus.to.aliyun-hk = { syncDirs = { "Cluster" = { }; }; };
          huawei-ecs.config = { client.size = 1 * 1024; };
          huawei-ecs.to.aliyun-hk = { syncDirs = { "Cluster" = { }; }; };
          aliyun-hk.config = { client.size = 1 * 1024; };
          aliyun-ecs.config = { client.size = 1 * 1024; };
          aliyun-ecs.to.aliyun-hk = { syncDirs = { "Cluster" = { }; }; };
        };
      };
      ssh.publicKeySops = ../secrets/public-key.yaml;
      nodes = let
        desktop_config = {
          guiServer.enable = true;
          guiClient.enable = true;
          develop.enable = true;
          container.enable = true;
          virtualisation.enable = true;
          buildNode.enable = true;
          ollama.enable = true;
          k3s.enable = true;
        };
        server_config = { container.enable = true; };
      in {
        wangzi-pc = desktop_config // {
          users.wangzi = ../home-manager/profiles/wangzi-desktop.nix;
        };
        wangzi-nuc = desktop_config // {
          users.wangzi = ../home-manager/profiles/wangzi-desktop.nix;
          # localIp = "192.168.32.1";
          wayland.enable = true;
        };
        wangzi-asus = desktop_config // {
          users.wangzi = ../home-manager/profiles/wangzi-asus.nix;
          wayland.enable = true;
        };
        huawei-ecs = server_config // {
          MySQL.enable = true;
          webssh.enable = true;
          redis.enable = true;
          # weedServer.enable = true;
          inVM = true;
        };
        aliyun-hk = server_config // {
          NextCloudServer.enable = true;
          webssh.enable = true;
          OnedevServer.enable = true;
          RustDeskServer.enable = true;
          XpraProxy.enable = true;
          atuin.enable = true;
          cockpitServer.enable = true;
          prometheus.server = true;
          k3s.enable = true;
          k3s.kind = "server";
          k3s.taint = "master:NoSchedule";
          inVM = true;
        };
        aliyun-ecs = server_config // {
          webssh.enable = true;
          prometheus.server = true;
          # redis.enable = true;
          inVM = true;
        };
        lxd = { inContainer = true; };
        nova9 = { arch = "aarch64-linux"; };
        M6 = { arch = "aarch64-linux"; };
      };
      nfs = { nodes = { }; };
    };
  };
}
