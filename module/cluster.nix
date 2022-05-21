{ pkgs, config, lib, ... }:
let
  cfg = config.cluster;
  nodeConfig = cfg.nodes."${cfg.nodeName}";
in
with lib; with builtins;{
  options =
    with types; {
      cluster = {
        weedFilers = mkOption { type = listOf str; };
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
          type = attrsOf
            (submodule
              ({ name, config, ... }: {
                options = {
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
                    type = listOf str;
                    default = [ ];
                  };
                  publicIp = mkOption {
                    type = nullOr str;
                    default = null;
                  };
                  localIp = mkOption {
                    type = nullOr str;
                    default = null;
                  };
                  sshd.enable = mkOption {
                    type = bool;
                    default = true;
                  };
                  wireguard = {
                    enable = mkOption {
                      type = bool;
                      default = true;
                    };
                    index = mkOption { type = ints.u8; };
                    clusterIp = mkOption {
                      type = str;
                      default = "192.168.16.${toString config.wireguard.index}";
                    };
                    clusterIp2 = mkOption {
                      type = str;
                      default = "192.168.17.${toString config.wireguard.index}";
                    };
                    port = mkOption {
                      type = nullOr ints.u16;
                      default = null;
                    };
                    publicKey = mkOption { type = str; };
                    gateway = mkOption { type = nullOr str; default = null; };
                    gatewayServer = mkOption { type = bool; default = false; };
                    iptables.enable = mkOption { type = bool; default = false; };
                    tcp = mkOption { type = bool; default = false; };
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
                  CodeServer.enable = mkOption {
                    type = bool;
                    default = false;
                  };
                  weed = {
                    enable = mkOption {
                      type = bool;
                      default = false;
                    };
                    server = {
                      enable = mkOption {
                        type = bool;
                        default = false;
                      };
                      path = mkOption {
                        type = path;
                        default = "/mnt/weed/server";
                      };
                    };
                    client = {
                      enable = mkOption {
                        type = bool;
                        default = false;
                      };
                      path = mkOption {
                        type = path;
                        default = "/tmp/weed-cache";
                      };
                      size = mkOption {
                        type = int;
                        default = 8192;
                      };
                      mount = mkOption {
                        type = path;
                        default = "/mnt/weed/mount";
                      };
                    };
                    nas.attach = mkOption {
                      type = listOf str;
                      default = [ ];
                    };
                    nas.server = mkOption {
                      type = bool;
                      default = false;
                    };
                    nas.cacheSize = mkOption {
                      type = int;
                      default = 8192;
                    };
                    backup.attach = mkOption {
                      type = nullOr str;
                      default = null;
                    };
                    backup.server = mkOption {
                      type = bool;
                      default = false;
                    };
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
                    default = false;
                  };
                  container.enable = mkOption {
                    type = bool;
                    default = false;
                  };
                  proxy.enable = mkOption {
                    type = bool;
                    default = false;
                  };
                };
              }));
        };
        collections = mkOption {
          default = { };
          type = attrsOf
            (submodule ({ name, config, ... }: {
              options = {
                name = mkOption { type = str; default = name; };
                servers = mkOption { type = listOf str; };
                clients = mkOption { type = listOf str; };
                sync = mkOption
                  {
                    type = listOf
                      (submodule ({ config, ... }: {
                        options = {
                          from = mkOption { type = str; };
                          to = mkOption { type = str; };
                        };
                      }));
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
    ./boot.nix
    ./user.nix

    ./display-manager.nix
    ./sshd.nix
    ./container.nix
    ./virtualbox.nix

    ./proxy.nix
    ./mySQL.nix
    ./nextcloud.nix
    ./onedev.nix
    ./code.nix
    ./redis.nix
    ./weed.nix
    ./prometheus.nix
  ];
  config = {
    boot.isContainer = nodeConfig.inContainer;
    cluster = {
      ssh.publicKeySops = ../secrets/public-key.yaml;
      collections = {
        wangzi = {
          servers = [ "wangzi-nuc" "wangzi-pc" ];
          clients = [ "wangzi-nuc" "wangzi-pc" ];
          sync = [{ from = "wangzi-nuc"; to = "wangzi-pc"; }];
        };
        workspace = {
          servers = [ "wangzi-nuc" "wangzi-pc" "aliyun-hk" ];
          clients = [ "wangzi-nuc" "wangzi-pc" "aliyun-hk" ];
          sync = [
            { from = "wangzi-nuc"; to = "wangzi-pc"; }
            { from = "aliyun-hk"; to = "wangzi-pc"; }
            { from = "aliyun-hk"; to = "wangzi-nuc"; }
          ];
        };
      };
      nodes = {
        wangzi-pc = {
          users = [ "wangzi" ];
          localIp = "192.168.32.128";
          wireguard = {
            index = 11;
            port = 16538;
            publicKey = "jh1sHn85aq4Hkb3/s8AeQwfzpQ5PtNU7p0dqyeUOTWQ=";
            gateway = "aliyun-hk";
            tcp = true;
          };
          guiServer.enable = true;
          guiClient.enable = true;
          develop.enable = true;
          container.enable = true;
          CodeServer.enable = true;
          weed = {
            enable = true;
            server.enable = true;
            client.enable = true;
            client.size = 8 * 1024;
            nas = {
              cacheSize = 8192;
              attach = [ "wangzi-nuc" "aliyun-hk" "huawei-ecs" ];
            };
          };
        };
        wangzi-nuc = {
          users = [ "wangzi" ];
          localIp = "192.168.32.1";
          wireguard = {
            index = 12;
            port = 16538;
            publicKey = "Vk2vw8TbtI7GgktauuppvfhKAAxyEeNC8+/nxt10t1s=";
            gateway = "aliyun-hk";
            tcp = true;
          };
          wayland.enable = true;
          guiServer.enable = true;
          guiClient.enable = true;
          develop.enable = true;
          container.enable = true;
          CodeServer.enable = true;
          weed = {
            enable = true;
            server.enable = true;
            client.enable = true;
            client.size = 64 * 1024;
            nas = {
              server = true;
              cacheSize = 8192;
              attach = [ "aliyun-hk" "huawei-ecs" ];
            };
          };
        };
        huawei-ecs = {
          publicIp = "139.9.235.87";
          wireguard = {
            index = 1;
            port = 30806;
            publicKey = "IfOIL06xMk/4r2QQvhNbLUFDLh97sXdHO3WIyOP4Sj4=";
          };
          weed = {
            enable = true;
            server.enable = true;
            client.enable = true;
            client.size = 1 * 1024;
            nas = {
              cacheSize = 1024;
              server = true;
            };
          };
          container.enable = true;
          MySQL.enable = true;
          CodeServer.enable = true;
          redis.enable = true;
          prometheus.server = true;
          # weedServer.enable = true;
          inVM = true;
        };
        aliyun-hk = {
          publicIp = "47.243.22.114";
          wireguard = {
            index = 2;
            port = 49638;
            publicKey = "kY4n/K6zHjRNq/5f1yId2156zyfO/cVAwQddasPqjE8=";
            iptables.enable = true;
            gatewayServer = true;
          };
          weed = {
            enable = true;
            server.enable = true;
            client.enable = true;
            client.size = 1 * 1024;
            nas = {
              cacheSize = 1024;
              server = true;
            };
          };
          proxy.enable = true;
          NextCloudServer.enable = true;
          CodeServer.enable = true;
          OnedevServer.enable = true;
          redis.enable = true;
          inVM = true;
        };
        aliyun-ecs = {
          publicIp = "116.23.62.116";
          wireguard = { enable = false; };
          container.enable = true;
          inVM = true;
        };
        lxd = {
          wireguard = { enable = false; };
          inContainer = true;
        };
        nova9 = {
          wireguard = {
            index = 21;
            port = 53555;
            publicKey = "Xy1ofNbrxk2Gm8Q29hzCxtu5djfAvv9EFg6yOkbkBhw=";
            gateway = "aliyun-hk";
          };
        };
      };
    };
  };
}
