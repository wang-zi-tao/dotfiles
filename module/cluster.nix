{ pkgs, config, lib, ... }:
let
  cfg = config.cluster;
  nodeConfig = cfg.nodes."${cfg.nodeName}";
in
with lib; {
  options = with lib;
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
                    clusterIp = mkOption { type = str; };
                    port = mkOption {
                      type = nullOr ints.u16;
                      default = null;
                    };
                    publicKey = mkOption { type = str; };
                    gateway = mkOption { type = nullOr str; default = null; };
                    gatewayServer = mkOption { type = bool; default = false; };
                    iptables.enable = mkOption { type = bool; default = false; };
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
                  weed = {
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
                        default = "/tmp";
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
                    nas.attach =
                      mkOption
                        {
                          type = listOf str;
                          default = [ ];
                        };
                    nas.server = mkOption {
                      type = bool;
                      default = false;
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
                  wayland.enable =
                    mkOption
                      {
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
                  proxy.enable = mkOption
                    {
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
    ./redis.nix
    ./weed.nix
  ];
  config = {
    boot.isContainer = nodeConfig.inContainer;
    cluster = {
      ssh.publicKeySops = ../secrets/public-key.yaml;
      collections = {
        wangzi = {
          servers = [ "wangzi-nuc" "wangzi-pc" ];
          clients = [ "wangzi-nuc" "wangzi-pc" ];
          sync = [{ from = "wangzi-pc"; to = "wangzi-nuc"; }];
        };
      };
      nodes = {
        wangzi-pc = {
          users = [ "wangzi" ];
          localIp = "192.168.32.128";
          wireguard = {
            clusterIp = "192.168.16.11";
            port = 16538;
            publicKey = "jh1sHn85aq4Hkb3/s8AeQwfzpQ5PtNU7p0dqyeUOTWQ=";
            gateway = "aliyun-hk";
          };
          guiServer.enable = true;
          guiClient.enable = true;
          develop.enable = true;
          container.enable = true;
          weed = {
            server.enable = true;
            client.enable = true;
            nas = { attach = [ "wangzi-nuc" ]; };
          };
        };
        wangzi-nuc = {
          users = [ "wangzi" ];
          localIp = "192.168.32.1";
          wireguard = {
            clusterIp = "192.168.16.12";
            port = 16538;
            publicKey = "Vk2vw8TbtI7GgktauuppvfhKAAxyEeNC8+/nxt10t1s=";
            gateway = "aliyun-hk";
          };
          wayland.enable = true;
          guiServer.enable = true;
          guiClient.enable = true;
          develop.enable = true;
          container.enable = true;
          weed = {
            server.enable = true;
            client.enable = true;
            client.size = 64 * 1024;
            nas = { server = true; };
          };
        };
        huawei-ecs = {
          publicIp = "139.9.235.87";
          wireguard = {
            clusterIp = "192.168.16.1";
            port = 30806;
            publicKey = "IfOIL06xMk/4r2QQvhNbLUFDLh97sXdHO3WIyOP4Sj4=";
          };
          container.enable = true;
          MySQL.enable = true;
          redis.enable = true;
          # weedServer.enable = true;
          inVM = true;
        };
        aliyun-hk = {
          publicIp = "47.243.22.114";
          wireguard = {
            clusterIp = "192.168.16.2";
            port = 49638;
            publicKey = "kY4n/K6zHjRNq/5f1yId2156zyfO/cVAwQddasPqjE8=";
            iptables.enable = true;
            gatewayServer = true;
          };
          proxy.enable = true;
          NextCloudServer.enable = true;
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
            clusterIp = "192.168.16.21";
            port = 53555;
            publicKey = "Xy1ofNbrxk2Gm8Q29hzCxtu5djfAvv9EFg6yOkbkBhw=";
            gateway = "aliyun-hk";
          };
        };
      };
    };
  };
}
