{ pkgs, config, lib, ... }:
let
  cfg = config.cluster;
  nodeConfig = cfg.nodes."${cfg.nodeName}";
in
with lib; with builtins;{
  options =
    with types; {
      cluster = {
        ssh.publicKeySops = mkOption { type = path; };
        nodeName = mkOption { type = str; default = config.networking.hostName; };
        nodeConfig = mkOption { type = attrs; default = nodeConfig; };
        nodes = mkOption {
          description = "nodes";
          type = attrsOf
            (submodule
              ({ name, config, ... }: {
                options = {
                  hostname = mkOption { type = str; default = name; };
                  inVM = mkOption { type = bool; default = false; };
                  inContainer = mkOption { type = bool; default = false; };
                  users = mkOption { type = listOf str; default = [ ]; };
                  sshd.enable = mkOption {
                    type = bool;
                    default = true;
                  };
                  prometheus = {
                    server = mkOption { type = bool; default = false; };
                    nodeExporter = mkOption { type = bool; default = true; };
                  };
                  NextCloudServer.enable = mkOption { type = bool; default = false; };
                  MySQL.enable = mkOption { type = bool; default = false; };
                  redis.enable = mkOption { type = bool; default = false; };
                  OnedevServer.enable = mkOption { type = bool; default = false; };
                  CodeServer.enable = mkOption { type = bool; default = false; };
                  binary-cache.enable = mkOption { type = bool; default = false; };
                  wayland.enable = mkOption { type = bool; default = false; };
                  guiServer.enable = mkOption { type = bool; default = false; };
                  guiClient.enable = mkOption { type = bool; default = false; };
                  shell.enable = mkOption { type = bool; default = true; };
                  develop.enable = mkOption { type = bool; default = false; };
                  container.enable = mkOption { type = bool; default = false; };
                  virtualisation.enable = mkOption { type = bool; default = false; };
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
    ./virtualbox.nix
    ./develop.nix

    ./binary-cache.nix
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
      network = {
        nodes = {
          wangzi-pc = { };
          wangzi-nuc = { };
          huawei-ecs.config = { publicIp = "139.9.235.87"; };
          aliyun-hk.config = { publicIp = "47.243.22.114"; };
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
          wangzi-nuc.config = {
            index = 12;
            port = 16538;
            publicKey = "Vk2vw8TbtI7GgktauuppvfhKAAxyEeNC8+/nxt10t1s=";
            gateway = "aliyun-hk";
          };
          huawei-ecs.config = {
            index = 1;
            port = 30806;
            publicKey = "IfOIL06xMk/4r2QQvhNbLUFDLh97sXdHO3WIyOP4Sj4=";
          };
          aliyun-hk.config = {
            index = 2;
            port = 49638;
            publicKey = "kY4n/K6zHjRNq/5f1yId2156zyfO/cVAwQddasPqjE8=";
            iptables.enable = true;
            gatewayServer = true;
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
          wangzi-pc.config = { client.size = 8 * 1024; };
          wangzi-pc.to.aliyun-hk = { syncDirs = { "Cluster" = { }; }; };
          wangzi-pc.to.wangzi-nuc = {
            mountDirs = { "wangzi-nuc" = { }; };
            syncDirs = { "wangzi" = { }; };
          };
          wangzi-nuc.config = { client.size = 64 * 1024; };
          wangzi-nuc.to.aliyun-hk = { syncDirs = { "Cluster" = { }; }; };
          huawei-ecs.config = { client.size = 1 * 1024; };
          huawei-ecs.to.aliyun-hk = { syncDirs = { "Cluster" = { }; }; };
          aliyun-hk.config = { client.size = 1 * 1024; };
          aliyun-ecs.config = { client.size = 1 * 1024; };
          aliyun-ecs.to.aliyun-hk = { syncDirs = { "Cluster" = { }; }; };
        };
      };
      ssh.publicKeySops = ../secrets/public-key.yaml;
      nodes = {
        wangzi-pc = {
          users = [ "wangzi" ];
          guiServer.enable = true;
          guiClient.enable = true;
          develop.enable = true;
          container.enable = true;
          virtualisation.enable = true;
          CodeServer.enable = true;
        };
        wangzi-nuc = {
          users = [ "wangzi" ];
          /* localIp = "192.168.32.1"; */
          wayland.enable = true;
          guiServer.enable = true;
          guiClient.enable = true;
          develop.enable = true;
          container.enable = true;
          virtualisation.enable = true;
          CodeServer.enable = true;
        };
        huawei-ecs = {
          container.enable = true;
          MySQL.enable = true;
          CodeServer.enable = true;
          redis.enable = true;
          prometheus.server = true;
          # weedServer.enable = true;
          inVM = true;
        };
        aliyun-hk = {
          NextCloudServer.enable = true;
          CodeServer.enable = true;
          OnedevServer.enable = true;
          binary-cache.enable = true;
          redis.enable = true;
          inVM = true;
        };
        aliyun-ecs = {
          CodeServer.enable = true;
          # redis.enable = true;
          inVM = true;
        };
        lxd = {
          inContainer = true;
        };
        nova9 = { };
        M6 = { };
      };
    };
  };
}
