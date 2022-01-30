{ pkgs, config, lib, ... }:
let
  cfg = config.cluster;
  nodeConfig = cfg.nodes."${cfg.nodeName}";
in with lib; {
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
          type = attrsOf (submodule ({ name, config, ... }: {
            options = {
              hostname = mkOption {
                type = str;
                default = name;
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
              sshd.enable = mkOption {
                type = bool;
                default = true;
              };
              wireguard.enable = mkOption {
                type = bool;
                default = true;
              };
              wireguard.clusterIp = mkOption { type = str; };
              wireguard.port = mkOption {
                type = nullOr ints.u16;
                default = null;
              };
              wireguard.publicKey = mkOption { type = str; };
              MySQL.enable = mkOption {
                type = bool;
                default = false;
              };
              redis.enable = mkOption {
                type = bool;
                default = false;
              };
              weedServer.enable = mkOption {
                type = bool;
                default = false;
              };
              weedHotBackupServer.enable = mkOption {
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

    ./lightdm.nix
    ./sshd.nix
    ./container.nix
    ./virtualbox.nix
    ./gunpg.nix

    ./mySQL.nix
    ./redis.nix
    ./weed.nix
  ];
  config = {
    boot.isContainer = nodeConfig.inContainer;
    cluster = {
      ssh.publicKeySops = ../secrets/public-key.yaml;
      nodes = {
        wangzi-pc = {
          users = [ "wangzi" ];
          wireguard = {
            clusterIp = "192.168.16.11";
            port = 16538;
            publicKey = "jh1sHn85aq4Hkb3/s8AeQwfzpQ5PtNU7p0dqyeUOTWQ=";
          };
          guiServer.enable = true;
          guiClient.enable = true;
          develop.enable = true;
          container.enable = true;
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
          weedServer.enable = true;
        };
        aliyun-ecs = {
          publicIp = "116.23.62.116";
          wireguard = { enable = false; };
          container.enable = true;
        };
        lxd = {
          wireguard = { enable = false; };
          inContainer = true;
        };
      };
    };
  };
}
