{ config, lib, pkgs, modulesPath, ... }: {

  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/4697-4985";
    fsType = "vfat";
  };
  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/ff337eaf-f0e7-468c-b23f-68a2ee2c0c73";
    fsType = "btrfs";
    options = [
      "rw"
      "nodev"
      "noatime"
      "compress=zstd:3"
      "ssd"
      "discard=async"
      "space_cache"
      "autodefrag"
      "subvolid=256"
      "subvol=/home"
    ];
  };
  swapDevices =
    [{ device = "/dev/disk/by-uuid/6768a3cc-9acb-4b83-9cd3-1d9967202f50"; }];
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/82d09e74-3344-4ed8-a91e-1a55cee670d9";
    fsType = "btrfs";
    options = [
      "rw"
      "nodev"
      "noatime"
      "compress=zstd:3"
      "ssd"
      "discard=async"
      "space_cache"
      "autodefrag"
      "space_cache=v2"
      "subvol=/nixos"
    ];
  };
  # fileSystems."/boot" =
  # { device = "/dev/disk/by-uuid/82d09e74-3344-4ed8-a91e-1a55cee670d9";
  # fsType = "btrfs";
  # options = ["rw" "nodev" "noatime" "compress=zstd:3" "ssd" "discard=async" "space_cache" "autodefrag" "subvol=/nixos-boot"];
  # };
  services.fstrim.interval = "daily";
  services.btrfs.autoScrub = { enable = true; };
  services.snapper.configs = {
    home = {
      subvolume = "/home";
      extraConfig = ''
        TIMELINE_LIMIT_HOURLY=0
        TIMELINE_LIMIT_DAILY=3
        TIMELINE_LIMIT_WEEKLY=1
        TIMELINE_LIMIT_MONTHLY=0
        TIMELINE_LIMIT_YEARLY=0
      '';
    };
    home_wangzi = {
      subvolume = "/home/wangzi";
      extraConfig = ''
        ALLOW_USERS="wangzi"
        TIMELINE_LIMIT_HOURLY=0
        TIMELINE_LIMIT_DAILY=3
        TIMELINE_LIMIT_WEEKLY=1
        TIMELINE_LIMIT_MONTHLY=0
        TIMELINE_LIMIT_YEARLY=0
      '';
    };
    home_config = {
      subvolume = "/home/wangzi/.config";
      extraConfig = ''
        ALLOW_USERS="wangzi"
        TIMELINE_LIMIT_HOURLY=0
        TIMELINE_LIMIT_DAILY=3
        TIMELINE_LIMIT_WEEKLY=1
        TIMELINE_LIMIT_MONTHLY=0
        TIMELINE_LIMIT_YEARLY=0
      '';
    };
    home_local = {
      subvolume = "/home/wangzi/.local";
      extraConfig = ''
        ALLOW_USERS="wangzi"
        TIMELINE_LIMIT_HOURLY=0
        TIMELINE_LIMIT_DAILY=3
        TIMELINE_LIMIT_WEEKLY=1
        TIMELINE_LIMIT_MONTHLY=0
        TIMELINE_LIMIT_YEARLY=0
      '';
    };
    home_download = {
      subvolume = "/home/wangzi/下载";
      extraConfig = ''
        ALLOW_USERS="wangzi"
        TIMELINE_LIMIT_HOURLY=0
        TIMELINE_LIMIT_DAILY=3
        TIMELINE_LIMIT_WEEKLY=1
        TIMELINE_LIMIT_MONTHLY=0
        TIMELINE_LIMIT_YEARLY=0
      '';
    };
    home_public = {
      subvolume = "/home/wangzi/公共";
      extraConfig = ''
        ALLOW_USERS="wangzi"
        TIMELINE_LIMIT_HOURLY=0
        TIMELINE_LIMIT_DAILY=3
        TIMELINE_LIMIT_WEEKLY=1
        TIMELINE_LIMIT_MONTHLY=0
        TIMELINE_LIMIT_YEARLY=0
      '';
    };
    home_picture = {
      subvolume = "/home/wangzi/图片";
      extraConfig = ''
        ALLOW_USERS="wangzi"
        TIMELINE_LIMIT_HOURLY=0
        TIMELINE_LIMIT_DAILY=3
        TIMELINE_LIMIT_WEEKLY=1
        TIMELINE_LIMIT_MONTHLY=0
        TIMELINE_LIMIT_YEARLY=0
      '';
    };
    home_video = {
      subvolume = "/home/wangzi/视频";
      extraConfig = ''
        ALLOW_USERS="wangzi"
        TIMELINE_LIMIT_HOURLY=0
        TIMELINE_LIMIT_DAILY=3
        TIMELINE_LIMIT_WEEKLY=1
        TIMELINE_LIMIT_MONTHLY=0
        TIMELINE_LIMIT_YEARLY=0
      '';
    };
    home_music = {
      subvolume = "/home/wangzi/音乐";
      extraConfig = ''
        ALLOW_USERS="wangzi"
        TIMELINE_LIMIT_HOURLY=0
        TIMELINE_LIMIT_DAILY=3
        TIMELINE_LIMIT_WEEKLY=1
        TIMELINE_LIMIT_MONTHLY=0
        TIMELINE_LIMIT_YEARLY=0
      '';
    };
    home_temp = {
      subvolume = "/home/wangzi/Temp";
      extraConfig = ''
        ALLOW_USERS="wangzi"
        TIMELINE_LIMIT_HOURLY=0
        TIMELINE_LIMIT_DAILY=3
        TIMELINE_LIMIT_WEEKLY=1
        TIMELINE_LIMIT_MONTHLY=0
        TIMELINE_LIMIT_YEARLY=0
      '';
    };
    home_vbox = {
      subvolume = "/home/wangzi/VirtualBox VMs";
      extraConfig = ''
        ALLOW_USERS="wangzi"
        TIMELINE_LIMIT_HOURLY=0
        TIMELINE_LIMIT_DAILY=3
        TIMELINE_LIMIT_WEEKLY=1
        TIMELINE_LIMIT_MONTHLY=0
        TIMELINE_LIMIT_YEARLY=0
      '';
    };
    home_document = {
      subvolume = "/home/wangzi/文档";
      extraConfig = ''
        ALLOW_USERS="wangzi"
        TIMELINE_LIMIT_HOURLY=24
        TIMELINE_LIMIT_DAILY=7
        TIMELINE_LIMIT_WEEKLY=4
        TIMELINE_LIMIT_MONTHLY=1
        TIMELINE_LIMIT_YEARLY=1
      '';
    };
    home_workspace = {
      subvolume = "/home/wangzi/workspace";
      extraConfig = ''
        ALLOW_USERS="wangzi"
        TIMELINE_LIMIT_HOURLY=24
        TIMELINE_LIMIT_DAILY=7
        TIMELINE_LIMIT_WEEKLY=4
        TIMELINE_LIMIT_MONTHLY=1
        TIMELINE_LIMIT_YEARLY=1
      '';
    };
  };
}
