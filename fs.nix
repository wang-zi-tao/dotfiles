{ config, lib, pkgs, modulesPath, ... }:
{

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
}
