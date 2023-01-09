{ config, lib, pkgs, modulesPath, ... }:
let
  disko-config = {
    disk.nvme0n1 = {
      device = "/dev/nvme0n1";
      type = "disk";
      content = {
        type = "table";
        format = "gpt";
        partitions = [
          {
            type = "partition";
            name = "ESP";
            start = "1MiB";
            end = "128MiB";
            bootable = true;
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/nvmeroot1-boot-efi";
            };
          }
          {
            name = "lvm";
            type = "partition";
            start = "128GiB";
            end = "100%";
            part-type = "primary";
            bootable = true;
            content = {
              type = "lvm_pv";
              vg = "pool";
            };
          }
        ];
      };
    };
    lvm_vg = {
      pool = {
        type = "lvm_vg";
        lvs = {
          root = {
            type = "lvm_lv";
            size = "100GiB";
            content = {
              type = "filesystem";
              format = "f2fs";
              mountpoint = "/mnt/nvmeroot-pool-root";
              mountOptions = [ ];
            };
          };
          home = {
            type = "lvm_lv";
            size = "100GiB";
            content = {
              type = "filesystem";
              format = "f2fs";
              extraArgs = "-f";
              mountpoint = "/mnt/nvmeroot-pool-home";
            };
          };
          server = {
            type = "lvm_lv";
            size = "100GiB";
            content = {
              type = "filesystem";
              format = "xfs";
              mountpoint = "/mnt/nvmeroot-pool-server";
            };
          };
          ntfs = {
            type = "lvm_lv";
            size = "64GiB";
            content = {
              type = "filesystem";
              format = "ntfs";
              mountpoint = "/mnt/nvmeroot-pool-ntfs";
            };
          };
        };
      };
    };
  };
in
{
  disko.devices = disko-config;
  disko.enableConfig = false;

  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/4697-4985";
    fsType = "vfat";
  };
  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/8a047596-a0e2-49bd-be28-ae784c825850";
    fsType = "f2fs";
    options = [ "rw" "noatime" ];
  };
  swapDevices =
    [{ device = "/dev/disk/by-uuid/6768a3cc-9acb-4b83-9cd3-1d9967202f50"; }];
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/1a2181e1-87a9-466f-8e44-715493f61f42";
    fsType = "f2fs";
    noCheck = false;
    options = [ "rw" "noatime" ];
  };
  fileSystems."/mnt/weed/server" = {
    device = "/dev/disk/by-uuid/8a047596-a0e2-49bd-be28-ae784c825850";
    fsType = "f2fs";
    options = [ "rw" "noatime" ];
  };
  boot.cleanTmpDir = false;
  boot.tmpOnTmpfs = false;
  services.fstrim.interval = "daily";
  programs.fuse = { userAllowOther = true; };
  environment.systemPackages = with pkgs; [ duperemove btrfs-progs ];
}
