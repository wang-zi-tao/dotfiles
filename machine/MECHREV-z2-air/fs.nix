{ config, lib, pkgs, modulesPath, ... }: {

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
