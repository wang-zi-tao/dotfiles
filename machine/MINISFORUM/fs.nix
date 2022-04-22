{ config, lib, pkgs, modulesPath, ... }: {
  programs.fuse = { userAllowOther = true; };
  environment.systemPackages = with pkgs; [ duperemove btrfs-progs ];
  services.rsnapshot = {
    enable = true;
    cronIntervals = {
      daily = "50 21 * * *";
      weekly = "50 21 6 * *";
    };
    extraConfig = "snapshot_root\t/mnt/data/backup/\n"
      + "retain\tdaily\t7\n"
      + "retain\tweekly\t4\n"
      + "retain\tmonthly\t12\n"
      + "backup\t/mnt/weed/mount/wangzi-nuc/wangzi/\tlocalhost/";
  };
}
