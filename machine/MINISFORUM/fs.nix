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
      + "backup\t/mnt/weed/mount/wangzi-nuc/\tlocalhost/\n";
  };
  swapDevices = [{
    device = "/swapfile";
    size = (1024 * 16);
  }];
  systemd.services = {
    create-swapfile = {
      serviceConfig.Type = "oneshot";
      wantedBy = [ "swap-swapfile.swap" ];
      script = ''
        ${pkgs.coreutils}/bin/truncate -s 0 /swapfile
      '';
    };
  };
}
