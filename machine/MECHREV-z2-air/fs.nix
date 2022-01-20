{ config, lib, pkgs, modulesPath, ... }: {

  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/4697-4985";
    fsType = "vfat";
  };
  boot.tmpOnTmpfs = true;
  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/8a047596-a0e2-49bd-be28-ae784c825850";
    fsType = "f2fs";
    options = [ "rw" "noatime" ];
  };
  fileSystems."/mnt/cache" = {
    device = "/dev/disk/by-uuid/8a047596-a0e2-49bd-be28-ae784c825850";
    fsType = "f2fs";
    options = [ "rw" "noatime" ];
  };
  fileSystems."/mnt/data" = {
    device = "/dev/disk/by-uuid/ff337eaf-f0e7-468c-b23f-68a2ee2c0c73";
    fsType = "btrfs";
    options = [ "rw" "noatime" ];
  };
  swapDevices =
    [{ device = "/dev/disk/by-uuid/6768a3cc-9acb-4b83-9cd3-1d9967202f50"; }];
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/1a2181e1-87a9-466f-8e44-715493f61f42";
    fsType = "f2fs";
    options = [ "rw" "noatime" ];
  };
  services.fstrim.interval = "daily";
  services.btrfs.autoScrub = { enable = true; };
  # systemd.timers.snapper-snapshot = {
  #   wantedBy = [ "timers.target" ];
  #   partOf = [ "snapper-snapshot.service" ];
  #   timerConfig.OnCalendar = [ "*-*-* *:00:00" ];
  # };
  # systemd.services.snapper-snapshot = {
  #   serviceConfig.Type = "oneshot";
  #   script = ''
  #     for i in `ls /etc/snapper/configs/`
  #     do
  #       ${pkgs.snapper}/bin/snapper -c $i create -c timeline -d timeline
  #       ${pkgs.snapper}/bin/snapper -c $i cleanup timeline
  #     done
  #   '';
  # };
  systemd.services.seaweedfs = {
    enable = true;
    description = "seaweedfs server";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = "5s";
      ExecStart =
        "${pkgs.seaweedfs}/bin/weed server -dir=/mnt/data/weed/weed -master.port=128 -master.dir=/mnt/data/weed/master -master.defaultReplication=000 -master.volumePreallocate=false -volume.dir.idx=/mnt/data/weed/volume -filer -filer.port=192 -volume.port=256 -volume.publicUrl=192.168.16.1:256";
    };
  };
  systemd.services.seaweedfs-mount = {
    enable = true;
    description = "mount seaweedfs";
    wantedBy = [ "multi-user.target" ];
    path = with pkgs; [ busybox unstable.seaweedfs fuse ];
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = "2s";
      ExecStart =
        "${pkgs.seaweedfs}/bin/weed mount -filer=localhost:192 -dir=/mnt/weed-mount -dirAutoCreate -cacheCapacityMB=${
          builtins.toString (128 * 1024)
        } -cacheDir=/mnt/cache/weed";
      ExecStop = "${pkgs.util-linux}/bin/umount /mnt/weed-mount -l";
    };
  };
  programs.fuse = { userAllowOther = true; };
  #systemd.services.k3s-weed = {
  #  enable = false;
  #  description = "mount seaweedfs";
  #  wantedBy = [ "multi-user.target" ];
  #  path = with pkgs; [ busybox unstable.seaweedfs fuse ];
  #  serviceConfig = {
  #    Type = "simple";
  #    Restart = "always";
  #    RestartSec = "5s";
  #    ExecStart =
  #      "${pkgs.busybox}/bin/sh -c '${pkgs.busybox}/bin/mkdir -p /mnt/seaweedfs ; ${pkgs.unstable.seaweedfs}/bin/weed mount -volumeServerAccess=filerProxy -filer=10.42.0.1:8888 -dir=/mnt/seaweedfs'";
  #    ExecStop = "${pkgs.util-linux}/bin/umount /mnt/seaweedfs -f";
  #  };
  #};
  environment.systemPackages = with pkgs; [ duperemove btrfs-progs seaweedfs ];
}
