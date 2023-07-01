{ config, lib, pkgs, modulesPath, ... }: {
  programs.fuse = { userAllowOther = true; };
  swapDevices = [{
    device = "/swapfile";
    size = 1024 * 16;
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
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/c148466b-94e1-4d69-8fa0-c8d4b6d531d2";
    fsType = "ext4";
  };
  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/ACC7-B8A2";
    fsType = "vfat";
  };
  environment.systemPackages = with pkgs; [
    duperemove
  ];
}
