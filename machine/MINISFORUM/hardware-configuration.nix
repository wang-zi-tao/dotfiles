{ config, lib, pkgs, modulesPath, ... }: {
  boot.loader =
    {
      systemd-boot.enable = true;
      systemd-boot.configurationLimit = 5;
      efi = {
        canTouchEfiVariables = true;
      };
      timeout = 1;
    };
  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ "amdgpu" ];
  boot.kernelModules = [ "kvm-amd" "mt7921e" ];
  boot.kernelPackages = pkgs.linuxKernel.packages.linux_5_16;
  boot.extraModulePackages = with config.boot.kernelPackages; [ ];
  boot.cleanTmpDir = false;
  boot.tmpOnTmpfs = false;
  services.udev.extraRules = ''
    SUBSYSTEM=="drivers", DEVPATH=="/bus/pci/drivers/mt7921e", ATTR{new_id}="14c3 0608"
  '';
  boot.extraModprobeConfig = ''
    alias pci:v000014C3d00000608sv*sd*bc*sc*i* mt7921e
  '';

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/e7cafd36-879f-4bae-8cd3-060799c7ab74";
      fsType = "f2fs";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/EC25-D9C6";
      fsType = "vfat";
    };
  fileSystems."/mnt/weed/server" = {
    device = "/dev/disk/by-uuid/ff337eaf-f0e7-468c-b23f-68a2ee2c0c73";
    fsType = "btrfs";
    options = [ "rw" "noatime" ];
  };
  fileSystems."/mnt/data" = {
    device = "/dev/disk/by-uuid/ff337eaf-f0e7-468c-b23f-68a2ee2c0c73";
    fsType = "btrfs";
    options = [ "rw" "noatime" ];
  };
  services.btrfs.autoScrub = { enable = true; };

  swapDevices = [ ];

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  hardware.firmware = [ pkgs.firmwareLinuxNonfree ];
  hardware.enableAllFirmware = true;
  hardware.opengl.extraPackages = [
    pkgs.amdvlk
  ];

  services.xserver = {
    modules = with pkgs.xorg; [ xf86videoamdgpu xf86inputlibinput xf86videodummy ];
    videoDrivers = [ "amdgpu" ];
  };
  # hardware.opengl.driSupport = true;
  # hardware.opengl.driSupport32Bit = true;

  systemd.timers.poweroff = {
    timerConfig.OnCalendar = "23:59:59 Asia/Shanghai";
    wantedBy = [ "timers.target" ];
  };
  systemd.services.poweroff = {
    path = with pkgs; [ systemd ];
    serviceConfig.Type = "oneshot";
    script = "systemctl poweroff";
  };
}
