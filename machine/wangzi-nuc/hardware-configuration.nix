{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:
{
  services.power-profiles-daemon.enable = false;
  boot.loader = {
    systemd-boot.enable = true;
    systemd-boot.configurationLimit = 5;
    efi = {
      canTouchEfiVariables = true;
    };
    timeout = 1;
  };
  boot.kernelParams = [
    "i915.enable_gvt=1"
    "intel_iommu=on"
    "i915.enable_guc=1"
    "i915.enable_fbc=1"
    "pcie_aspm=off"
  ];
  boot.initrd.availableKernelModules = [
    "nvme"
    "xhci_pci"
    "ahci"
    "usbhid"
    "usb_storage"
    "sd_mod"
  ];
  boot.blacklistedKernelModules = [ "uvcvideo" ];
  boot.kernelModules = [
    "kvm-intel"
    "dm-snapshot"
    "dm-raid"
    "dm-cache-default"
    "dm-thin-pool"
    "dm-mirror"
    "i2c-i801"
    "i2c-dev"
  ];

  environment.systemPackages = with pkgs; [
    openrgb-with-all-plugins
  ];

  services.hardware.openrgb = {
    enable = true;
    package = pkgs.openrgb-with-all-plugins;
    motherboard = "intel";
  };

  boot.extraModulePackages = with config.boot.kernelPackages; [
    asus-ec-sensors
  ];
  boot.tmp.cleanOnBoot = false;
  boot.tmp.useTmpfs = false;
  services.udev.extraRules = ''
    SUBSYSTEM=="drivers", DEVPATH=="/bus/pci/drivers/mt7921e", ATTR{new_id}="14c3 0608"
  '';
  boot.extraModprobeConfig = ''
    alias pci:v000014C3d00000608sv*sd*bc*sc*i* mt7921e
  '';

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/b0e6efba-4627-4326-96db-e649592ce2c9";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/21E6-1EB0";
    fsType = "vfat";
  };

  fileSystems."/mnt/weed/server" = {
    device = "/dev/disk/by-uuid/ff337eaf-f0e7-468c-b23f-68a2ee2c0c73";
    fsType = "btrfs";
    options = [
      "rw"
      "noatime"
    ];
  };
  fileSystems."/mnt/data" = {
    device = "/dev/disk/by-uuid/ff337eaf-f0e7-468c-b23f-68a2ee2c0c73";
    fsType = "btrfs";
    options = [
      "rw"
      "noatime"
    ];
  };
  services.btrfs.autoScrub = {
    enable = true;
  };

  swapDevices = [ ];

  hardware.firmware = [ pkgs.firmwareLinuxNonfree ];
  hardware.enableAllFirmware = true;

  hardware = {
    graphics = {
      extraPackages32 = with pkgs.pkgsi686Linux; [ mesa ];
      extraPackages = with pkgs; [ 
        mesa
        vpl-gpu-rt
      ];
    };
  };

  services.xserver = {
    modules = with pkgs.xorg; [
      xf86videoamdgpu
      xf86inputlibinput
      xf86videodummy
      xf86videovesa
    ];
    videoDrivers = [ "modestting" ];
  };

  boot.kernelPackages = pkgs.linuxKernel.packages.linux_6_17;


  # services.xserver.monitorSection = ''Modeline "1920x1080" 23.53 1920 1952 2040 2072 1080 1106 1108 1135'';
  # services.xserver.resolutions = [{ x = "1920"; y = "1080"; }];
  # services.xserver.displayManager.autoLogin = {
  # enable = true;
  # user = "wangzi";
  # };
  # services.xserver.displayManager.defaultSession = lib.mkForce "none+awesome";
}
