{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:
{
  boot.loader = {
    systemd-boot.enable = true;
    systemd-boot.configurationLimit = 5;
    efi = {
      canTouchEfiVariables = false;
      efiSysMountPoint = "/boot/efi";
    };
    timeout = 1;
  };
  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "rtsx_usb_sdmmc"
    "bcache"
  ];
  boot.initrd.kernelModules = [ ];
  boot.blacklistedKernelModules = [ "uvcvideo" ];
  boot.kernelModules = [
    "kvm-intel"
    "nvidia"
  ];
  boot.kernelParams = [
    "i915.enable_gvt=1"
    "i915.enable_guc=1"
    "i915.enable_fbc=1"
  ];
  boot.extraModulePackages = [ ];
  boot.supportedFilesystems = [
    "btrfs"
    "ext4"
    "fat32"
    "ntfs"
    "f2fs"
  ];
  hardware.enableAllFirmware = true;

  hardware = {
    nvidia.open = true;
    nvidia.modesetting.enable = true;
    nvidia.prime = {
      sync.enable = true;
      allowExternalGpu = true;
      # offload.enable = true;
      nvidiaBusId = "PCI:1:0:0";
      intelBusId = "PCI:0:2:0";
    };
    bluetooth.enable = true;
  };
  services.xserver = {
    videoDrivers = [
      "nvidia"
      # "modesetting"
    ];
  };
  virtualisation.kvmgt.vgpus = {
    i915-GVTg_V5_8.uuid = [ ];
    i915-GVTg_V5_4.uuid = [ "104319bc-2adb-11ed-ae66-73f45bf4765e" ];
  };
  # services.xserver.monitorSection = ''Modeline "1920x1080" 23.53 1920 1952 2040 2072 1080 1106 1108 1135'';
  # services.xserver.resolutions = [{ x = "1920"; y = "1080"; }];
  # services.xserver.displayManager.autoLogin = {
  #   enable = true;
  #   user = "wangzi";
  # };
  # services.displayManager.defaultSession = lib.mkForce "gnome";
  services.xserver.desktopManager.gnome.enable = true;
}
