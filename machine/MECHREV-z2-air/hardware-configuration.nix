{ config, lib, pkgs, modulesPath, ... }: {
  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ahci" "rtsx_usb_sdmmc" "bcache" ];
  boot.initrd.kernelModules = [ ];
  boot.blacklistedKernelModules = [ "uvcvideo" ];
  boot.kernelModules = [ "kvm-intel" "nvidia" ];
  boot.kernelParams = [ ];
  boot.extraModulePackages = [ ];
  boot.supportedFilesystems = [ "btrfs" "ext4" "fat32" "ntfs" "f2fs" ];
  hardware.enableAllFirmware = true;

  # powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  hardware = {
    opengl.driSupport32Bit = true;
    opengl.enable = true;
    opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
    opengl.extraPackages = with pkgs; [
      vaapiIntel
      libvdpau-va-gl
      vaapiVdpau
      intel-ocl
    ];
    opengl.setLdLibraryPath = true;
    opengl.driSupport = true;
    nvidia.prime = {
      sync.enable = true;
      sync.allowExternalGpu = true;
      # offload.enable = true;
      nvidiaBusId = "PCI:1:0:0";
      intelBusId = "PCI:0:2:0";
    };
    bluetooth.enable = true;
    pulseaudio = { enable = true; };
  };
  services.xserver = {
    modules = with pkgs.xorg; [ xf86videointel xf86inputlibinput ];
    videoDrivers = [ "nvidia" ];
  };
}
