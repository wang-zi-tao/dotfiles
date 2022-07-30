{ config, lib, pkgs, modulesPath, ... }: {
  boot.loader = {
    systemd-boot.enable = true;
    systemd-boot.configurationLimit = 5;
    efi = {
      canTouchEfiVariables = false;
      efiSysMountPoint = "/boot/efi";
    };
    timeout = 1;
  };
  boot.kernelPackages = pkgs.linuxKernel.packages.linux_xanmod.extend (self: super: {
    virtualbox = super.virtualbox.override { inherit (self) kernel; };
  });
  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ahci" "rtsx_usb_sdmmc" "bcache" ];
  boot.initrd.kernelModules = [ ];
  boot.blacklistedKernelModules = [ "uvcvideo" ];
  boot.kernelModules = [ "kvm-intel" "nvidia" ];
  boot.kernelParams = [ ];
  boot.extraModulePackages = [ ];
  boot.supportedFilesystems = [ "btrfs" "ext4" "fat32" "ntfs" "f2fs" ];
  hardware.enableAllFirmware = true;

  hardware = {
    opengl.driSupport32Bit = true;
    opengl.enable = true;
    opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
    opengl.extraPackages = with pkgs; [
      vaapiIntel
      libvdpau-va-gl
      vaapiVdpau
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
    modules = with pkgs.xorg; [ xf86videointel xf86inputlibinput xf86videovesa ];
    videoDrivers = [ "nvidia" ];
  };
}
