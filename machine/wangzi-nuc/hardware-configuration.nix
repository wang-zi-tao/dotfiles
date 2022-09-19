{ config, lib, pkgs, modulesPath, ... }: {
  services.power-profiles-daemon.enable = false;
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
      noCheck = false;
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

  hardware = {
    opengl.enable = true;
    opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva pkgs.driversi686Linux.mesa ];
    opengl.extraPackages = with pkgs; [
      amdvlk
      vaapiIntel
      libvdpau-va-gl
      vaapiVdpau
    ];
    opengl.setLdLibraryPath = true;
    opengl.driSupport = true;
    opengl.driSupport32Bit = true;
    bluetooth.enable = true;
    pulseaudio = { enable = true; };
  };

  services.xserver = {
    modules = with pkgs.xorg; [ xf86videoamdgpu xf86inputlibinput xf86videodummy xf86videovesa ];
    videoDrivers = [ "amdgpu" ];
  };
  services.xserver.monitorSection = ''Modeline "1920x1080" 23.53 1920 1952 2040 2072 1080 1106 1108 1135'';
  services.xserver.resolutions = [{ x = "1920"; y = "1080"; }];
  services.xserver.displayManager.autoLogin = {
    enable = true;
    user = "wangzi";
  };
  services.xserver.displayManager.defaultSession = lib.mkForce "none+awesome";
}
