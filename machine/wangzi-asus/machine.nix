{ pkgs-template, nixpkgs, home-manager, sops-nix, nixseparatedebuginfod, ... }@inputs:
let
  hostname = "wangzi-asus";
  system = "x86_64-linux";
  pkgs = pkgs-template system;
in
nixpkgs.lib.nixosSystem {
  inherit pkgs system;
  specialArgs = inputs;
  modules = [
    sops-nix.nixosModules.sops
    home-manager.nixosModules.home-manager
    nixseparatedebuginfod.nixosModules.default
    ({ pkgs, lib, ... }: {
      imports = [
        ../../module/cluster.nix
        ./fs.nix
        ./network.nix
      ];
      networking.hostName = hostname;
      nixpkgs.config.allowUnfree = true;
      services.nixseparatedebuginfod.enable = true;
      sops.defaultSopsFile = ../../secrets/wangzi-asus.yaml;
      sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
      sops.age.keyFile = "/var/lib/sops-nix/key.txt";
      sops.age.generateKey = true;
      sops.secrets."script" = {
        mode = "0500";
        restartUnits = [ "run-secrets-scripts" ];
      };
      boot.loader = {
        systemd-boot.enable = true;
        systemd-boot.configurationLimit = 5;
        efi = {
          canTouchEfiVariables = false;
          efiSysMountPoint = "/boot/efi";
        };
        timeout = 1;
      };
      boot.initrd.availableKernelModules =
        [ "xhci_pci" "ahci" "rtsx_usb_sdmmc" "bcache" "nvme" ];
      boot.initrd.kernelModules = [ ];
      boot.blacklistedKernelModules = [ "uvcvideo" ];
      boot.kernelModules = [ "kvm-intel" "nvidia" ];
      boot.kernelParams = [
        "i915.enable_gvt=1"
        "intel_iommu=on"
        "i915.enable_guc=1"
        "i915.enable_fbc=1"
      ];
      boot.extraModulePackages = [ ];
      boot.supportedFilesystems = [ "ext4" "fat32" "ntfs" ];

      hardware = {
        enableRedistributableFirmware = true;
        enableAllFirmware = true;
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
        nvidia.modesetting.enable = true;
        nvidia.nvidiaSettings = true;
        nvidia.prime = {
          # sync.enable = true;
          allowExternalGpu = true;
          # offload.enable = true;
          nvidiaBusId = "PCI:1:0:0";
          intelBusId = "PCI:0:2:0";
        };
        bluetooth.enable = true;
        pulseaudio = { enable = true; };
      };
      services.xserver = {
        verbose = 7;
        dpi = 144;
        modules = with pkgs.xorg; [ xf86videointel xf86inputlibinput xf86videovesa ];
        videoDrivers = [
          # "modesetting"
          "nvidia"
        ];
      };
      services.touchegg.enable = true;
      boot.plymouth.enable = lib.mkForce false;
    })
  ];
}
