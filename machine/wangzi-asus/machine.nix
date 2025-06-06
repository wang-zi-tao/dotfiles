{ pkgs-template, nixpkgs, modules, ... }@inputs:
let
  hostname = "wangzi-asus";
  system = "x86_64-linux";
  pkgs = pkgs-template system;
in
nixpkgs.lib.nixosSystem {
  inherit pkgs system;
  specialArgs = inputs;
  modules = modules ++ [
    (
      { pkgs, lib, ... }:
      {
        imports = [
          ../../module/cluster.nix
          ./fs.nix
          ./network.nix
        ];
        networking.hostName = hostname;
        services.nixfs.enable = true;
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
        # boot.kernelPackages = pkgs.linuxKernel.packages.linux_xanmod;
        boot.initrd.availableKernelModules = [
          "xhci_pci"
          "ahci"
          "rtsx_usb_sdmmc"
          "nvme"
          "amdgpu"
        ];
        boot.initrd.kernelModules = [
          "vfio_pci"
          "vfio"
          "vfio_iommu_type1"
        ];
        boot.blacklistedKernelModules = [ "uvcvideo" ];
        boot.kernelModules = [
          "kvm-intel"
          "acpi_call"
          "dm-snapshot"
          "dm-raid"
          "dm-cache-default"
          "dm-thin-pool"
          "dm-mirror"
        ];
        boot.kernelParams = [
          "i915.enable_gvt=1"
          "intel_iommu=on"
          "i915.enable_guc=1"
          "i915.enable_fbc=1"
          "vfio-pci.ids=8086:a70d,1043:18ed"
        ];
        boot.extraModulePackages = [ ];
        boot.supportedFilesystems = [
          "ext4"
          "fat32"
          "ntfs"
        ];
        hardware = {
          nvidia.prime = {
            sync.enable = true;
            # reverseSync.enable = true;
            # offload.enable = true;
            # offload.enableOffloadCmd = true;
            allowExternalGpu = true;
            nvidiaBusId = "PCI:1:0:0";
            intelBusId = "PCI:0:2:0";
          };
        };
        hardware.nvidia-container-toolkit.enable = true;
        services.xserver = {
          dpi = 144;
          videoDrivers = [ "nvidia" ];
        };
        services.touchegg.enable = true;
        boot.plymouth.enable = lib.mkForce false;

        environment.systemPackages = with pkgs; [
          cudatoolkit
          cudatoolkit.lib
        ];
        services = {
          power-profiles-daemon.enable = false;
          tlp = {
            enable = true;
            settings = {
              PLATFORM_PROFILE_ON_BAT = "low-power";
              CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
              CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
              PCIE_ASPM_ON_BAT = "powersupersave";
            };
          };
          ollama.acceleration = "cuda";
        };

      }
    )
  ];
}
