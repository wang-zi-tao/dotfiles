{
  pkgs-template,
  nixpkgs,
  modules,
  ...
}@inputs:
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
        boot.kernelPackages = pkgs.linuxKernel.packages.linux_7_1;
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
          # prime.sync 只会生成 Xorg 配置（NVIDIA 当唯一活动 Screen），而本机会话是 GNOME 50
          # Wayland：GNOME 50 已经没有 Xorg 会话，Wayland 下由 mutter 按“内屏挂在哪块卡上”
          # 自己选主 GPU，而内屏 eDP-1 接在 i915 上（gpu_mux_mode=1，Optimus 模式），
          # 所以 sync 在这台机器上永远不会生效，桌面一直由 Intel 渲染。
          # 想让整个桌面走 NVIDIA：切 MUX 到独显直连（gpu_mux_mode=0，需重启）或者用
          # 支持 WLR_DRM_DEVICES 的合成器（如本仓库已启用的 sway）。
          # 这里改用 PRIME render offload：合成器留在 iGPU，用 `nvidia-offload <程序>` 让
          # 指定程序用 NVIDIA 渲染（GLX/Vulkan 都有效）。
          nvidia.prime = {
            # sync.enable = true; # 仅 Xorg 有效，Wayland 下无作用
            # reverseSync.enable = true;
            offload.enable = true;
            offload.enableOffloadCmd = true;
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
              DEVICES_TO_DISABLE_ON_BAT_NOT_IN_USE = "bluetooth";
            };
          };
          ollama.package = pkgs.ollama-cuda;
        };

      }
    )
  ];
}
