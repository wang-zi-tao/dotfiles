{ pkgs-template, nixpkgs, home-manager, sops-nix, ... }@inputs:
let
  hostname = "nixvm";
  system = "x86_64-linux";
  pkgs = pkgs-template system;
in
nixpkgs.lib.nixosSystem
{
  inherit pkgs system;
  specialArgs = inputs;
  modules = [
    sops-nix.nixosModules.sops
    home-manager.nixosModules.home-manager
    ({ pkgs, lib, config, modulesPath, ... }: {
      imports = [
        (modulesPath + "/profiles/qemu-guest.nix")
        (modulesPath + "/virtualisation/qemu-vm.nix")
        ../module/cluster.nix
      ];

      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;
      boot.initrd.availableKernelModules =
        [ "ata_piix" "uhci_hcd" "virtio_pci" "sr_mod" "virtio_blk" ];
      boot.cleanTmpDir = true;
      virtualisation = {
        resolution = { x = 1920; y = 1080; };
        memorySize = 4096;
        cores = 16;
        diskSize = 8192;
        sharedDirectories = {
          home-wangzi = {
            source = "/home/wangzi";
            target = "/mnt/wangzi";
          };
        };
        qemu = {
          networkingOptions = [ "-net nic,model=virtio" ];
          options = [
            "-device virtio-balloon"

          ];
        };
      };
      documentation.nixos.enable = false;
      nix.settings.substituters = pkgs.lib.mkForce [
        "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
        "https://mirrors.ustc.edu.cn/nix-channels/store"
        "https://nix-community.cachix.org"
        "https://nixpkgs-wayland.cachix.org"
        "https://cache.nixos.org/"
      ];
      cluster.network.nodes."${hostname}" = { };
      cluster.nodes."${hostname}" = {
        users.wangzi = ../home-manager/profiles/wangzi-desktop-mini.nix;
        guiClient.enable = true;
        guiServer.enable = true;
        develop.enable = true;
        prometheus.nodeExporter = false;
      };
      sops.defaultSopsFile = "/";
      boot.extraModulePackages = [ ];
      # boot.kernelPackages = pkgs.linuxKernel.packages.linux_6_1;
      boot.kernelParams = [
        "elevator=noop"
        "console=tty1"
        "console=ttyS0,115200"
      ];
      networking = {
        #useDHCP = true;
        hostName = hostname;
        networkmanager = { enable = true; };
      };

      fileSystems."/" = {
        device = "/dev/vda1";
        fsType = "ext4";
      };
      services.xserver.desktopManager.gnome.enable = true;
      hardware.ksm.enable = true;
      services.xserver.enable = true;
      networking.firewall.rejectPackets = lib.mkForce true;
    })

  ];
}
