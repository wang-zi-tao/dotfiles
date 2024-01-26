{ pkgs-template, nixpkgs, home-manager, sops-nix, ... }@inputs:
let
  hostname = "wangzi-kingsoft";
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
    ({ pkgs, lib, config, ... }: {
      imports = [
        ../module/cluster.nix
      ];
      nix.settings.substituters = pkgs.lib.mkForce [
        "https://cache.nixos.org/"
        "https://nix-community.cachix.org"
        "https://nixpkgs-wayland.cachix.org"
        "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
        "https://mirrors.ustc.edu.cn/nix-channels/store"
      ];
      cluster.network.nodes."${hostname}" = { };
      cluster.nodes."${hostname}" = {
        users.wangzi = ../home-manager/profiles/wangzi-develop.nix;
        guiClient.enable = true;
        guiServer.enable = true;
        develop.enable = true;
        prometheus.nodeExporter = false;
        virtualisation.enable = true;
        sshd.enable = true;
      };
      sops.defaultSopsFile = ../secrets/wangzi-kingsoft.yaml;
      sops.secrets."script" = {
        mode = "0500";
        restartUnits = [ "run-secrets-scripts" ];
      };
      boot.loader.systemd-boot.enable = true;
      boot.supportedFilesystems = [ "ext4" "fat32" "ntfs" "f2fs" ];
      boot.loader.efi.canTouchEfiVariables = true;
      boot.loader.efi.efiSysMountPoint = "/boot/efi";
      boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "nvme" "usb_storage" "sd_mod" ];
      boot.initrd.kernelModules = [
        "amdgpu"
        "dm-snapshot"
        "dm-raid"
        "dm-cache-default"
        "dm-thin-pool"
        "dm-mirror"
      ];
      boot.kernelModules = [ "kvm-amd" ];
      boot.extraModulePackages = [ ];
      # boot.kernelPackages = pkgs.linuxKernel.packages.linux_6_1;
      boot.kernelParams = [
        "elevator=noop"
      ];
      hardware.cpu.amd.updateMicrocode = pkgs.lib.mkDefault config.hardware.enableRedistributableFirmware;
      hardware.firmware = [ pkgs.firmwareLinuxNonfree ];
      hardware.enableAllFirmware = true;
      networking = {
        #useDHCP = true;
        hostName = hostname;
        networkmanager = { enable = true; };
      };

      fileSystems."/" = {
        device = "/dev/pool/NixOS";
        fsType = "ext4";
      };
      /* fileSystems."/" = { */
      /*   device = "/dev/disk/by-uuid/d8a33441-258d-4698-a388-dc82bfaefda1"; */
      /*   fsType = "f2fs"; */
      /* }; */
      swapDevices = [{
        device = "/swapfile";
        size = 1024 * 16;
      }];
      fileSystems."/boot/efi" = {
        device = "/dev/disk/by-uuid/8EC5-6DAA";
        fsType = "vfat";
      };
      # fileSystems."/mnt/vm" = {
      #   device = "/dev/disk/by-uuid/42a82751-3f31-4ef7-abe5-5a610df9f146";
      #   fsType = "ext4";
      # };
      /* fileSystems."/mnt/build" = { */
      /*   device = "/dev/disk/by-uuid/3c0e89be-1fe4-46cd-840b-42c0bb21c33e"; */
      /*   fsType = "ext4"; */
      /* }; */
      # fileSystems."/mnt/data" = {
      #   device = "/dev/disk/by-uuid/201a5f10-3a83-4d9a-85dc-f85a87abacb6";
      #   fsType = "btrfs";
      # };
      # fileSystems."/mnt/weed/server" = {
      #   device = "/dev/disk/by-uuid/201a5f10-3a83-4d9a-85dc-f85a87abacb6";
      #   fsType = "btrfs";
      # };
      # virtualisation.kvmgt.vgpus = {
      #   i915-GVTg_V5_8.uuid = [ ];
      #   i915-GVTg_V5_4.uuid = [ "7ae0918a-834e-4942-ad9a-b399979595e3" ];
      # };
      hardware = {
        opengl.driSupport32Bit = true;
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
        bluetooth.enable = true;
        pulseaudio = { enable = true; };
      };
      services.xserver = {
        modules = with pkgs.xorg; [ xf86videoamdgpu xf86inputlibinput xf86videodummy xf86videovesa ];
        videoDrivers = [ "amdgpu" ];
      };
      services.samba = {
        enable = true;
        securityType = "user";
        extraConfig = ''
          workgroup = WORKGROUP
          server string = 192.168.56.1
          netbios name = wangzi-kingsoft
          #use sendfile = yes
          #max protocol = smb2
          # note: localhost is the ipv6 localhost ::1
          hosts allow = 192.168.122. 127.0.0.1 localhost
          hosts deny = 0.0.0.0/0
          guest account = wangzi
          map to guest = bad user
          follow symlinks = yes
          wide links = yes
          allow insecure wide links = yes

        '';
        shares = {
          wangzi-home = {
            path = "/home/wangzi";
            browseable = "yes";
            "read only" = "no";
            "guest ok" = "yes";
            "create mask" = "0644";
            "directory mask" = "0755";
            "follow symlinks" = "yes";
            "wide links" = "yes";
            "acl allow execute always" = "yes";
          };
        };
      };
      fileSystems."/export/home" = {
          device = "/home/wangzi";
          options = [ "bind" ];
      };
      services.nfs.server.enable = true;
      services.nfs.server.exports = ''
        /home 192.168.122.0/24(rw,all_squash,anonuid=1000,anongid=1000,insecure,no_subtree_check)
        /export 192.168.122.0/24(rw,all_squash,anonuid=1000,anongid=1000,insecure,no_subtree_check)
      '';
      services.nfs.idmapd.settings = {

      };
      users.users.root.hashedPassword = "$6$EleVrSVkk8j6lvlN$5EPVW5nhguBtB7WFaLBWrJHCCT.7xj7.NNgMR9OVdf3ngH80miDyox3JXcuHEu65NTnbGtlCX14bzxg0F1po8.";
      users.users.wangzi.hashedPassword = "$6$zBepEnWeXpVqW3Di$neIo/RZP.X7WS/VjECbcsLgKvXw4Ax1tgkoKBQikhoy7qlAdYSE/V5QQkwbl/dwSAx3daPVW1f.V93H.7.EZb1";
      hardware.ksm.enable = true;
      networking.firewall.rejectPackets = lib.mkForce false;
      services.openssh.permitRootLogin = lib.mkForce "no";
      # services.openssh.passwordAuthentication = lib.mkForce false;

      services.udev.extraRules = ''
        SUBSYSTEM=="usb", ATTR{idVendor}=="12d1", ATTR{idProduct}=="5000", GROUP="users", MODE="0777"
      '';
      vm = {
        guest-reserved = 1600;
        host-reserved = 1600;
        guest-reserved-percent = 0.6;
      };
    })
  ];
}
