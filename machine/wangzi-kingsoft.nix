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
    ({ pkgs, ... }: {
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
      };
      sops.defaultSopsFile = "/";
      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;
      boot.loader.efi.efiSysMountPoint = "/boot/efi";
      boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
      boot.initrd.kernelModules = [ ];
      boot.kernelModules = [ "kvm-intel" ];
      boot.extraModulePackages = [ ];
      boot.kernelPackages = pkgs.linuxPackages_6_0;
      boot.kernelParams = [
        "i915.enable_gvt=1"
        "intel_iommu=on"
        /* "i915.enable_guc=1" */
        /* "i915.enable_fbc=1" */
      ];
      networking = {
        #useDHCP = true;
        hostName = hostname;
        networkmanager = { enable = true; };
      };

      fileSystems."/" = {
        device = "/dev/disk/by-uuid/ecb782a6-db6a-40c8-a620-26f442945cdc";
        fsType = "ext4";
      };
      swapDevices = [{
        device = "/swapfile";
        size = 1024 * 16;
      }];
      fileSystems."/boot/efi" = {
        device = "/dev/disk/by-uuid/CA47-8911";
        fsType = "vfat";
      };
      fileSystems."/mnt/vm" = {
        device = "/dev/disk/by-uuid/42a82751-3f31-4ef7-abe5-5a610df9f146";
        fsType = "ext4";
      };
      fileSystems."/mnt/build" = {
        device = "/dev/disk/by-uuid/3c0e89be-1fe4-46cd-840b-42c0bb21c33e";
        fsType = "ext4";
      };
      fileSystems."/mnt/data" = {
        device = "/dev/disk/by-uuid/201a5f10-3a83-4d9a-85dc-f85a87abacb6";
        fsType = "btrfs";
      };
      fileSystems."/mnt/weed/server" = {
        device = "/dev/disk/by-uuid/201a5f10-3a83-4d9a-85dc-f85a87abacb6";
        fsType = "btrfs";
      };
      virtualisation.kvmgt.vgpus = {
        i915-GVTg_V5_8.uuid = [ ];
        i915-GVTg_V5_4.uuid = [ "7ae0918a-834e-4942-ad9a-b399979595e3" ];
      };
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
        bluetooth.enable = true;
        pulseaudio = { enable = true; };
      };
      services.xserver = {
        modules = with pkgs.xorg; [ xf86videointel xf86inputlibinput xf86videovesa ];
        videoDrivers = [ "modesetting" ];
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
      /* services.mongodb.enable = true; */
      /* services.mongodb.package = pkgs.mongodb-5_0; */
      /* services.mongodb.dbpath = "/mnt/build/mongodb"; */
      services.mysql.enable = true;
      services.mysql.package = pkgs.mysql80;
      # services.mysql.dataDir = "/mnt/build/mysqld";
      services.mysql.ensureUsers = [{
        name = "wangzi";
        ensurePermissions = {
          "mydb.*" = "ALL PRIVILEGES";
        };
      }];
      services.mysql.ensureDatabases = [ "mydb" ];
      environment.systemPackages = with pkgs; [ mysql ];
      users.users.root.hashedPassword = "$6$EleVrSVkk8j6lvlN$5EPVW5nhguBtB7WFaLBWrJHCCT.7xj7.NNgMR9OVdf3ngH80miDyox3JXcuHEu65NTnbGtlCX14bzxg0F1po8.";
      users.users.wangzi.hashedPassword = "$6$zBepEnWeXpVqW3Di$neIo/RZP.X7WS/VjECbcsLgKvXw4Ax1tgkoKBQikhoy7qlAdYSE/V5QQkwbl/dwSAx3daPVW1f.V93H.7.EZb1";
      /* hardware.ksm.enable = true; */

    })
  ];
}
