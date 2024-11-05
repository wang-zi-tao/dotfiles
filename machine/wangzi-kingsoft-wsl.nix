{
  pkgs-template,
  nixpkgs,
  home-manager,
  sops-nix,
  nixos-wsl,
  nixfs,
  ...
}@inputs:
let
  hostname = "wangzi-kingsoft-wsl";
  system = "x86_64-linux";
  pkgs = pkgs-template system;
in
nixpkgs.lib.nixosSystem {
  inherit pkgs system;
  specialArgs = inputs;
  modules = [
    sops-nix.nixosModules.sops
    home-manager.nixosModules.home-manager
    nixos-wsl.nixosModules.default
    nixfs.nixosModules.nixfs
    (
      {
        pkgs,
        lib,
        config,
        ...
      }:
      {
        wsl.enable = true;
        wsl.defaultUser = "wangzi";
        imports = [ ../module/cluster.nix ];
        cluster.network.nodes."${hostname}" = { };
        cluster.nodes."${hostname}" = {
          users.wangzi = ../home-manager/profiles/wangzi-cpp.nix;
          guiClient.enable = false;
          guiServer.enable = false;
          develop.enable = false;
          prometheus.nodeExporter = false;
          virtualisation.enable = false;
          container.enable = true;
          inContainer = true;
          sshd.enable = false;
        };
        sops.defaultSopsFile = "/";
        networking = {
          #useDHCP = true;
          hostName = hostname;
          networkmanager = {
            enable = true;
          };
        };

        swapDevices = [
          {
            device = "/swapfile";
            size = 1024 * 16;
          }
        ];
        users.users.root.hashedPassword = "$6$EleVrSVkk8j6lvlN$5EPVW5nhguBtB7WFaLBWrJHCCT.7xj7.NNgMR9OVdf3ngH80miDyox3JXcuHEu65NTnbGtlCX14bzxg0F1po8.";
        users.users.wangzi.hashedPassword = "$6$zBepEnWeXpVqW3Di$neIo/RZP.X7WS/VjECbcsLgKvXw4Ax1tgkoKBQikhoy7qlAdYSE/V5QQkwbl/dwSAx3daPVW1f.V93H.7.EZb1";
        security.sudo = {
          enable = true;
        };
        virtualisation.docker = {
          enable = true;
        };
        virtualisation.podman = {
          enable = true;
        };
      }
    )
  ];
}
