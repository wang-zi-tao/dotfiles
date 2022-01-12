{ pkgs, config, lib,  ... }:
let hostname = config.networking.hostName;
in {
  imports = [
    pkgs.home-manager.nixosModules.home-manager
    ({
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      hostname = config.networking.hostName;
      home-manager.users.wangzi = { ... }: {
        imports = [
          ../../home-manager/application/application.nix
          ../../home-manager/desktop/desktop.nix
          ../../home-manager/terminal/terminal.nix
          ../../home-manager/develop/develop.nix
        ];
        inherit hostname;
      };
    })
  ];
  users.groups.wangzi = {
    gid = 1000;
    name = "wangzi";
  };
  users.users.wangzi = {
    isNormalUser = true;
    uid = 1000;
    shell = pkgs.zsh;
    group = "wangzi";
    description = "王子陶";
    extraGroups =
      [ "wheel" "networkmanager" "vboxusers" "docker" "lxd" "audio" ];
  };
}
