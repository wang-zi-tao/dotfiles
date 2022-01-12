{ pkgs, nixpkgs, home-manager, ... }:
let hostname = "wangzi-pc";
in nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    ../../module/nixos.nix
    ../../module/boot.nix
    ../../module/terminal.nix
    ../../module/gui.nix
    ../../module/libinput.nix

    ../../services/gunpg.nix
    ../../services/gpaste.nix
    ../../services/sshd.nix
    ../../services/container.nix
    ../../services/lightdm.nix
    ../../services/virtualbox.nix
    ../../module/network.nix

    ./fs.nix
    ./power.nix
    ./network.nix
    ./hardware-configuration.nix

    # ../../module/user/root.nix
    # ../../module/user/wangzi.nix

    home-manager.nixosModules.home-manager
    ({
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.users.wangzi = { ... }: {
        imports = [
          ../../home-manager/application/application.nix
          ../../home-manager/desktop/desktop.nix
          ../../home-manager/terminal/terminal.nix
          ../../home-manager/develop/develop.nix
        ];
      };
      home-manager.users.root = { ... }: {
        imports = [ ../../home-manager/terminal/terminal.nix ];
        inherit hostname;
      };
    })
    ({ pkgs, ... }: {
      users.groups.wangzi = {
        gid = 1000;
        name = "wangzi";
      };
      users.users.root = { shell = pkgs.zsh; };
      users.users.wangzi = {
        isNormalUser = true;
        uid = 1000;
        shell = pkgs.zsh;
        group = "wangzi";
        description = "王子陶";
        extraGroups =
          [ "wheel" "networkmanager" "vboxusers" "docker" "lxd" "audio" ];
      };
    })
  ];
  inherit pkgs;
}
