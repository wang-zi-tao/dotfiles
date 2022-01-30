{ config, pkgs, ... }:
let
  lib = pkgs.lib;
  nodeConfig = config.cluster.nodeConfig;
in {
  config = {
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    users.users.root = {
      shell = pkgs.zsh;
      hashedPassword =
        "$6$5fY2AtLiWqvmZ/Td$TCiebidTEBeYii.wHEktzs6sAxyH5GU7EivvMAY8uM2PMHCZUmeujfusgJA/xYVNyGa1rQG2oqCB9AfZ9Bio40";
    };
    home-manager.users.root = { ... }: {
      imports = [ ../home-manager/terminal/terminal.nix ];
    };
    home-manager.users.wangzi =
      lib.mkIf (builtins.elem "wangzi" nodeConfig.users) ({ ... }: {
        imports = [
          ../home-manager/application/application.nix
          ../home-manager/desktop/desktop.nix
          ../home-manager/terminal/terminal.nix
          ../home-manager/develop/develop.nix
        ];
      });
    users.groups.wangzi = lib.mkIf (builtins.elem "wangzi" nodeConfig.users) {
      gid = 1000;
      name = "wangzi";
    };
    users.users.wangzi = lib.mkIf (builtins.elem "wangzi" nodeConfig.users) {
      isNormalUser = true;
      uid = 1000;
      shell = pkgs.zsh;
      group = "wangzi";
      description = "王子陶";
      extraGroups =
        [ "wheel" "networkmanager" "vboxusers" "docker" "lxd" "audio" ];
      hashedPassword =
        "$6$Rd67.bPCRXvMahE1$seiawpNy.1eV/CLVBY5qogsP5Z77BIGMW2FvNf51XWi0QU597YpbnfaNjTwQQxKA3mSwBV47dxlkJmqyX1y5x1";
    };
  };
}
