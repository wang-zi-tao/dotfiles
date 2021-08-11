# This file is generated from "README.org"
{
  description = "NixOS configuration for all machines";

  inputs = {
    home-manager = {
      url = "github:rycee/home-manager/release-21.05";
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };
    nur.url = "github:nix-community/NUR";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    master.url = "github:nixos/nixpkgs/master";
  };

  outputs = inputs@{ self, home-manager, nur, nixpkgs, ... }:
    let
      inherit (builtins) listToAttrs attrValues attrNames readDir;
      inherit (nixpkgs) lib;
      inherit (lib) removeSuffix;

      pkgs = (import nixpkgs) {
        system = "x86_64-linux";
        config = { allowUnfree = true; };
        overlays = attrValues self.overlays;
      };

      defaults = { pkgs, ... }: { imports = [ ]; };
    in {
      overlays = let
        overlayFiles = listToAttrs (map (name: {
          name = removeSuffix ".nix" name;
          value = import (./overlays + "/${name}");
        }) (attrNames (readDir ./overlays)));
      in overlayFiles // {
        nur = final: prev: {
          nur = import inputs.nur {
            nurpkgs = final.unstable;
            pkgs = final.unstable;
          };
        };
        unstable = final: prev: {
          unstable = import inputs.unstable {
            system = final.system;
            config = { allowUnfree = true; };
          };
        };
        master = final: prev: {
          master = import inputs.master {
            system = final.system;
            config = { allowUnfree = true; };
          };
        };
      };

      nixosConfigurations = {
        wangzi-pc = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            defaults
            ./configuration.nix
            home-manager.nixosModules.home-manager
            ({
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.wangzi = { ... }: {
                imports = [ ./home.nix ];
              };
              home-manager.users.root = { ... }: {
                imports = [
                  ./zsh/home.nix
                  ./tmux/home.nix
                  ./git/home.nix
                  ./neovim/home.nix
                ];
                home.packages =
                  (with pkgs; [ autojump killall curl wget unzip ]);
              };
            })
          ];
          inherit pkgs;
        };
      };
    };
}
