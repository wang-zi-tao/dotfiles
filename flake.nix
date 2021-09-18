# This file is generated from "README.org"
{
  description = "NixOS configuration for all machines";

  inputs = {
    home-manager.url = "github:nix-community/home-manager/release-21.05";
    nur.url = "github:nix-community/NUR";
    nixpkgs.url = "github:nixos/nixpkgs/release-21.05";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    master.url = "github:nixos/nixpkgs/master";
    fenix = {
      url = "github:nix-community/fenix";
      # inputs.nixpkgs.follows = "unstable";
    };
    naersk = {
      url = "github:nmattia/naersk";
      # inputs.nixpkgs.follows = "unstable";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs@{ self, home-manager, nur, fenix, nixpkgs, flake-compat
    , naersk, ... }:
    let
      inherit (builtins) listToAttrs attrValues attrNames readDir;
      inherit (nixpkgs) lib;
      inherit (lib) removeSuffix;

      system = "x86_64-linux";
      pkgs = (import nixpkgs) {
        inherit system;
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
        fenix = fenix.overlay;
        other = final: prev: {
          # flake-compat = import inputs.flake-compat { };
          flake-compat = inputs.flake-compat;
          naersk = inputs.naersk;
          system = system;
          nur = import inputs.nur {
            nurpkgs = final.unstable;
            pkgs = final.unstable;
          };
        };
        nur = nur.overlay;
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
          system = system;
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
