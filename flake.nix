{
  description = "NixOS configuration for all machines in wangzicloud.cn";
  inputs = {
    home-manager.url = "github:nix-community/home-manager/release-21.11";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/NUR";
    nixpkgs-21-05.url = "github:nixos/nixpkgs/release-21.05";
    nixpkgs.url = "github:nixos/nixpkgs/release-21.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-master.url = "github:nixos/nixpkgs/master";
    fenix = { url = "github:nix-community/fenix"; };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs =
    inputs@{ self, home-manager, nur, fenix, nixpkgs, flake-compat, ... }:
    let
      system = "x86_64-linux";
      pkgs = (import nixpkgs) {
        inherit system;
        config = { allowUnfree = true; };
        overlays = with builtins;
          ([
            nur.overlay
            fenix.overlay
            (final: prev:
              {
                # flake-compat = import inputs.flake-compat { };
                flake-compat = inputs.flake-compat;
                # naersk = inputs.naersk;
                system = system;
                nur = import inputs.nur {
                  nurpkgs = final.unstable;
                  pkgs = final.unstable;
                };
                master = import inputs.nixpkgs-master {
                  system = final.system;
                  config = { allowUnfree = true; };
                };
                unstable = import inputs.nixpkgs-unstable {
                  system = final.system;
                  config = { allowUnfree = true; };
                };
                nixpkgs-21-05 = import inputs.nixpkgs-21-05 {
                  system = final.system;
                  config = { allowUnfree = true; };
                };
                scripts = (map
                  (f: pkgs.writeScriptBin f (readFile (./scripts + "/${f}")))
                  (attrNames (readDir ./scripts)));
              } // (listToAttrs (map (name: {
                inherit name;
                value = final.callPackage (./packages + "/${name}") { };
              }) (attrNames (readDir ./packages)))))
          ] ++ (map (name: import (./overlays + "/${name}"))
            (attrNames (readDir ./overlays))));
      };
      args = { inherit pkgs; } // inputs;
    in {
      nixosConfigurations = {
        wangzi-pc = import ./machine/MECHREV-z2-air/machine.nix args;
        huawei-ecs = import ./machine/huawei-ecs.nix args;
        aliyun-ecs = import ./machine/aliyun-ecs.nix args;
      };
      lxd = import ./machine/lxd.nix args;
    };
}
