{
  description = "NixOS configuration for all machines in wangzicloud.cn";
  inputs = {
    home-manager.url = "github:nix-community/home-manager/release-22.05";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/NUR";
    nixpkgs-21.url = "github:nixos/nixpkgs/release-21.11";
    nixpkgs.url = "github:nixos/nixpkgs/release-22.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    fenix = { url = "github:nix-community/fenix"; };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    sops-nix.url = "github:Mic92/sops-nix";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    inputs@{ self
    , home-manager
    , nur
    , fenix
    , nixpkgs
    , flake-compat
    , flake-utils
    , ...
    }:
    let
      system = "x86_64-linux";
      pkgs-args = {
        inherit system;
        config = { allowUnfree = true; };
        overlays = with builtins;
          ([
            nur.overlay
            fenix.overlay
            (final: prev:
              {
                flake-compat = inputs.flake-compat;
                system = system;
                flakes = inputs;
                nur = import inputs.nur {
                  nurpkgs = prev;
                  pkgs = prev;
                };
                unstable = import inputs.nixpkgs-unstable {
                  system = final.system;
                  config = { allowUnfree = true; };
                };
                nixpkgs-21 = import inputs.nixpkgs-21 {
                  system = final.system;
                  config = { allowUnfree = true; };
                };
                scripts = (map
                  (f: pkgs.writeScriptBin f (readFile (./scripts + "/${f}")))
                  (attrNames (readDir ./scripts)));
              } // (listToAttrs (map
                (name: {
                  inherit name;
                  value = final.callPackage (./packages + "/${name}") { };
                })
                (attrNames (readDir ./packages)))))
          ] ++ (map (name: import (./overlays + "/${name}"))
            (attrNames (readDir ./overlays))));
      };
      pkgs = (import nixpkgs) pkgs-args;
      args = { inherit pkgs pkgs-args; } // inputs;
    in
    {
      nixosConfigurations = {
        wangzi-pc = import ./machine/MECHREV-z2-air/machine.nix args;
        wangzi-nuc = import ./machine/MINISFORUM/machine.nix args;
        huawei-ecs = import ./machine/huawei-ecs.nix args;
        aliyun-hk = import ./machine/aliyun-hk.nix args;
        aliyun-ecs = import ./machine/aliyun-ecs.nix args;
        lxd = import ./machine/lxd.nix args;
      };
    } // flake-utils.lib.eachDefaultSystem (system:
    let pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [ sops gnumake git rnix-lsp nixfmt nix-du sumneko-lua-language-server nixos-generators ];
      };
      apps.repl = flake-utils.lib.mkApp {
        drv = pkgs.writeShellScriptBin "repl" ''
          confnix=$(mktemp)
          echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
          trap "rm $confnix" EXIT
          nix repl $confnix
        '';
      };
    }) // { pkgs = pkgs; lib = pkgs.lib; unstable = pkgs.unstable; nur = pkgs.nur; };
}
