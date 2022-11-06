{
  description = "NixOS configuration for all machines in wangzicloud.cn";
  inputs = {
    home-manager.url = "github:nix-community/home-manager/release-22.05";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-on-droid = {
      url = "github:t184256/nix-on-droid";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
    nur.url = "github:nix-community/NUR";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
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
    , nixpkgs-wayland
    , flake-compat
    , flake-utils
    , nix-on-droid
    , sops-nix
    , ...
    }:
    let import-dir = dir: file: builtins.listToAttrs (builtins.attrValues (builtins.mapAttrs
      (name: value:
        if value == "directory" then {
          name = name;
          value = import (dir + "/${name}/${file}");
        } else {
          name = builtins.substring 0 (builtins.stringLength name - 4) name;
          value = import (dir + "/${name}");
        })
      (builtins.readDir dir))
    ); in
    {
      nixosModules = (import-dir ./module "module.nix");
    } // flake-utils.lib.eachDefaultSystem (system:
    let pkgs = import nixpkgs {
      inherit system;
      config = { allowUnfree = true; };
      overlays = with builtins; ([
        nur.overlay
        fenix.overlay
        nixpkgs-wayland.overlay
        (final: prev: {
          unstable = import inputs.nixpkgs-unstable { system = final.system; config = { allowUnfree = true; }; };
          scripts = (map
            (f: prev.writeScriptBin f (readFile (./scripts + "/${f}")))
            (attrNames (readDir ./scripts)));
        } // (listToAttrs (map (name: { inherit name; value = final.callPackage (./packages + "/${name}") { }; }) (attrNames (readDir ./packages)))))
      ] ++ (map (name: import (./overlays + "/${name}"))
        (attrNames (readDir ./overlays))));
    }; in
    {
      packages = pkgs // {
        all =
          let profiles = self.outputs.packages.${system}; in
          pkgs.stdenv.mkDerivation rec{
            name = "all";
            src = ./.;
            buildInputs = (builtins.map (profile: profile.config.system.build.toplevel) (builtins.attrValues (profiles.nixosConfigurations)))
            ++ (builtins.map (profile: profile.activationPackage) (builtins.attrValues (profiles.homeConfigurations)));
            installPhase = ''
              mkdir $out
              echo $buildInputs >> $out/all
            '';
          };
        nixosModules = (import-dir ./module "module.nix");
        nixosConfigurations = builtins.mapAttrs
          (name: value: nixpkgs.lib.nixosSystem {
            inherit pkgs system;
            specialArgs = inputs // { inherit inputs system; };
            modules = [
              value
              sops-nix.nixosModules.sops
              home-manager.nixosModules.home-manager
            ];
          })
          (import-dir ./machine "machine.nix");
        nixOnDroidConfigurations = builtins.mapAttrs (name: value: nix-on-droid.lib.nixOnDroidConfiguration (value (inputs // { inherit pkgs inputs system; }))) (import-dir ./nix-on-droid/profiles "profile.nix");
        homeConfigurations = builtins.mapAttrs
          (name: value: home-manager.lib.homeManagerConfiguration
            (value (inputs // { inherit inputs system pkgs; })))
          (import-dir ./home-manager/profiles "home.nix");
      };
      devShell = pkgs.mkShell {
        buildInputs = with pkgs;
          [ sops gnumake git rnix-lsp nixfmt nix-du sumneko-lua-language-server nixos-generators ];
      };
      apps.repl = flake-utils.lib.mkApp {
        drv = pkgs.writeShellScriptBin "repl" ''
          confnix=$(mktemp)
          echo "(builtins.getFlake (toString $(git rev-parse --show-toplevel))).vars.${system}
            //(builtins.getFlake (toString $(git rev-parse --show-toplevel))).packages.${system}
            //(builtins.getFlake (toString $(git rev-parse --show-toplevel)))" >$confnix
          trap "rm $confnix" EXIT
          nix repl $confnix
        '';
      };
      vars = {
        pkgs = pkgs;
        lib = pkgs.lib;
        unstable = pkgs.unstable;
        nur = pkgs.nur;
      };
    });
}
