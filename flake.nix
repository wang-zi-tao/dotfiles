{
  description = "NixOS configuration for all machines in wangzicloud.cn";
  inputs = {
    home-manager.url = "github:nix-community/home-manager/release-22.05";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    deploy-rs = {
      url = "github:serokell/deploy-rs";
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
    , flake-utils
    , nix-on-droid
    , sops-nix
    , deploy-rs
    , ...
    }:
    let
      import-dir = dir: file: builtins.listToAttrs (builtins.attrValues (builtins.mapAttrs
        (name: value:
          if value == "directory" then {
            name = name;
            value = import (dir + "/${name}/${file}");
          } else {
            name = builtins.substring 0 (builtins.stringLength name - 4) name;
            value = import (dir + "/${name}");
          })
        (builtins.readDir dir))
      );
      pkgs-template = system: import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        overlays = with builtins; ([
          deploy-rs.overlay
          nur.overlay
          fenix.overlays.default
          nixpkgs-wayland.overlay
          (final: prev: {
            unstable = import inputs.nixpkgs-unstable { system = final.system; config = { allowUnfree = true; }; };
            scripts = (map
              (f: prev.writeScriptBin f (readFile (./scripts + "/${f}")))
              (attrNames (readDir ./scripts)));
          } // (listToAttrs (map (name: { inherit name; value = final.callPackage (./packages + "/${name}") { }; }) (attrNames (readDir ./packages)))))
        ] ++ (map (name: import (./overlays + "/${name}"))
          (attrNames (readDir ./overlays))));
      };
    in
    flake-utils.lib.eachDefaultSystem
      (system:
      let pkgs = pkgs-template system; in
      {
        inherit pkgs;
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [ sops gnumake git rnix-lsp nixfmt nix-du sumneko-lua-language-server nixos-generators wangzi-neovim pkgs.home-manager deploy-rs.defaultPackage.${system} ];
        };
        apps.repl = flake-utils.lib.mkApp {
          drv = pkgs.writeShellScriptBin "repl" ''
            confnix=$(mktemp)
            echo "(builtins.getFlake (toString $(git rev-parse --show-toplevel))).vars.${system}
              //(builtins.getFlake (toString $(git rev-parse --show-toplevel)))" >$confnix
            trap "rm $confnix" EXIT
            nix repl $confnix
          '';
        };
        apps.deploy-rs = deploy-rs.defaultApp.${system};
        vars = {
          pkgs = pkgs;
          lib = pkgs.lib;
          unstable = pkgs.unstable;
          nur = pkgs.nur;
        };
        homeConfigurations = builtins.mapAttrs
          (name: value: home-manager.lib.homeManagerConfiguration
            (value (inputs // { inherit inputs system pkgs; })))
          (import-dir ./home-manager/profiles "home.nix");
      })
    // {
      nixosConfigurations = builtins.mapAttrs
        (name: value: value ({ inherit pkgs-template; } // inputs))
        (import-dir ./machine "machine.nix");
      nixOnDroidConfigurations = builtins.mapAttrs
        (name: value: (value (inputs // { inherit pkgs-template; })))
        (import-dir ./nix-on-droid/profiles "profile.nix");
      nixosModules = (import-dir ./module "module.nix");
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
      deploy.nodes = builtins.listToAttrs (builtins.map
        (host: {
          name = host;
          value = {
            hostname = "${host}.wg";
            profiles.system = {
              sshUser = "root";
              path = deploy-rs.lib.${self.nixosConfigurations.${host}.pkgs.system}.activate.nixos self.nixosConfigurations.${host};
            };
          };
        }) [ "wangzi-pc" "wangzi-nuc" "aliyun-hk" "aliyun-ecs" "huawei-ecs" ]);
    };
}

