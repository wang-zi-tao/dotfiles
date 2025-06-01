{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-old.url = "github:nixos/nixpkgs/release-24.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    master.url = "github:nixos/nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    nixos-wsl.url = "github:nix-community/NixOS-WSL/main";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    deploy-rs = {
      url = "github:serokell/deploy-rs";
    };
    nix-on-droid = {
      url = "github:t184256/nix-on-droid";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/nix-darwin-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/NUR";
    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
    };
    fenix = {
      url = "github:nix-community/fenix";
    };
    sops-nix.url = "github:Mic92/sops-nix";
    flake-utils.url = "github:numtide/flake-utils";
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    eza.url = "github:eza-community/eza";
    nixfs.url = "github:illustris/nixfs";
    NixVirt = {
      url = "https://flakehub.com/f/AshleyYakeley/NixVirt/*.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    inputs@{
      self,
      home-manager,
      nur,
      fenix,
      nixpkgs,
      nixpkgs-wayland,
      nixos-wsl,
      flake-utils,
      nix-on-droid,
      sops-nix,
      deploy-rs,
      disko,
      eza,
      master,
      nixfs,
      NixVirt,
      ...
    }:
    let
      import-dir =
        dir: file:
        builtins.listToAttrs (
          builtins.attrValues (
            builtins.mapAttrs (
              name: value:
              if value == "directory" then
                {
                  inherit name;
                  value = import (dir + "/${name}/${file}");
                }
              else
                {
                  name = builtins.substring 0 (builtins.stringLength name - 4) name;
                  value = import (dir + "/${name}");
                }
            ) (builtins.readDir dir)
          )
        );
      overlays =
        with builtins;
        map (name: import (./overlays + "/${name}")) (attrNames (readDir ./overlays));
      packages =
        final: prev:
        with builtins;
        listToAttrs (
          map (name: {
            inherit name;
            value = final.callPackage (./packages + "/${name}") { };
          }) (attrNames (readDir ./packages))
        );
      config = {
        allowUnfree = true;
        permittedInsecurePackages = [ ];
      };
      pkgs-config = system: {
        inherit system config;
        overlays = overlays ++ [ packages ];
      };
      pkgs-template =
        system:
        import nixpkgs {
          inherit system config;
          overlays =
            with builtins;
            (
              [
                packages
                nur.overlay
                fenix.overlays.default
                # nixpkgs-wayland.overlay
                (
                  final: prev:
                  let
                    unstable = import inputs.nixpkgs-unstable { inherit system overlays config; };
                  in
                  {
                    unstable = unstable;
                    master = import inputs.master { inherit system overlays config; };
                    nixpkgs-old = import inputs.nixpkgs-old { inherit system overlays config; };
                    flake-inputs = inputs;
                    eza = eza.packages.${system}.default;
                    atuin = unstable.atuin;
                    scripts = builtins.mapAttrs (
                      name: kind: prev.writeScriptBin name (readFile (./scripts + "/${name}"))
                    ) (readDir ./scripts);
                  }
                  // (listToAttrs (
                    map (name: {
                      inherit name;
                      value = final.callPackage (./packages + "/${name}") { };
                    }) (attrNames (readDir ./packages))
                  ))
                )
              ]
              ++ overlays
            );
        };
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = pkgs-template system;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            sops
            git
            nixfmt-rfc-style
            nix-du
            nix-tree
            nix-diff
            sumneko-lua-language-server
            statix
            nixos-generators
            nix
            (pkgs.wangzi-neovim.override {
              enable-all = true;
            })
            pkgs.home-manager
          ];
        };
        apps.deploy-rs = deploy-rs.defaultApp.${system};
        vars =
          {
            inherit pkgs inputs;
            inherit (pkgs) lib unstable nur;
            outputs = self;
          }
          // inputs
          // self;
        packages = packages pkgs pkgs;
        homeConfigurations = builtins.mapAttrs (
          name: value:
          home-manager.lib.homeManagerConfiguration {
            modules = [ value ];
            inherit pkgs;
            extraSpecialArgs = inputs;
          }
        ) (import-dir ./home-manager/profiles "home.nix");
        all = pkgs.stdenv.mkDerivation rec {
          name = "all";
          src = ./.;
          nixos = builtins.map (profile: profile.config.system.build.toplevel) (
            builtins.attrValues self.nixos
          );
          home = builtins.map (profile: profile.activationPackage) (
            builtins.attrValues self.homeConfigurations.${system}
          );
          installPhase = ''
            mkdir $out
            echo $nixos >> $out/nixos
            echo $hoe >> $out/home
          '';
        };
      }
    )
    // {
      nixos = builtins.mapAttrs (name: value: value ({ inherit pkgs-template; } // inputs)) (
        import-dir ./machine "machine.nix"
      );
      # nixosConfigurations = builtins.mapAttrs
      #   (name: value: value ({ inherit pkgs-template; } // inputs))
      #   (import-dir ./machine "machine.nix");
      nixOnDroidConfigurations = builtins.mapAttrs (
        name: value: (value (inputs // { inherit pkgs-template; }))
      ) (import-dir ./nix-on-droid/profiles "profile.nix");
      nixosModules = import-dir ./module "module.nix";
      darwinConfigurations = builtins.mapAttrs (
        name: value: (value (inputs // { inherit pkgs-template; }))
      ) (import-dir ./darwin "profile.nix");
      deploy.nodes =
        builtins.mapAttrs
          (host: config: {
            hostname = "${host}.wg";
            autoRollback = config.magicRollback;
            magicRollback = config.magicRollback;
            confirmTimeout = 600;
            activationTimeout = 600;
            profiles.system = {
              sshUser = "root";
              path = deploy-rs.lib.${self.nixos.${host}.pkgs.system}.activate.nixos self.nixos.${host};
            };
          })
          {
            "wangzi-asus" = {
              magicRollback = false;
            };
            "wangzi-nuc" = {
              magicRollback = false;
            };
            "wangzi-pc" = {
              magicRollback = false;
            };
            "aliyun-hk" = {
              magicRollback = true;
            };
            "aliyun-ecs" = {
              magicRollback = true;
            };
            # "huawei-ecs"
          };
    };
}
