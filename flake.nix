{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-22-11.url = "github:nixos/nixpkgs/release-22.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    master.url = "github:nixos/nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-23.05";
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
    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
    };
    fenix = { url = "github:nix-community/fenix"; };
    sops-nix.url = "github:Mic92/sops-nix";
    flake-utils.url = "github:numtide/flake-utils";
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixseparatedebuginfod.url = "github:symphorien/nixseparatedebuginfod";
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
    , disko
    , nixseparatedebuginfod
    , master
    , ...
    }:
    let
      import-dir = dir: file: builtins.listToAttrs (builtins.attrValues (builtins.mapAttrs
        (name: value:
          if value == "directory" then {
            inherit name;
            value = import (dir + "/${name}/${file}");
          } else {
            name = builtins.substring 0 (builtins.stringLength name - 4) name;
            value = import (dir + "/${name}");
          })
        (builtins.readDir dir))
      );
      overlays = with builtins; map (name: import (./overlays + "/${name}")) (attrNames (readDir ./overlays));
      packages = final: prev: with builtins;listToAttrs (map (name: { inherit name; value = final.callPackage (./packages + "/${name}") { }; }) (attrNames (readDir ./packages)));
      pkgs-config = system: {
        inherit system;
        config = { allowUnfree = true; };
        overlays = overlays ++ [ packages ];
      };
      pkgs-template = system: import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
          permittedInsecurePackages = [
            "nodejs-16.20.2"
          ];
        };
        overlays = with builtins; ([
          packages
          deploy-rs.overlay
          nur.overlay
          fenix.overlays.default
          # nixpkgs-wayland.overlay
          nixseparatedebuginfod.overlays.default
          (final: prev: {
            unstable = import inputs.nixpkgs-unstable { inherit system overlays; config = { allowUnfree = true; }; };
            master = import inputs.master { inherit system overlays; config = { allowUnfree = true; }; };
            nixpkgs-22-11 = import inputs.nixpkgs-22-11 { inherit system overlays; config = { allowUnfree = true; }; };
            flake-inputs = inputs; 
            scripts = map
              (f: prev.writeScriptBin f (readFile (./scripts + "/${f}")))
              (attrNames (readDir ./scripts));
          } // (listToAttrs (map (name: { inherit name; value = final.callPackage (./packages + "/${name}") { }; }) (attrNames (readDir ./packages)))))
        ] ++ overlays);
      };
    in
    flake-utils.lib.eachDefaultSystem
      (system:
      let pkgs = pkgs-template system; in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            sops
            git
            rnix-lsp
            nixfmt
            nix-du
            sumneko-lua-language-server
            statix
            nixos-generators
            (wangzi-neovim.override { enable-all = false; })
            pkgs.home-manager
          ];
        };
        apps.repl = flake-utils.lib.mkApp {
          drv = pkgs.writeShellScriptBin "repl" ''
            confnix=$(mktemp)
            echo "
                let flake=builtins.getFlake (toString $(git rev-parse --show-toplevel));
                in (flake.vars.${system} // flake.outputs // flake.inputs // flake)
              " >$confnix
            trap "rm $confnix" EXIT
            nix repl $confnix
          '';
        };
        apps.deploy-rs = deploy-rs.defaultApp.${system};
        vars = {
          inherit pkgs;
          inherit (pkgs) lib unstable nur;
        };
        homeConfigurations = builtins.mapAttrs
          (name: value: home-manager.lib.homeManagerConfiguration {
            modules = [ value ];
            inherit pkgs;
            extraSpecialArgs = inputs;
          })
          (import-dir ./home-manager/profiles "home.nix");
        all = pkgs.stdenv.mkDerivation rec{
          name = "all";
          src = ./.;
          nixos = builtins.map (profile: profile.config.system.build.toplevel) (builtins.attrValues self.nixos);
          home = builtins.map (profile: profile.activationPackage) (builtins.attrValues self.homeConfigurations.${system});
          installPhase = ''
            mkdir $out
            echo $nixos >> $out/nixos
            echo $hoe >> $out/home
          '';
        };
      })
    // {
      nixos = builtins.mapAttrs
        (name: value: value ({ inherit pkgs-template; } // inputs))
        (import-dir ./machine "machine.nix");
      # nixosConfigurations = builtins.mapAttrs
      #   (name: value: value ({ inherit pkgs-template; } // inputs))
      #   (import-dir ./machine "machine.nix");
      nixOnDroidConfigurations = builtins.mapAttrs
        (name: value: (value (inputs // { inherit pkgs-template; })))
        (import-dir ./nix-on-droid/profiles "profile.nix");
      nixosModules = import-dir ./module "module.nix";
      deploy.nodes = builtins.listToAttrs (builtins.map
        (host: {
          name = host;
          value = {
            hostname = "${host}";
            profiles.system = {
              sshUser = "root";
              path = deploy-rs.lib.${self.nixos.${host}.pkgs.system}.activate.nixos self.nixos.${host};
            };
          };
        }) [ "wangzi-nuc" "aliyun-hk" "aliyun-ecs" /*"huawei-ecs"*/ ]);
    };
}
