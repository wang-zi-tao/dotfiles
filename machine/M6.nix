{ pkgs, nixpkgs, home-manager, nix-on-droid, ... }:
let hostname = "wangzi-phone";
in
nix-on-droid.lib.nixOnDroidConfiguration {
  config = { pkgs, config, ... }: {
    environment.packages = with pkgs;[ ];
    environment.etcBackupExtension = ".bak";
    home-manager.config = { ... }: {
      imports = [ ../home-manager/terminal/terminal.nix ];
    };
    home-manager.useGlobalPkgs = true;
  };
  system = "aarch64-linux";
  extraModules = [
    # import source out-of-tree modules like:
    # flake.nixOnDroidModules.module
  ];
  extraSpecialArgs = {
    # arguments to be available in every nix-on-droid module
  };
  inherit pkgs;
}
