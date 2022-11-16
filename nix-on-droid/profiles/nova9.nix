inputs@{ pkgs-template, nixpkgs, home-manager, nix-on-droid, ... }:
let
  hostname = "wangzi-nova9";
  system = "aarch64-linux";
  pkgs = pkgs-template system;
in
nix-on-droid.lib.nixOnDroidConfiguration {
  config = { pkgs, config, ... }: {
    imports = [ ../mobile.nix ];
  };
  inherit pkgs system;
}
