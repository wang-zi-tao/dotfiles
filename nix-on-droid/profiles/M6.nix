inputs@{ pkgs-template, nix-on-droid, ... }:
let
  hostname = "wangzi-M6";
  system = "aarch64-linux";
  pkgs = pkgs-template system;
in
nix-on-droid.lib.nixOnDroidConfiguration {
  modules = [ ../mobile.nix ];
  extraSpecialArgs = inputs;
  inherit pkgs;
}
