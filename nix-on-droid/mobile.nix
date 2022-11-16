{ pkgs, lib, config, ... }:
{
  environment.packages = with pkgs;[ ];
  environment.etcBackupExtension = ".bak";
  home-manager.config = { ... }: (import ../../home-manager/profiles/wangzi-mini.nix (inputs // { inherit pkgs; })).configuration;
  home-manager.useGlobalPkgs = true;
  user.nix-on-droid.shell = "${pkgs.zsh}/bin/zsh";
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
}
