inputs@{
  pkgs,
  lib,
  config,
  nixpkgs,
  nur,
  nixpkgs-unstable,
  ...
}:
{
  environment.packages = with pkgs; [
    gnugrep
    gnused
  ];
  home-manager.config = import ../home-manager/profiles/wangzi-mobile.nix;
  home-manager.useGlobalPkgs = true;
  user.shell = "${pkgs.zsh}/bin/zsh";
  system.stateVersion = "22.05";
  terminal.font = "${pkgs.iosevka-nerd}/share/fonts/truetype/IosevkaTermNerdFontComplete.ttf";
  time.timeZone = "Asia/Shanghai";
  nix = {
    substituters = [
      "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
      "https://cache.nixos.org/"
      "https://nix-community.cachix.org"
      "https://nixpkgs-wayland.cachix.org"
      "https://mirrors.ustc.edu.cn/nix-channels/store"
    ];
    trustedPublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
    ];
    extraOptions = "experimental-features = nix-command flakes";
    registry.n.flake = nixpkgs;
    registry.nixpkgs.flake = nixpkgs;
    registry.u.flake = nixpkgs-unstable;
    registry.unstable.flake = nixpkgs-unstable;
    registry.nur.flake = nur;
  };
  environment.etc.nixos = {
    source = ../.;
  };
}
