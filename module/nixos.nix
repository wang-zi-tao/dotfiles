{ config, pkgs, lib, ... }:
with pkgs.flakes; {
  nix = {
    binaryCaches = [
      "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
      "https://cache.nixos.org/"
    ];
    buildCores = 10;
    extraOptions = "experimental-features = nix-command flakes";
    package = pkgs.nixFlakes;
    gc.automatic = true;
    gc.dates = "weekly";
    gc.options = "-d";
    optimise.automatic = true;
  };
  # environment.memoryAllocator.provider = "jemalloc";
  nixpkgs.config.allowUnfree = true;
  time.timeZone = "Asia/Shanghai";
  i18n.defaultLocale = "zh_CN.UTF-8";
  nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
  nix.registry.n.flake = nixpkgs;
  nix.registry.nixpkgs.flake = nixpkgs;
  nix.registry.u.flake = nixpkgs-unstable;
  nix.registry.unstable.flake = nixpkgs-unstable;
  nix.registry.nur.flake = nur;
}
