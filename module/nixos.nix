{ config, pkgs, lib, ... }:
with pkgs.flakes; {
  nix = {
    binaryCaches = [
      "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
      "https://mirrors.ustc.edu.cn/nix-channels/store"
      "https://nix-community.cachix.org"
      "https://cache.nixos.org/"
      "https://nixpkgs-wayland.cachix.org"
      /* "https://mirror.sjtu.edu.cn/nix-channels/store" */
    ];
    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
    ];
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
  system.autoUpgrade = {
    enable = true;
    flake = "ssh://wangzi@wangzi-nuc.wg/home/wangzi/workspace/nixos/";
    randomizedDelaySec = "30min";
    dates = "12:00";
  };
  system.stateVersion = "22.05";
  systemd.services.run-secrets-scripts = {
    serviceConfig.Type = "oneshot";
    path = with pkgs; [ busybox nix ];
    script = ''
      if [[ -e /run/secrets/script ]];then
        /run/secrets/script
      fi
    '';
  };
}
