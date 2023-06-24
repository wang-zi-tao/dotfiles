{ config, pkgs, lib, nixpkgs, nixpkgs-unstable, nur, ... }:
let hostName = config.networking.hostName; in
{
  nix = {
    settings.substituters = [
      "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
      "https://cache.nixos.org/"
      "https://nix-community.cachix.org"
      "https://nixpkgs-wayland.cachix.org"
      "https://mirrors.ustc.edu.cn/nix-channels/store"
      # "root@aliyun-hk:64022"
    ];
    settings.trusted-substituters = [
      "https://hydra.nixos.org/"
      # "ssh://root@aliyun-hk.wg:64022"
      # "ssh://root@aliyun-ecs.wg:64022"
      # "ssh://root@aliyun-hk:64022"
      # "ssh://root@aliyun-ecs:64022"
    ];
    settings.trusted-public-keys = [
      # "47.243.22.114:5000:wfL5ei3BfHGUVpiOihncv1LmbBzjqDm6uTFtJ95wueI="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
    ];
    settings.auto-optimise-store = true;
    settings.max-jobs = "auto";
    settings.stalled-download-timeout = 60;
    daemonIOSchedClass = "idle";
    daemonCPUSchedPolicy = "batch";
    extraOptions = "experimental-features = nix-command flakes";
    gc.automatic = true;
    gc.dates = "weekly";
    gc.options = "-d --delete-older-than 7d";
    optimise.automatic = true;
  };
  networking.proxy.noProxy = "mirrors.tuna.tsinghua.edu.cn,mirrors.ustc.edu.cn,127.0.0.1,localhost";

  # environment.memoryAllocator.provider = "jemalloc";
  nixpkgs.config.allowUnfree = true;
  time.timeZone = "Asia/Shanghai";
  nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
  nix.registry.n.flake = nixpkgs;
  nix.registry.nixpkgs.flake = nixpkgs;
  nix.registry.u.flake = nixpkgs-unstable;
  nix.registry.unstable.flake = nixpkgs-unstable;
  nix.registry.nur.flake = nur;
  system.autoUpgrade = {
    enable = false;
    flake = "github:wang-zi-tao/dotfiles";
    randomizedDelaySec = "30min";
    dates = "12:00";
  };
  system.stateVersion = "22.11";
  programs.nix-ld.enable = true;
}

