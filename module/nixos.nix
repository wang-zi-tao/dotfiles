{ config, pkgs, lib, nixpkgs, nixpkgs-unstable, nur, ... }:
{
  imports = [ ./nix.nix ];
  networking.proxy.noProxy = "mirrors.tuna.tsinghua.edu.cn,mirrors.ustc.edu.cn,127.0.0.1,localhost";
  # environment.memoryAllocator.provider = "jemalloc";
  nixpkgs.config.allowUnfree = true;
  time.timeZone = "Asia/Shanghai";
  system.autoUpgrade = {
    enable = false;
    flake = "github:wang-zi-tao/dotfiles";
    randomizedDelaySec = "30min";
    dates = "12:00";
  };
  environment.etc.nixos = {
    source = ../.;
  };
  system.stateVersion = "22.05";
  programs.nix-ld.enable = true;
}

