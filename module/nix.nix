{ config, pkgs, lib, nixpkgs, nixpkgs-unstable, nur, ... }: {
  nix = {
    settings = {
      substituters = [
        "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
        "https://cache.nixos.org/"
        "https://nix-community.cachix.org"
        "https://nixpkgs-wayland.cachix.org"
        "https://mirrors.ustc.edu.cn/nix-channels/store"
      ];
      trusted-substituters = [ ];
      trusted-public-keys = [
        # "47.243.22.114:5000:wfL5ei3BfHGUVpiOihncv1LmbBzjqDm6uTFtJ95wueI="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
      ];
    };
    extraOptions = "experimental-features = nix-command flakes";
    gc = {
      automatic = true;
      dates = "weekly";
      options = "-d --delete-older-than 7d";
    };
    optimise.automatic = true;
    nixPath = [ "nixpkgs=${nixpkgs}" ];
    registry = {
      n.flake = nixpkgs;
      nixpkgs.flake = nixpkgs;
      u.flake = nixpkgs-unstable;
      unstable.flake = nixpkgs-unstable;
      nur.flake = nur;
    };
  };
}
