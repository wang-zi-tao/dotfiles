{ config, pkgs, lib, ... }: {
  config = lib.mkIf config.cluster.nodeConfig.shell.enable {
    programs.zsh.enable = true;
    environment.systemPackages = with pkgs; [ nix-direnv uutils-coreutils pciutils ];
  };
}
