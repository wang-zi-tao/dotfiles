{ config, pkgs, lib, ... }: {
  imports = [ ../secret/secret.nix ];
  programs.ssh.forwardX11 = true;
  services.openssh = {
    enable = true;
    forwardX11 = true;
    gatewayPorts = "yes";
    passwordAuthentication = true;
    startWhenNeeded = true;
    authorizedKeysFiles = config.secret.ssh-public-keys-file;
    extraConfig = "";
  };
  boot.initrd.network.ssh.enable = true;
  boot.initrd.network.ssh.shell = "${pkgs.zsh}/bin/zsh";
}
