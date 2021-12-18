{ config, pkgs, lib, ... }: {
  programs.ssh.forwardX11 = true;
  services.openssh.forwardX11 = true;
  services.sshd.enable=true;
}
