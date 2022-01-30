{ config, pkgs, lib, ... }:
let nodeConfig = config.cluster.nodes."${config.cluster.nodeName}";
in {
  config = lib.mkIf config.cluster.nodeConfig.sshd.enable {
    programs.ssh.forwardX11 = true;
    sops.secrets."ssh-public-keys" = {
      sopsFile = config.cluster.ssh.publicKeySops;
    };
    services.openssh = {
      enable = true;
      forwardX11 = true;
      gatewayPorts = "yes";
      passwordAuthentication = true;
      startWhenNeeded = true;
      authorizedKeysFiles = [ config.sops.secrets.ssh-public-keys.path ];
    };
    boot.initrd.network.ssh.enable = true;
    boot.initrd.network.ssh.shell = "${pkgs.zsh}/bin/zsh";
  };
}
