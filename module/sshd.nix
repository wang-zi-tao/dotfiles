{ config, pkgs, lib, ... }:
let nodeConfig = config.cluster.nodes."${config.cluster.nodeName}";
in
{
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
      extraConfig = ''
        TCPKeepAlive yes
        ClientAliveCountMax 360
        ClientAliveInterval 360
      '';
    };
    boot.initrd.network.ssh.enable = true;
    boot.initrd.network.ssh.shell = "${pkgs.zsh}/bin/zsh";
    programs.ssh.extraConfig = ''
      ControlMaster auto
      ControlPath /tmp/ssh_mux_%h_%p_%r
      ControlPersist yes
      ServerAliveInterval 360
    '';
  };
}
