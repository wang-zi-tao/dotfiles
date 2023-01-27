{ config, pkgs, lib, ... }:
let
  nodeConfig = config.cluster.nodes."${config.cluster.nodeName}";
  sops-enable = config.sops.defaultSopsFile != "/";
in
{
  config = lib.mkIf config.cluster.nodeConfig.sshd.enable {
    programs.ssh.forwardX11 = true;
    sops.secrets."ssh-public-keys" = lib.mkIf sops-enable {
      sopsFile = config.cluster.ssh.publicKeySops;
    };
    nix.sshServe.enable = true;
    nix.settings.trusted-users = [ "nix-ssh" ];
    nix.sshServe.write = true;
    services.openssh = {
      enable = true;
      forwardX11 = true;
      gatewayPorts = "yes";
      permitRootLogin = "yes";
      passwordAuthentication = true;
      authorizedKeysFiles = lib.optional sops-enable config.sops.secrets.ssh-public-keys.path;
      extraConfig = ''
        TCPKeepAlive yes
        ClientAliveCountMax 360
        ClientAliveInterval 360
      '';
      ports = [ 22 64022 ];
      openFirewall = true;
    };
    programs.ssh.extraConfig = ''
      ControlMaster auto
      ControlPath /tmp/ssh_mux_%h_%p_%r
      ControlPersist yes
      ServerAliveInterval 360
    '';
    services.sshguard = {
      enable = true;
      whitelist = [ "192.168.0.0/16" ];
    };
  };
}
