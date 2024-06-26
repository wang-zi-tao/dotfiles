{
  config,
  pkgs,
  lib,
  ...
}:
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
    users.users.nix-ssh = {
      description = "Nix SSH store user";
      isSystemUser = true;
      group = "nix-ssh";
      useDefaultShell = true;
    };
    users.groups.nix-ssh = { };
    # nix.settings.trusted-users = [ "nix-ssh" ];
    services.openssh = {
      enable = true;
      settings = {
        X11Forwarding = true;
        GatewayPorts = "yes";
        PermitRootLogin = "yes";
        PasswordAuthentication = true;
      };
      authorizedKeysFiles = lib.optional sops-enable config.sops.secrets.ssh-public-keys.path;
      extraConfig = ''
        TCPKeepAlive yes
        ClientAliveCountMax 60
        ClientAliveInterval 60
        AuthorizedKeysFile %h/.ssh/authorized_keys %h/.ssh/authorized_keys2 /etc/ssh/authorized_keys.d/%u ${lib.optionalString sops-enable config.sops.secrets.ssh-public-keys.path}

        Match User *
          AuthorizedKeysFile %h/.ssh/authorized_keys %h/.ssh/authorized_keys2 /etc/ssh/authorized_keys.d/%u ${lib.optionalString sops-enable config.sops.secrets.ssh-public-keys.path}

        Match User nix-ssh
          AllowAgentForwarding no
          AllowTcpForwarding no
          PermitTTY no
          PermitTunnel no
          X11Forwarding no
          # ForceCommand nix-store --serve --write
          AuthorizedKeysFile %h/.ssh/authorized_keys %h/.ssh/authorized_keys2 /etc/ssh/authorized_keys.d/%u ${lib.optionalString sops-enable config.sops.secrets.ssh-public-keys.path}

        Match all
      '';
      ports = [
        22
        64022
      ];
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
