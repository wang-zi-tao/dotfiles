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
      # 必须对「所有可能登录的用户」可读，不能是 sops 默认的 0400 root:root。
      #
      # 原因：sshd 在打开 AuthorizedKeysFile 之前会把自己降权到目标用户，
      #   openssh auth2-pubkey.c: user_key_allowed2() -> temporarily_use_uid(pw)
      #   openssh uidswap.c:
      #     /*
      #      * Temporarily changes to the given uid.  If the effective user
      #      * id is not root, this does nothing.
      #      */
      #     if (saved_euid != 0) { privileged = 0; return; }
      #     ...
      #     initgroups(pw->pw_name, pw->pw_gid);      // 连附加组一起换成该用户的
      #     setgroups(user_groupslen, user_groups);
      #     seteuid(pw->pw_uid);
      # 所以 root 登录是空操作（能读 0400），非 root 登录则是真实降权：
      #   sshd-session[..]: Could not open user 'wangzi' authorized keys
      #     '/run/secrets/ssh-public-keys': Permission denied
      # sshd 会静默跳过该文件并回退到密码认证 —— 表现为「密钥登录没生效，弹密码」。
      #
      # 修复只能放开读权限（没有保留 0400 的办法：降权后就是普通用户身份在 open）。
      # 内容是公钥（authorized_keys 格式），公开无任何风险；
      # NixOS 自己写 /etc/ssh/authorized_keys.d/%u 用的也正是 mode = "0444"。
      mode = "0444";
    };
    nix.sshServe = {
      enable = true;
      keys = [ ];
    };
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
        MaxStartups 500:30:1000
        AuthorizedKeysFile %h/.ssh/authorized_keys %h/.ssh/authorized_keys2 /etc/ssh/authorized_keys.d/%u ${lib.optionalString sops-enable config.sops.secrets.ssh-public-keys.path}

        Match User *
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
      ControlMaster no
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
