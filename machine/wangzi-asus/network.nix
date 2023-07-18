{ config, pkgs, lib, ... }: {
  systemd.services.NetworkManager-wait-online.enable = false;
  networking = {
    networkmanager = {
      enable = true;
    };
  };
  services.samba-wsdd.enable = true;
  services.samba = {
    enable = true;
    securityType = "user";
    extraConfig = ''
      workgroup = WORKGROUP
      server string = 192.168.16.11
      netbios name = wangzi-pc
      #use sendfile = yes
      #max protocol = smb2
      # note: localhost is the ipv6 localhost ::1
      hosts allow = 192.168.122. 127.0.0.1 localhost
      hosts deny = 0.0.0.0/0
      guest account = wangzi
      map to guest = bad user
      follow symlinks = yes
      wide links = yes
      allow insecure wide links = yes
    '';
    shares = {
      wangzi-home = {
        path = "/home/wangzi";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        "create mask" = "0644";
        "directory mask" = "0755";
        "follow symlinks" = "yes";
        "wide links" = "yes";
      };
      nix-store = {
        path = "/nix/store";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        "create mask" = "0644";
        "directory mask" = "0755";
        "follow symlinks" = "yes";
        "wide links" = "yes";
      };
    };
  };
}
