{ config, pkgs, lib, ... }: {
  systemd.services.n2n_edge = {
    enable = true;
    description = "wangzi n2n network";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = "5s";
      ExecStart =
        "${pkgs.unstable.n2n}/bin/edge -t 20000 -d n2n -a 192.168.0.3 -c n2n -A1 -l 139.9.235.87:49 -r -f";
    };
  };
  networking = {
    #useDHCP = true;
    hostName = "wangzi-pc";
    firewall.enable = false;
    networkmanager = { enable = true; };
    wireguard = { enable = true; };
    proxy.default = "http://127.0.0.1:8889";
    hosts = {
      "127.0.0.1" = [ "wangzi-pc.wangzicloud.cn" ];
      "116.62.23.116" = [ "aliyun.wangzicloud.cn" "aliyun" ];
      "139.9.235.87" = [ "huawei-cloud.wangzicloud.cn" "huawei-cloud" ];
    };
  };
  systemd.services.NetworkManager-wait-online.enable = false;
  environment.systemPackages = with pkgs; [
    wireguard
    wireguard-tools
    seaweedfs
  ];
}
