{ config, pkgs, lib, ... }: {
  config = lib.mkIf config.cluster.nodeConfig.container.enable {
    environment.etc."docker/daemon.json".text = ''
      {
        "registry-mirrors": [
          "https://mirror.ccs.tencentyun.com",
          "https://registry.docker-cn.com",
          "https://registry.cn-hangzhou.aliyuncs.com"
        ]
      }
    '';
    virtualisation = {
      podman = {
        enable = true;
        dockerCompat = true;
        enableNvidia = config.services.xserver.enable;
      };
      lxd = {
        enable = true;
        package = pkgs.unstable.lxd;
        recommendedSysctlSettings = true;
      };
      libvirtd = { enable = true; };
    };
    environment.systemPackages = with pkgs; [ criu ];
    systemd.services.k3s = {
      enable = false;
      description = "k3s.wangzicloud.cn";
      wantedBy = [ "multi-user.target" ];
      path = [
        pkgs.wireguard-tools
        pkgs.busybox
        pkgs.bash
        (pkgs.writeScriptBin "wg-add.sh" ./scripts/wg-add.sh)
      ];
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = "2s";
        wants = [ "network-online.target" ];
        after = [ "network-online.target" ];
        ExecStart =
          "${pkgs.k3s}/bin/k3s agent --node-taint mobile=true:NoSchedule --server https://116.62.23.116:6443 --token 'K1080728ddd8df3515ba3f9be378dbe1ff69f3a22f3d88bf3a67726e7885d53bf63::server:6608812d75e0de2af2fcfdb925a5f3f5' ";
      };
    };
  };
}
