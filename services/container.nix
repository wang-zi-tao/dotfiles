{ config, pkgs, lib, ... }: {
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
    docker = {
      enable = true;
      enableNvidia = config.services.xserver.enable;
      enableOnBoot = true;
      storageDriver = "btrfs";
    };
    lxd = {
      enable = true;
      package = pkgs.unstable.lxd;
      recommendedSysctlSettings = true;
    };
    lxc = {
      enable = true;
      lxcfs.enable = true;
      defaultConfig = ''
        lxc.net.0.type = veth
        lxc.net.0.name = eth0
        lxc.net.0.link = lxcbr0
        lxc.net.0.flags = up
        lxc.net.0.hwaddr = 00:16:3e:xx:xx:xx
        lxc.net.1.name = cluster
        lxc.net.1.type = veth
        lxc.net.1.flags = up
        lxc.net.1.link = lxcbr1
        qwerfvs234
        lxc.idmap = u 0 100000 10000
        lxc.idmap = g 0 100000 10000
        lxc.init.cmd = /sbin/init
        lxc.mount.entry = proc mnt/proc proc create=dir 0 0
      '';
      usernetConfig = "";
    };
    libvirtd = { enable = true; };
    containers.storage.settings = { driver = "btrfs"; };
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
}
