{ pkgs, config, lib, ... }:
let nodeConfig = config.cluster.nodes."${config.cluster.nodeName}";
in
{
  networking = {
    # proxy.default = "http://192.168.16.2:8889";
    proxy.noProxy = "mirrors.ustc.edu.cn,mirrors.tuna.tsinghua.edu.cn,127.0.0.1,localhost,.localdomain";
    firewall.enable = false;
    hosts = (builtins.listToAttrs (builtins.map
      ({ hostname, publicIp, ... }: {
        name = publicIp;
        value = [ hostname ];
      })
      (builtins.filter ({ publicIp, ... }: publicIp != null)
        (builtins.attrValues config.cluster.nodes)))) //
    (builtins.listToAttrs (builtins.map
      ({ hostname, localIp, ... }: {
        name = localIp;
        value = [ hostname ];
      })
      (builtins.filter ({ localIp, publicIp, ... }: localIp != null && publicIp == null)
        (builtins.attrValues config.cluster.nodes)))) //
    (builtins.listToAttrs
      (builtins.map
        ({ hostname, wireguard, ... }: {
          name = wireguard.clusterIp;
          value = [ "${hostname}.wg" ];
        })
        (builtins.filter ({ wireguard, ... }: wireguard.enable)
          (builtins.attrValues config.cluster.nodes)))) //
    (builtins.listToAttrs
      (builtins.map
        ({ hostname, wireguard, ... }: {
          name = builtins.replaceStrings [ ".16." ] [ ".17." ] wireguard.clusterIp;
          value = [ "${hostname}.wg1" ];
        })
        (builtins.filter ({ wireguard, ... }: wireguard.enable)
          (builtins.attrValues config.cluster.nodes)))) //
    { "127.0.0.1" = [ config.networking.hostName "localhost.wangzicloud.cn" ]; };
  };
  sops.secrets."wireguard/private-key" =
    lib.mkIf nodeConfig.wireguard.enable { };
  networking.wg-quick = lib.mkIf nodeConfig.wireguard.enable {
    interfaces = {
      wg0 = {
        preUp = lib.optionalString nodeConfig.wireguard.iptables.enable "${pkgs.iptables}/bin/iptables -A FORWARD -i wg0 -j ACCEPT; ${pkgs.iptables}/bin/iptables -A FORWARD -o wg0 -j ACCEPT; ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -o ens3 -j MASQUERADE";
        postDown = lib.optionalString nodeConfig.wireguard.iptables.enable "${pkgs.iptables}/bin/iptables -D FORWARD -i wg0 -j ACCEPT; ${pkgs.iptables}/bin/iptables -D FORWARD -o wg0 -j ACCEPT; ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -o ens3 -j MASQUERADE";
        address = [ nodeConfig.wireguard.clusterIp ];
        listenPort = nodeConfig.wireguard.port;
        privateKeyFile = config.sops.secrets."wireguard/private-key".path;
        mtu = 1600;
        peers = map
          (node:
            let w = node.wireguard;
            in
            {
              persistentKeepalive = 32;
              allowedIPs =
                [ w.clusterIp ] ++
                (lib.lists.optionals (node.hostname == nodeConfig.wireguard.gateway)
                  (builtins.map
                    (p: p.wireguard.clusterIp)
                    (builtins.filter
                      (p: p.wireguard.enable && p.publicIp == null && p.localIp == null)
                      (builtins.attrValues config.cluster.nodes))));
              # allowedIPs = [ w.clusterIp ] ++ (lib.lists.optional "192.168.16.0/24");
              publicKey = w.publicKey;
              endpoint =
                if nodeConfig.wireguard.tunnel then
                  "127.0.0.1:${builtins.toString (w.index + 40000)}"
                else if node.publicIp != null then
                  "${node.publicIp}:${builtins.toString w.port}"
                else if node.localIp != null then
                  "${node.localIp}:${builtins.toString w.port}"
                else
                  null;
            })
          (builtins.filter (node: node.wireguard.enable)
            (builtins.attrValues config.cluster.nodes));
      };
    };
  };

  systemd.services =
    (lib.optionalAttrs (nodeConfig.wireguard.enable && nodeConfig.wireguard.tunnel)
      (builtins.listToAttrs
        (builtins.map
          (node: lib.nameValuePair "wireguard-tunnel-client-${node.hostname}" (
            let ip = if node.publicIp != null then node.publicIp else if node.localIp != null then node.localIp else null; in
            {
              enable = true;
              wantedBy = [ "multi-user.target" ];
              before = [ "multi-user.target" ];
              path = with pkgs; [ busybox openssh ];
              serviceConfig = {
                Type = "simple";
                Restart = "always";
                RestartSec = "5s";
                LimitNOFILE = 500000;
                LimitNPROC = 500000;
                ExecStart = "${pkgs.udp2raw}/bin/udp2raw --fix-gro -k qMQ9rUOA --raw-mode faketcp --cipher-mode xor --auth-mode simple -c -l127.0.0.1:${builtins.toString (node.wireguard.index + 40000)} -r ${ip}:${builtins.toString (nodeConfig.wireguard.index + 40000)}";
              };
            }
          ))
          (builtins.filter (node: node.wireguard.enable && node.hostname != nodeConfig.hostname && (node.publicIp != null || node.localIp != null)) (builtins.attrValues config.cluster.nodes)))))
    // (builtins.listToAttrs
      (builtins.map
        (node: lib.nameValuePair "wireguard-tunnel-server-${node.hostname}" (
          let ip = if node.publicIp != null then node.publicIp else if node.localIp != null then node.localIp else null; in
          {
            enable = true;
            wantedBy = [ "multi-user.target" ];
            before = [ "multi-user.target" ];
            path = with pkgs; [ busybox openssh ];
            serviceConfig = {
              Type = "simple";
              Restart = "always";
              RestartSec = "5s";
              LimitNOFILE = 500000;
              LimitNPROC = 500000;
              ExecStart = "${pkgs.udp2raw}/bin/udp2raw --fix-gro -k qMQ9rUOA --raw-mode faketcp --cipher-mode xor --auth-mode simple -s -l0.0.0.0:${builtins.toString (node.wireguard.index + 40000)} -r 127.0.0.1:${builtins.toString nodeConfig.wireguard.port}";
            };
          }
        ))
        (builtins.filter (node: node.wireguard.enable && node.hostname != nodeConfig.hostname && node.wireguard.tunnel) (builtins.attrValues config.cluster.nodes))))

    // {
      wg-netmanager = lib.mkIf nodeConfig.wireguard.enable
        {
          description = "Wireguard network manager wg1";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          path = with pkgs; [ wireguard-tools iproute2 ];
          serviceConfig = {
            Type = "simple";
            Restart = "always";
            ExecStart = "${pkgs.unstable.wg-netmanager}/bin/wg_netmanager ${if (nodeConfig.publicIp == null) then "-H" else "-w ${builtins.toString (nodeConfig.wireguard.port+1)}"} -v -v";
            ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
            ExecStop = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
            ReadWritePaths = [ "/tmp" ];
          };
          restartTriggers = [ "/etc/wg_netmanager/network.yaml" "/etc/wg_netmanager/peer.yaml" ];
        };
    };
  environment.etc."wg_netmanager/peer.yaml".text = lib.optionalString nodeConfig.wireguard.enable (lib.generators.toYAML { } {
    wgInterface = "wg1";
    wgIp = builtins.replaceStrings [ ".16." ] [ ".17." ] nodeConfig.wireguard.clusterIp;
    name = nodeConfig.hostname;
    existingInterface = false;
  });
  environment.etc."wg_netmanager/network.yaml".text = lib.optionalString nodeConfig.wireguard.enable
    (lib.generators.toYAML
      { }
      {
        network = {
          sharedKey = "qMHMtszyM8tbv4Tin1Ny6CgGiygEoWDARyaY1u+p000=";
          subnet = "192.168.17.0/24";
        };
        peers =
          (builtins.map
            (node: {
              endPoint =
                if node.publicIp != null then
                  "${node.publicIp}:${builtins.toString (node.wireguard.port+1)}"
                else
                  "${node.localIp}:${builtins.toString (node.wireguard.port+1)}";
              adminPort = 52343;
              wgIp = builtins.replaceStrings [ ".16." ] [ ".17." ] node.wireguard.clusterIp;
            })
            (builtins.filter (p: p.wireguard.enable && (p.publicIp != null) && p.wireguard.gatewayServer) (builtins.attrValues config.cluster.nodes)));
      });
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
    "net.ipv4.conf.all.proxy_arp" = 1;
  };
  environment.systemPackages = with pkgs; [
    wireguard-tools
  ];
}
