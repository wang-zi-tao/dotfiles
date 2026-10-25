{
  config,
  lib,
  pkgs,
  ...
}:
# cluster-node：把 cluster.* 的集群数据编译成 /etc/cluster-node/config.json5，
# 并常驻运行 cluster-node 守护进程（对等探测 + 拓扑 gossip + 路由计算 + 可选 DNS/隧道）。
let
  cfg = config.services.cluster-node;
  cluster = config.cluster;

  # 单个节点的对等配置：地址、SSH host key、WireGuard 信息。
  mkNode =
    name: _node:
    let
      net = (cluster.network.nodes.${name} or { }).config or { };
      wg = (cluster.wireguard.nodes.${name} or { }).config or { };
      keys = (cluster.nodes.${name} or { }).ssh.hostPublicKeys or [ ];
    in
    {
      localIp = lib.optional (net.localIp or null != null) net.localIp ++ (net.ips or [ ]);
      publicIp = lib.optional (net.publicIp or null != null) net.publicIp;
      speed = net.speed or null;
      hostPubKey = if keys == [ ] then "" else lib.head keys;
    }
    // lib.optionalAttrs (wg ? index) {
      index = wg.index;
      clusterIpRange = wg.clusterIpRange;
      clusterIp = wg.clusterIp or "192.168.16.${toString wg.index}";
      wgPublicKey = wg.publicKey or "";
    }
    // lib.optionalAttrs ((wg.port or null) != null) {
      wgPort = wg.port;
    };

  nodes = lib.mapAttrs mkNode cluster.nodes;

  # hostnames：把每个节点的 extraHostname 反向聚合成 { "ollama.c.wg" = { hosts = [ ... ]; }; }
  hostnames = lib.foldl' (
    acc: name:
    let
      wg = (cluster.wireguard.nodes.${name} or { }).config or { };
    in
    lib.foldl' (
      acc: hostname:
      acc
      // {
        ${hostname} = {
          hosts = (acc.${hostname}.hosts or [ ]) ++ [ name ];
        };
      }
    ) acc (wg.extraHostname or [ ])
  ) { } (lib.attrNames cluster.wireguard.nodes);

  generated = {
    self = cluster.nodeName;
    inherit nodes hostnames;
    service = {
      peerPort = cfg.peerPort;
      socket = cfg.socket;
      stateDir = cfg.stateDir;
      sshHostKey = cfg.sshHostKey;
      applyKernel = cfg.applyKernel;
      wgInterface = cfg.wgInterface;
      # 开启自动隧道时接管 wg 的 peer endpoint（指向本机的隧道端口）。
      manageEndpoints = true;
      wgTunnel = {
        enable = cfg.wgTunnel.enable;
        portBase = cfg.wgTunnel.portBase;
        proxyLan = cfg.wgTunnel.proxyLan;
      };
      netWatch = {
        enable = cfg.netWatch.enable;
        debounceMs = cfg.netWatch.debounceMs;
      };
      dns = {
        enable = cfg.dns.enable;
        bind = cfg.dns.bind;
        port = cfg.dns.port;
        ttlSecs = cfg.dns.ttlSecs;
      };
      tunnels = cfg.tunnels;
    }
    // cfg.extraSettings;
  };

  configFile = pkgs.writeText "cluster-node-config.json5" (builtins.toJSON generated);
in
{
  options.services.cluster-node = {
    enable = lib.mkEnableOption "cluster-node 集群网络自愈守护进程";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.cluster-node;
      defaultText = lib.literalExpression "pkgs.cluster-node";
      description = "cluster-node 包。";
    };

    peerPort = lib.mkOption {
      type = lib.types.port;
      default = 7788;
      description = "对等节点 API（HTTP/2 + 自签 TLS）监听端口。";
    };

    socket = lib.mkOption {
      type = lib.types.path;
      default = "/run/cluster-node/node.sock";
      description = "本地 CLI 使用的 unix socket。";
    };

    stateDir = lib.mkOption {
      type = lib.types.path;
      default = "/var/lib/cluster-node";
      description = "状态目录：自签证书、WireGuard 计划文件、自动生成的临时 host key。";
    };

    sshHostKey = lib.mkOption {
      type = lib.types.path;
      default = "/etc/ssh/ssh_host_ed25519_key";
      description = "本机 SSH host key（ed25519），用于对等请求签名与 TLS 身份。";
    };

    cliGroup = lib.mkOption {
      type = lib.types.str;
      default = "wheel";
      description = ''
        允许访问本地 CLI socket 的用户组。服务以 root 运行、socket 为 0660，
        因此该组成员无需 sudo 即可使用 cluster-node-cli。
      '';
    };

    applyKernel = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "是否把计算出的 wg/ip 命令真正下发到内核（默认只写计划到 stateDir）。";
    };

    wgInterface = lib.mkOption {
      type = lib.types.str;
      default = "wg0";
      description = "WireGuard 接口名。";
    };

    dns = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "是否启动本地 DNS 服务。";
      };
      bind = lib.mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
        description = "DNS 监听地址。";
      };
      port = lib.mkOption {
        type = lib.types.port;
        default = 5335;
        description = ''
          DNS 监听端口。默认 5335：5353 通常已被 avahi/mDNS 占用，绑不上。
        '';
      };
      ttlSecs = lib.mkOption {
        type = lib.types.ints.positive;
        default = 5;
        description = "DNS 应答 TTL（秒）。";
      };
      resolvedIntegration = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          是否给 systemd-resolved 写 drop-in，把集群域（~c.wg）交给 cluster-node DNS。
          开启前请确认本机启用 systemd-resolved。
        '';
      };
    };

    wgTunnel = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          是否用 cluster-node 的 HTTP/2 长连接自动代理 WireGuard 流量（替代 udp2raw）。
          开启后本机为每个 peer 绑定 127.0.0.1:(peer.index + portBase)，
          并把跨网段（Wan）邻居的 wg endpoint 指向该端口。
        '';
      };
      portBase = lib.mkOption {
        type = lib.types.port;
        default = 41000;
        description = "隧道端口的基数（沿用 udp2raw 时代的 channelPort = index + 41000）。";
      };
      proxyLan = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "是否也代理同一局域网（Lan）的直连邻居；默认只代理跨网段链路。";
      };
    };

    netWatch = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          是否监听内核 netlink 事件（链路 / 地址变化）并立即重新探测。
          关闭后只能靠 probeIntervalMs 的定时探测发现网络变化。
        '';
      };
      debounceMs = lib.mkOption {
        type = lib.types.ints.positive;
        default = 1500;
        description = ''
          防抖时长（毫秒）：一次 wifi 重连 / DHCP 续租会连发多条事件，
          静默窗口内的事件合并成一次重新探测。
        '';
      };
    };

    tunnels = lib.mkOption {
      type = lib.types.listOf (
        lib.types.submodule {
          options = {
            listen = lib.mkOption {
              type = lib.types.str;
              description = "本地 UDP 监听地址，例如 127.0.0.1:19101。";
            };
            via = lib.mkOption {
              type = lib.types.str;
              description = "中转节点名（必须是 nodes 中的节点）。";
            };
            target = lib.mkOption {
              type = lib.types.str;
              description = "目标 UDP 地址，例如 192.168.16.3:16538。";
            };
          };
        }
      );
      default = [ ];
      description = ''
        通过 HTTP/2 长连接代理的 UDP 隧道列表。例如把本机 WireGuard 的
        endpoint 指向 127.0.0.1:41012，由 cluster-node 转发到网关节点的 wg 端口：
        { listen = "127.0.0.1:41012"; via = "aliyun-gz"; target = "192.168.16.3:16538"; }
      '';
    };

    extraSettings = lib.mkOption {
      type = lib.types.attrsOf lib.types.anything;
      default = { };
      description = "额外合并到配置 service 段的字段（如 probeIntervalMs、tunnelAllowedTargets）。";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = builtins.hasAttr cluster.nodeName cluster.nodes;
        message = "services.cluster-node：cluster.nodeName（${cluster.nodeName}）不在 cluster.nodes 中";
      }
    ];

    environment.etc."cluster-node/config.json5".source = configFile;

    environment.etc."systemd/resolved.conf.d/cluster-node.conf" =
      lib.mkIf (cfg.dns.enable && cfg.dns.resolvedIntegration)
        {
          text = ''
            [Resolve]
            DNS=127.0.0.1:${toString cfg.dns.port}
            Domains=~c.wg
          '';
        };

    systemd.services.cluster-node = {
      description = "cluster-node 集群网络自愈守护进程";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [
        "network-online.target"
        "sshd.service"
      ];
      restartTriggers = [ configFile ];
      # 改 wg endpoint / allowed-ips 与 ip route 需要 CAP_NET_ADMIN；
      # 命令经 sh -c 执行，所以 PATH 里要有 wg 与 ip。
      path = [
        pkgs.bash
        pkgs.iproute2
        pkgs.wireguard-tools
      ];
      serviceConfig = {
        # socket 权限是 0660，组由服务的 Group 决定；wheel 成员可直接跑 CLI。
        Group = cfg.cliGroup;
        AmbientCapabilities = [ "CAP_NET_ADMIN" ];
        CapabilityBoundingSet = [ "CAP_NET_ADMIN" ];
        ExecStart = "${cfg.package}/bin/cluster-node --config /etc/cluster-node/config.json5";
        Restart = "always";
        RestartSec = 5;
        StateDirectory = "cluster-node";
        RuntimeDirectory = "cluster-node";
        RuntimeDirectoryMode = "0750";
      };
    };

    networking.firewall.allowedTCPPorts = [ cfg.peerPort ];

    environment.systemPackages = [ cfg.package ];
  };
}
