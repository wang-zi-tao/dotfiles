{
  pkgs,
  config,
  lib,
  ...
}:
# ── dsh-web：把 DeepSeek Harness 的浏览器 GUI 挂到公网 ──────────────────────────
#
# 分两层（都是上游行为，别绕过）：
#
#   1. dsh --profile web 只肯绑 127.0.0.1：--host 0.0.0.0 会被 CLI 直接拒绝，理由是
#      那等于把远程代码执行暴露到网络。所以公网入口只能由反代提供。
#   2. dsh 自带一层认证：每个进程启动时生成随机 launch token，印成
#      「dsh web: http://127.0.0.1:<port>/?token=…」。用该 URL 访问一次会换取一枚
#      HMAC 签名的 HttpOnly cookie（默认 30 天），之后 /api 与 /api/remote.mux 的
#      WebSocket 流都必须带上它。签名密钥持久在 $DSH_HOME/.credentials.yaml，所以进程
#      重启后老 cookie 仍然有效，只有 token 每次都会变。
#      但它没有「密码」概念，也没有任何注册入口——「不能注册」天然成立，「要密码」得外挂。
#
# 外挂的登录改成了 SSO：站点级用 Caddy 的 forward_auth 调 homepage 的 /api/verify，
# 由 homepage 的登录 Cookie 决定放行。这样同一浏览器只需在 https://<ip>/homepage/
# 登录一次，dsh-web、homepage 以及以后接入的服务共用同一个会话（Cookie 是
# host-only + Path=/，同一主机的不同端口天然共享）。
#
#   * forward_auth 覆盖包括静态资源在内的每一个请求（dsh 的静态资源与 /plugins/events
#     本身不认证，必须由反代兜住）；
#   * X-Original-URL 把用户原本要访问的完整 URL 传给 /api/verify，未登录时 homepage
#     返回 302 到 /homepage/?next=…，登录后自动跳回来；
#   * 仍保留独立端口而不是挂在 /homepage/ 子路径下：dsh 前端走绝对路径的 /api 与
#     /plugins，挂子路径会 404。
#
# launch token 这一层仍然存在（登录 SSO 后 dsh 自己还要认 cookie）：首次登录用
# dsh-web-login-url 从日志里取带 token 的 URL，浏览器打开一次换 dsh 的 cookie；
# 之后 cookie 有效期内（默认 30 天）以及任意次重启都不用再取。
let
  cfg = config.cluster;
  nodeConfig = cfg.nodes.${cfg.nodeName};
  networkConfig = cfg.network.edges.${cfg.nodeName}.config;
  sops-enable = config.sops.defaultSopsFile != "/";

  publicIp = if networkConfig.publicIp!=null then builtins.toString networkConfig.publicIp else "127.0.0.1";
  publicPort = nodeConfig.dshWeb.port;
  bindPort = nodeConfig.dshWeb.bindPort;
  homepagePort = nodeConfig.homepage.bindPort;

  home = "/var/lib/dsh";
  workspace = "${home}/workspace";

  loginUrl = pkgs.writeShellScriptBin "dsh-web-login-url" ''
    set -euo pipefail
    if [ "$(id -u)" -ne 0 ]; then
      echo "dsh-web-login-url: 需要 root（要读 systemd 日志）" >&2
      exit 1
    fi
    tokenized=$(journalctl -u dsh-web -o cat --no-pager | grep -o 'http://127[.]0[.]0[.]1:[0-9]*/?token=[A-Za-z0-9_-]*' | tail -n 1 || true)
    if [ -z "$tokenized" ]; then
      echo "dsh-web-login-url: 日志里没有带 token 的 URL，先看 systemctl status dsh-web" >&2
      exit 1
    fi
    echo "$(echo "$tokenized" | sed 's#^http://127[.]0[.]0[.]1:[0-9]*#https://${publicIp}:${toString publicPort}#')"
  '';
in
{
  config = lib.mkMerge [
    {
      assertions = [
        {
          assertion = !nodeConfig.dshWeb.enable || sops-enable;
          message = "cluster.dshWeb 需要 sops：DEEPSEEK_API_KEY 由 sops 模板 dsh-env 提供";
        }
        {
          assertion = !nodeConfig.dshWeb.enable || nodeConfig.homepage.enable;
          message = "dsh-web 的登录依赖 homepage 的 SSO：请在同一节点启用 cluster.nodes.<node>.homepage";
        }
      ];
    }
    (lib.mkIf (nodeConfig.dshWeb.enable && sops-enable) {
      users.groups.dsh = { };
      users.users.dsh = {
        isSystemUser = true;
        group = "dsh";
        inherit home;
        description = "DeepSeek Harness browser GUI";
      };
      nix.settings.trusted-users = ["dsh"];

      # DSH_HOME 与 agent 的工作目录。dsh 会自己在 profiles/ 下初始化 web profile
      # （package.json + cordis.patch.yml），它要的 bundle 都在 pkgs.dsh 自己的
      # node_modules 里，所以这里不需要 pnpm / nix 预先铺任何文件。
      systemd.tmpfiles.rules = [
        "d ${home} 0700 dsh dsh -"
        "d ${workspace} 0700 dsh dsh -"
      ];

      systemd.services.dsh-web = {
        description = "DeepSeek Harness browser GUI";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        environment = {
          HOME = home;
          DSH_HOME = home;
        };
        # 这两项会以 mkAfter 追加在 systemd 服务默认 PATH（coreutils/findutils/
        # gnugrep/gnused/systemd）之后：
        #   * bubblewrap 是 dsh 文件沙箱的第一级（sandbox-local 里
        #     spawnSync('bwrap', …) 探针），宿主没有它时 workspace-write 这类
        #     沙箱模式会直接报 sandbox-unavailable；
        #   * git 是 agent 干活的基本工具，这里直接给上（系统 profile 里没有它）；
        #   * config.system.path 让 agent 的 shell 看得到系统 profile（bash 就来自
        #     programs.bash，还有你自己往 environment.systemPackages 里装的东西）。
        path = [
          pkgs.bubblewrap
          pkgs.git
          config.system.path
          pkgs.nodejs
          pkgs.python3
          pkgs.pnpm
          pkgs.uv
          pkgs.clang
        ];
        serviceConfig = {
          User = "dsh";
          Group = "dsh";
          WorkingDirectory = workspace;
          ExecStart = "${pkgs.dsh}/bin/dsh --profile web --no-open --port ${toString bindPort} --trusted-host ${publicIp}";
          # module/ai.nix 里的 sops 模板，提供 DEEPSEEK_API_KEY。
          EnvironmentFile = [ config.sops.templates."dsh-env".path ];
          Restart = "always";
          RestartSec = 3;

          # ── 加固 ──────────────────────────────────────────────────────
          # 这个 web 面等于「谁能过登录，谁就能在这台机器上执行代码」，所以把
          # 文件系统收紧到只剩 DSH_HOME 可写。要让 agent 操作别的目录（例如某个
          # 仓库），把路径追加进 ReadWritePaths；要用 nix 命令还得加 /nix/var/nix
          # （nix daemon 的 unix socket 需要写权限）。
          NoNewPrivileges = true;
          PrivateTmp = true;
          ProtectSystem = "strict";
          ProtectHome = true;
          ReadWritePaths = [ home ];
        };
      };

      # ── Caddy：公网入口 + SSO 第一道锁 ─────────────────────────────
      # 用独立端口而不是路径前缀：前端走绝对路径的 /api 与 /plugins，挂在子路径下
      # 会 404；而且同一 IP 的 443 站点上还住着 atuin/onedev/webssh，不该被这里的
      # SSO 一起拦住。
      services.caddy = {
        enable = true;
        virtualHosts."https://${publicIp}:${toString publicPort}" = {
          extraConfig = ''
            tls internal

            # 未登录的请求交给 homepage：200 放行，302 则把浏览器送回登录页
            # （forward_auth 会把非 2xx 响应原样透传给客户端）。
            forward_auth 127.0.0.1:${toString homepagePort} {
              uri /api/verify
              header_up X-Original-URL https://{http.request.hostport}{http.request.uri}
            }

            reverse_proxy 127.0.0.1:${toString bindPort} {
              # 必须把原始 Host（含端口）原样转给 dsh：
              #   * dsh 的 /api 围栏要求 Host ∈ {loopback} ∪ trustedHosts；
              #   * Origin 必须严格等于 Host，否则 POST /api 直接 403；
              #   * 浏览器 cookie 的名字与签名负载都绑 authority。
              # {host} 会丢掉端口，所以用 {http.request.hostport}（原样回显 Host 头）。
              header_up Host {http.request.hostport}
            }
          '';
        };
      };

      networking.firewall.allowedTCPPorts = [ publicPort ];

      environment.systemPackages = [ loginUrl ];
    })
  ];
}
