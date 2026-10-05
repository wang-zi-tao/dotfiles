{
  pkgs,
  config,
  lib,
  ...
}:
# ── dsh-web：把 DeepSeek Harness 的浏览器 GUI 挂到公网 ──────────────────────────
#
# 为什么必须分两层（都是上游行为，别绕过）：
#
#   1. dsh --profile web 只肯绑 127.0.0.1：--host 0.0.0.0 会被 CLI 直接拒绝，理由是
#      那等于把远程代码执行暴露到网络。所以公网入口只能由反代提供。
#   2. dsh 自带一层认证：每个进程启动时生成随机 launch token，印成
#      「dsh web: http://127.0.0.1:<port>/?token=…」。用该 URL 访问一次会换取一枚
#      HMAC 签名的 HttpOnly cookie（默认 30 天），之后 /api 与 /api/remote.mux 的
#      WebSocket 流都必须带上它。签名密钥持久在 $DSH_HOME/.credentials.yaml，所以进程
#      重启后老 cookie 仍然有效，只有 token 每次都会变。
#      但它没有「密码」概念，也没有任何注册入口——「不能注册」天然成立，「要密码」得外挂。
#   3. 静态资源（JS/CSS、/plugins/* 的 bundle）和 /plugins/events 这条 SSE 通道是
#      **不认证**的。所以「全部请求都必须先登录」只能靠 Caddy 的 basic_auth 兜住：
#      本模块把它放在站点级，覆盖包括静态资源在内的每一个请求。
#
# 密码走 sops（dsh-web/password-hash，只放哈希——Caddy 不接受明文）：
#   nix run nixpkgs#caddy -- hash-password --plaintext '<你的密码>'
# 把产出的 $2a$14$… 填进 sops。本模块用 sops.templates 把它渲染成一段 Caddyfile
# 片段，caddy 以 import 读入：哈希不进 nix store，也不需要任何运行期服务。
# 换算法（--algorithm argon2id）时记得同步改下面的 hashAlgorithm，否则只会一直认证失败。
#
# 首次登录：token 每次启动都变，只能从日志里取。服务器上跑 dsh-web-login-url 拿到
#   把 127.0.0.1 换成公网地址的完整 URL，浏览器打开一次换 cookie。此后在 cookie 有效期
#   内（默认 30 天）以及任意次重启都不用再登，只需要过 Caddy 的密码。
let
  cfg = config.cluster;
  nodeConfig = cfg.nodes.${cfg.nodeName};
  networkConfig = cfg.network.edges.${cfg.nodeName}.config;
  sops-enable = config.sops.defaultSopsFile != "/";

  publicIp = builtins.toString networkConfig.publicIp;
  publicPort = nodeConfig.dshWeb.port;
  bindPort = nodeConfig.dshWeb.bindPort;

  home = "/var/lib/dsh";
  workspace = "${home}/workspace";

  # basic_auth 的哈希算法，必须与 sops 里那份哈希的算法一致：
  #   caddy hash-password（默认）产出 bcrypt；--algorithm argon2id 产出 argon2id。
  # 不一致不会报错，只是永远认证失败（浏览器反复弹密码框）。
  hashAlgorithm = "bcrypt";
  # sops.templates 渲染出的 basic_auth 片段，由下面的 Caddyfile import 进去。
  authFragment = config.sops.templates."dsh-web-basic-auth".path;

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
          message = "cluster.dshWeb 需要 sops：登录密码哈希由 sops.secrets.dsh-web/password-hash 提供";
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
          # 这个 web 面等于「谁能过密码认证，谁就能在这台机器上执行代码」，所以把
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

      # ── Caddy：公网入口 + 第一道锁 ──────────────────────────────────
      # 用独立端口而不是路径前缀：前端走绝对路径的 /api 与 /plugins，挂在子路径下
      # 会 404；而且同一 IP 的 443 站点上还住着 atuin/onedev/webssh，不该被这里的
      # basic_auth 一起拦住。
      services.caddy = {
        enable = true;
        virtualHosts."https://${publicIp}:${toString publicPort}" = {
          extraConfig = ''
            tls internal
            import ${authFragment}
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

      # 哈希本身仍然声明成 secret：sops.placeholder 是由 sops.secrets 的键推导出来的，
      # 模板里的占位符靠它才能换成真值。
      sops.secrets."dsh-web/password-hash" = {
        mode = "0400";
      };

      # 只含哈希的 Caddyfile 片段，caddy 在配置加载期 import 它。
      # 渲染目录（/run/secrets/rendered）由 sops-install-secrets 以 0751 建立，即
      # 其它用户可穿越，所以 owner=caddy + 0400 就够：caddy 读得到，别人读不到。
      # 注意：片段为空或语法错误会让 caddy 整个起不来（同机其它站点一起挂）。
      # 换哈希后如果网页一直弹密码框，先 systemctl status caddy 看配置有没有加载失败。
      sops.templates."dsh-web-basic-auth" = {
        content = ''
          basic_auth ${hashAlgorithm} {
            dsh ${config.sops.placeholder."dsh-web/password-hash"}
          }
        '';
        owner = config.services.caddy.user;
        mode = "0400";
        restartUnits = [ "caddy.service" ];
      };
    })
  ];
}
