{
  pkgs,
  config,
  lib,
  ...
}:
# ── homepage：登录后才能看的个人浏览器主页 ───────────────────────────────────
#
# 组成：
#   * Rust/Axum 后端（packages/homepage）：提供 JSON API、会话 Cookie、静态资源；
#   * Vue 3 前端（packages/homepage/web）：构建产物由后端以 SPA 方式托管；
#   * Caddy：把 https://<公网IP>/homepage/* 反代到 127.0.0.1:18081。使用服务器
#     默认的 443 端口，与 nextcloud / onedev 等共用同一个站点，不单独占端口；
#   * sops：只存密码的 bcrypt 哈希，后端每次登录时重读，换密码不用改 nix。
#
# 收藏数据来自 packages/homepage/bookmarks.json（由 pages.infinity 转换而来，
# 转换命令见 packages/homepage/README.md）；首次启动且数据库为空时导入，之后
# 增删改都落在 /var/lib/homepage/homepage.db。
#
# 内网入口不写在数据库里：本模块根据 cluster.nodes 里各节点启用的服务实时生成
# config.intranet，所以加一个服务只需要改 cluster.nix。
#
# SSO：dsh-web 的 Caddy 站点用 forward_auth 调本服务的 /api/verify。已登录时
# 返回 200 + X-Auth-User；未登录时 302 到 /homepage/?next=<原地址>，登录完自动
# 跳回。Cookie 是 host-only + Path=/ 的，所以同一主机的不同端口共享会话。
let
  cfg = config.cluster;
  nodeConfig = cfg.nodes.${cfg.nodeName};
  homepage = nodeConfig.homepage;
  sops-enabled = config.sops.defaultSopsFile != "/";

  package = pkgs.homepage;
  stateDir = "/var/lib/homepage";

  # 公网入口：https://<publicIp>/homepage/
  basePath = "/homepage";

  # 公开 SVG 图标：selfh.st 提供各服务的品牌图标，Iconify 补一个 Xpra。
  icons = {
    nextcloud = "https://cdn.jsdelivr.net/gh/selfhst/icons/svg/nextcloud.svg";
    onedev = "https://cdn.jsdelivr.net/gh/selfhst/icons/svg/onedev.svg";
    webssh = "https://cdn.jsdelivr.net/gh/selfhst/icons/svg/webssh.svg";
    atuin = "https://cdn.jsdelivr.net/gh/selfhst/icons/svg/atuin.svg";
    grafana = "https://cdn.jsdelivr.net/gh/selfhst/icons/svg/grafana.svg";
    xpra = "https://api.iconify.design/mdi/monitor-multiple.svg";
    dsh = "https://cdn.jsdelivr.net/gh/selfhst/icons/svg/deepseek.svg";
  };

  # 取节点的公网地址；只有走公网的节点才会出现在内网入口里。
  publicIpOf = name: cfg.network.edges.${name}.config.publicIp;
  hasPublicIp = name: publicIpOf name != null;

  nodeEntries = name:
    let
      node = cfg.nodes.${name};
      ip = builtins.toString (publicIpOf name);
      https = path: "https://${ip}${path}";
      entry = entryName: path: icon: {
        name = entryName;
        url = https path;
        inherit icon;
      };
    in
    lib.optionals node.NextCloudServer.enable [ (entry "Nextcloud" "/nextcloud/" icons.nextcloud) ]
    ++ lib.optionals node.OnedevServer.enable [ (entry "OneDev" "/onedev/" icons.onedev) ]
    ++ lib.optionals node.webssh.enable [ (entry "WebSSH" "/webssh/" icons.webssh) ]
    ++ lib.optionals node.prometheus.server [ (entry "Grafana" "/prometheus/" icons.grafana) ]
    ++ lib.optionals node.XpraProxy.enable [
      {
        name = "Xpra";
        url = "https://${ip}:7800/xpra/client/";
        icon = icons.xpra;
      }
    ]
    ++ lib.optionals node.dshWeb.enable [
      {
        name = "dsh-web";
        url = "https://${ip}:${toString node.dshWeb.port}";
        icon = icons.dsh;
      }
    ];

  intranetGroups =
    let
      names = lib.filter (name: hasPublicIp name) (lib.attrNames cfg.nodes);
      groups = map (name: {
        inherit name;
        entries = nodeEntries name;
      }) names;
    in
    lib.filter (group: group.entries != [ ]) groups;

  configFile = (pkgs.formats.json { }).generate "homepage-config.json" {
    title = "我的主页";
    listen = "127.0.0.1";
    port = homepage.bindPort;
    basePath = basePath;
    publicBaseUrl = "https://${builtins.toString (publicIpOf cfg.nodeName)}";
    database = "${stateDir}/homepage.db";
    staticDir = "${package}/share/homepage/web";
    secureCookies = true;
    auth = {
      username = "wangzi";
      passwordHashFile = config.sops.secrets."homepage/password-hash".path;
      sessionDays = 30;
    };
    seed.file = "${package}/share/homepage/bookmarks.json";
    searchEngines = [
      {
        id = "google";
        name = "Google";
        url = "https://www.google.com/search?q={query}";
      }
      {
        id = "bing";
        name = "Bing";
        url = "https://www.bing.com/search?q={query}";
      }
      {
        id = "baidu";
        name = "百度";
        url = "https://www.baidu.com/s?wd={query}";
      }
      {
        id = "duckduckgo";
        name = "DuckDuckGo";
        url = "https://duckduckgo.com/?q={query}";
      }
      {
        id = "github";
        name = "GitHub";
        url = "https://github.com/search?q={query}";
      }
      {
        id = "nixos";
        name = "NixOS Search";
        url = "https://search.nixos.org/packages?query={query}";
      }
    ];
    defaultSearchEngine = "google";
    intranet = intranetGroups;
  };
in
{
  config = lib.mkMerge [
    {
      assertions = [
        {
          assertion = !homepage.enable || sops-enabled;
          message = "cluster 里的 homepage 需要 sops：登录密码哈希来自 sops.secrets.homepage/password-hash";
        }
        {
          assertion = !homepage.enable || hasPublicIp cfg.nodeName;
          message = "homepage 需要节点有公网 IP（cluster.network.edges.<node>.config.publicIp）";
        }
      ];
    }

    (lib.mkIf homepage.enable {
      users.groups.homepage = { };
      users.users.homepage = {
        isSystemUser = true;
        group = "homepage";
        home = stateDir;
        description = "个人浏览器主页";
      };

      environment.etc."homepage/config.json".source = configFile;
      environment.systemPackages = [ package ];

      systemd.services.homepage = {
        description = "个人浏览器主页";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        environment.RUST_LOG = "info,homepage=info";
        serviceConfig = {
          User = "homepage";
          Group = "homepage";
          ExecStart = "${package}/bin/homepage --config /etc/homepage/config.json";
          StateDirectory = "homepage";
          Restart = "always";
          RestartSec = 3;

          # 服务只提供网页与 JSON API，把文件系统收紧到只剩状态目录可写。
          NoNewPrivileges = true;
          PrivateTmp = true;
          ProtectSystem = "strict";
          ProtectHome = true;
          ReadWritePaths = [ stateDir ];
          RestrictAddressFamilies = [
            "AF_INET"
            "AF_INET6"
            "AF_UNIX"
          ];
        };
      };

      sops.secrets."homepage/password-hash" = {
        owner = "homepage";
        mode = "0400";
        # 登录时后端会重新读这个文件，正常轮换密码不需要重启；这里仍声明，
        # 便于 sops-nix 在文件缺失时把旧服务拉起。
        restartUnits = [ "homepage.service" ];
      };

      services.caddy = {
        enable = true;
        virtualHosts."https://${builtins.toString (publicIpOf cfg.nodeName)}" = {
          extraConfig = ''
            # 443 站点上的 /homepage 路由；Vite 的 base 是 /homepage/。
            @homepage-root path /homepage
            redir @homepage-root /homepage/ 308

            route /homepage/* {
              uri strip_prefix /homepage
              reverse_proxy 127.0.0.1:${toString homepage.bindPort}
            }
          '';
        };
      };
    })
  ];
}
