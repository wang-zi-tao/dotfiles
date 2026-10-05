{
  pkgs,
  lib,
  inputs,
  config,
  ...
}:

let
  cordis_patch = [
    {
      insert = [
        {
          id = "computer-use";
          name = "@deepseek-ai/dsh-computer-use";
        }
        {
          id = "computer-use-cua-driver-mcp";
          name = "@deepseek-ai/dsh-experimental-computer-use-cua-driver-mcp";
          config = {
            command = "${pkgs.cua-driver}/bin/cua-driver";
            args = [ "mcp" ];
            toolCallTimeoutMs = 120000;
          };
        }
      ];
    }
    {
      id = "hindsight";
      name = "dsh-hindsight";
      config = {
        apiUrl = "http://127.0.0.1:11438";
        memoryMode = "hybrid";
        # 经 pkgs.dsh-cordis-patch 处理后才成为真正的 YAML 标签。
        #
        # 表达式只能用求值沙箱里有的东西：cordis-plugin-loader 的实现是
        #   new Function("ctx", "expr", "with (ctx) { return eval(expr) }")
        # 即全局作用域 + loader 提供的成员。dsh 只 provide 了 dshHomePath，
        # **没有 require** —— 写 require('fs') 会 ReferenceError，该 entry
        # 激活失败，整个 dsh-hindsight 插件不加载（工具直接消失）。
        # process 是 Node 全局，用 process.getBuiltinModule 才能拿到内建模块。
        #
        # 另外 readFileSync 必须显式带 'utf8'：不给编码返回 Buffer，
        # 而 Buffer 没有 trim()，会 TypeError。
        apiKey = "!!js process.getBuiltinModule('fs').readFileSync('/run/secrets/hindsight/apikey','utf8').trim()";
      };
    }
    {
      id = "agent-default-model";
      config = {
        provider = "deepseek";
        model = "deepseek-v4.1-flash";
      };
    }
  ];
  plugins = with pkgs; [
    dsh-hindsight
    dsh-lsp
    dsh-neovim
  ];

  # @omdsh-dev/dsh-genui 以 git 依赖装进 profile，但 nixpkgs 的 pnpm 钩子用
  # --ignore-scripts 安装，它的 build/prepare 不跑，落地只有 src/ 没有 lib/ →
  # dsh 启动报 “…/dsh-genui/lib/index.js 不存在”，插件崩。
  # 这里换成 overlays/ai.nix 里从 npm 发布包构建的 pkgs.dsh-genui（自带 lib/，
  # 并把清单名统一成 profile 的依赖键），generate-bundles 会就地覆盖空壳目录，
  # 因此不需要动 web/package.json 与 pnpm-lock.yaml。
  profile-web = pkgs.dsh-profile {
    name = "web";
    hash = "sha256-kanOfX4fr8i33MpmWagE4WDf9VBwttR4Ku0MiLN6f8c=";
    plugins = plugins ++ [ pkgs.dsh-genui ];
    src = ./web;
    package = pkgs.dsh;
    cordis_patch = cordis_patch;
  };
  profile-tui = pkgs.dsh-profile {
    name = "tui";
    hash = "sha256-vcsUnG5yikVBxC+lQo/EKHOhnNfFWmShpOeuCF+2hcg=";
    plugins = plugins ++ [ ];
    src = ./tui;
    package = pkgs.dsh;
    cordis_patch = cordis_patch;
  };

  # dsh 的运行时解析器只对三类 importer 路径启用「拦截层」
  # （@deepseek-ai/dsh-app-boot 的 profile-resolution/resolver.js → findInterceptionLayer）：
  #   ① $DSH_HOME/profiles 树内；② 当前 profile 目录内；
  #   ③ <profile>/node_modules 下登记的 symlink 条目（linked root）的真实路径内。
  #
  # profile 的依赖里 @deepseek-ai/dsh-* 全是 optional peerDependency，而
  # pnpm-workspace.yaml 设了 autoInstallPeers=false，所以它们不会装进 profile 的
  # node_modules，只能靠拦截层从 dsh 本体的 installation scope 解析。
  # 若把整个 node_modules 做成指向 /nix/store 的 symlink，插件模块真实路径就落在
  # 三个条件之外，拦截层失效，启动报 “N entries did not activate”。
  # 逐条 symlink 顶层条目，每个条目都会成为 linked root，拦截层随即生效。
  profileModuleFiles =
    profile: dir:
    lib.listToAttrs (
      map (e: {
        name = "${dir}/node_modules/${e}";
        value.source = "${profile}/lib/node_modules/${e}";
      }) (lib.attrNames (builtins.readDir "${profile}/lib/node_modules"))
    );

  # One @deepseek-ai/dsh-mcp-client plugin row per home-manager MCP server,
  # inserted through the home-level cordis.patch.yml layer.
  mcpPatch =
    name: server:
    let
      wrapper = pkgs.writeScript "dsh-mcp-${name}-wrapper" ''
        #!${pkgs.busybox}/bin/sh
        exec "$@" 2> >(logger -t ${name})
      '';
    in
    {
      id = "mcp-${name}";
      name = "@deepseek-ai/dsh-mcp-client";
      config =
        if server.url != null then
          {
            serverName = name;
            transport = "streamable-http";
            url = server.url;
            headers = server.headers or { };
          }
        else
          {
            serverName = name;
            transport = "stdio";
            command = "${wrapper}";
            args = [ server.command ] ++ (server.args or [ ]);
            env = server.env or { };
          };
    };
in
{
  config = {

    home.file = {
      ".dsh/profiles/web/package.json".source = "${profile-web}/package.json";
      ".dsh/profiles/web/pnpm-lock.yaml".source = "${profile-web}/pnpm-lock.yaml";
      ".dsh/profiles/web/pnpm-workspace.yaml".source = "${profile-web}/pnpm-workspace.yaml";
      ".dsh/profiles/web/cordis.patch.yml".source = "${profile-web}/cordis.patch.yml";
      ".dsh/profiles/tui/package.json".source = "${profile-tui}/package.json";
      ".dsh/profiles/tui/pnpm-lock.yaml".source = "${profile-tui}/pnpm-lock.yaml";
      ".dsh/profiles/tui/pnpm-workspace.yaml".source = "${profile-tui}/pnpm-workspace.yaml";
      ".dsh/profiles/tui/cordis.patch.yml".source = "${profile-tui}/cordis.patch.yml";
      ".dsh/profiles/node_modules".source = "${pkgs.dsh}/lib/node_modules";
      ".dsh/pet.json".source = ./pet.json;
      # formats.yaml 会给每个字符串加引号，`!!js …` 因此退化成普通字符串，
      # dsh 不求值 → 插件把整串当 apiKey 发出去 → 401。
      # dsh-cordis-patch 把带引号的 !!js 还原成真正的 YAML 标签。
      ".dsh/cordis.patch.yml".source = pkgs.dsh-cordis-patch {
        yaml = (pkgs.formats.yaml { }).generate "cordis.raw.yml" ([
          {
            insert = (builtins.attrValues (builtins.mapAttrs mcpPatch config.programs.mcp.servers)) ++ [
              {
                id = "mcp-context7";
                name = "@deepseek-ai/dsh-mcp-client";
                config = {
                  serverName = "context7";
                  transport = "streamable-http";
                  url = "https://mcp.context7.com/mcp";
                  headers = {
                    # 同 hindsight：沙箱里没有 require，且 readFileSync 必须带
                    # 'utf8'（否则返回 Buffer，Buffer 没有 trim）。
                    Authorization = "!!js ('Bearer '+process.getBuiltinModule('fs').readFileSync('/run/secrets/apikey/context7','utf8').trim())";
                  };
                };
              }
              {
                id = "skills-nix";
                name = "@deepseek-ai/dsh-skill-filesystem";
                customSkillDirs = [
                  ".opencode/skills"
                ];

              }
            ];
          }
        ]);
      };
    }
    # 逐条 symlink 顶层条目，让每个条目成为 dsh 解析器的 linked root
    # （原因见上面 profileModuleFiles 的注释）。node_modules 必须是真目录，
    # 整目录 symlink 到 /nix/store 会让拦截层失效。
    #
    # 代价：这里对 profile 派生做 IFD —— Nix 求值 home-manager 配置时会先构建
    # dsh-profile-{web,tui} 才能列出 node_modules 内容。不想 IFD 的话，改由
    # packages/dsh-profile 生成 symlink 农场目录，再整目录链接过来。
    // (profileModuleFiles profile-web ".dsh/profiles/web")
    // (profileModuleFiles profile-tui ".dsh/profiles/tui");

    systemd.user.services.dsh = {
      Unit = {
        Description = "dsh server";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        Type = "simple";
        ExecStart = "${pkgs.dsh}/bin/dsh web";
        Restart = "always";
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };

    home.packages = with pkgs; [
      bubblewrap

      (writeScriptBin "dsh" ''
        #!${bash}/bin/bash
        if [ -e /run/secrets/apikey/deepseek ]; then
          export DEEPSEEK_API_KEY=$(cat /run/secrets/apikey/deepseek)
        fi
        export DSH_TUI_SKIP_UPDATE=1
        exec ${dsh}/bin/dsh "$@"
      '')
    ];
  };
}
