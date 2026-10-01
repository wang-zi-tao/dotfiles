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
        apiKey = "!!js require('fs').readFileSync('/run/secrets/hindsight/apikey').trim()";
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
    hash = "sha256-N8q4dUmOkZOS9RLdgCcuhoAl5auVsnkwSEM5sbU7kvw=";
    plugins = plugins ++ [ ];
    src = ./tui;
    package = pkgs.dsh;
    cordis_patch = cordis_patch;
  };

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
      ".dsh/profiles/web/node_modules".source = "${profile-web}/lib/node_modules";
      ".dsh/profiles/tui/package.json".source = "${profile-tui}/package.json";
      ".dsh/profiles/tui/pnpm-lock.yaml".source = "${profile-tui}/pnpm-lock.yaml";
      ".dsh/profiles/tui/pnpm-workspace.yaml".source = "${profile-tui}/pnpm-workspace.yaml";
      ".dsh/profiles/tui/cordis.patch.yml".source = "${profile-tui}/cordis.patch.yml";
      ".dsh/profiles/tui/node_modules".source = "${profile-tui}/lib/node_modules";
      ".dsh/pet.json".source = ./pet.json;
      ".dsh/cordis.patch.yml".source = (pkgs.formats.yaml { }).generate "cordis.patch.yml" ([
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
                  Authorization = "!!js ('Bearer '+require('fs').readFileSync('/run/secrets/apikey/context7')).trim()";
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
