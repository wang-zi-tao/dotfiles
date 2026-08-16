{
  pkgs,
  lib,
  inputs,
  config,
  ...
}:

let
  dsh-unwrapped = pkgs.llm-agents.dsh;
  cordis_patch = [
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
      id = "dsh-tool-describe-image";
      name = "@linxin666/dsh-tool-describe-image";
      config = {
        baseURL = "https://api.moonshot.cn/v1";
        model = "kimi-k2.5";
        apiKey = "!!js require('fs').readFileSync('/run/secrets/apikey/moonshotai-cn').trim()";
      };
    }
  ];
  plugins = with pkgs; [ dsh-hindsight ];
  profile-web = pkgs.dsh-profile {
    name = "web";
    hash = "sha256-EC+p6NEc8wYcYMktZVFdvZpljZsyPu2V1OZtD3OZw60=";
    plugins = plugins;
    src = ./web;
    package = dsh-unwrapped;
    cordis_patch = cordis_patch;
  };
  profile-tui = pkgs.dsh-profile {
    name = "tui";
    hash = "sha256-YLD4/dgddUU9MRgxPOFG/lvTgetM6v11ioAvztaVM8g=";
    plugins = plugins;
    src = ./tui;
    package = dsh-unwrapped;
    cordis_patch = cordis_patch;
  };

  # One @deepseek-ai/dsh-mcp-client plugin row per home-manager MCP server,
  # inserted through the home-level cordis.patch.yml layer.
  mcpPatch = name: server: {
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
          command = server.command;
          args = server.args or [ ];
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
      ".dsh/profiles/web/node_modules".source = "${profile-web}/node_modules";
      ".dsh/profiles/tui/package.json".source = "${profile-tui}/package.json";
      ".dsh/profiles/tui/pnpm-lock.yaml".source = "${profile-tui}/pnpm-lock.yaml";
      ".dsh/profiles/tui/pnpm-workspace.yaml".source = "${profile-tui}/pnpm-workspace.yaml";
      ".dsh/profiles/tui/cordis.patch.yml".source = "${profile-tui}/cordis.patch.yml";
      ".dsh/profiles/tui/node_modules".source = "${profile-tui}/node_modules";
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

    home.packages = with pkgs; [
      (writeScriptBin "dsh" ''
        #!${bash}/bin/bash
        if [ -e /run/secrets/apikey/deepseek ]; then
          export DEEPSEEK_API_KEY=$(cat /run/secrets/apikey/deepseek)
        fi
        export DSH_TUI_SKIP_UPDATE=1
        exec ${dsh-unwrapped}/bin/dsh "$@"
      '')
    ];
  };
}
