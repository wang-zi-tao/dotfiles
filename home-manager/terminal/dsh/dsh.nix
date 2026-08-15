{
  pkgs,
  lib,
  inputs,
  config,
  ...
}:

let
  dsh-unwrapped = pkgs.llm-agents.dsh;
  profile-web = pkgs.dsh-profile {
    name = "web";
    hash = "sha256-SDS/Ace/Xd7GiUsokyo0jC/lKUxgKSoLKdgmXBg0hbE=";
    plugins = with pkgs; [dsh-hindsight];
    src = ./web;
    cordis_patch = [
        {
          id = "hindsight";
          config = {
            apiUrl = "http://127.0.0.1:8888";
            memoryMode = "hybrid";
          };
        }
        {
          id = "profile-web";
          name = "@linxin666/dsh-tool-describe-image";
          config = {
            baseURL = "https://api.moonshot.cn/v1";
            model = "kimi-k2.5";
            apiKeyEnv = "KIMI_API_KEY";
          };
        }
    ];
  };
  presets = pkgs.fetchgit {
    url = "https://github.com/xiaobright/dsh-anchored-standard/";
    rev = "db4527a2a70a9032d3a8525ce3c0ea6ef528d6fc";
    sha256 = "sha256-/E6O9YHKjb2SYR5AtRinNZSDi6yHJopBKQkrQDyg0Bw=";
  };
in
{
  config = {

    home.file = {
      ".dsh/profiles/web/package.json".source = "${profile-web}/package.json";
      ".dsh/profiles/web/pnpm-lock.yaml".source = "${profile-web}/pnpm-lock.yaml";
      ".dsh/profiles/web/pnpm-workspace.yaml".source = "${profile-web}/pnpm-workspace.yaml";
      ".dsh/profiles/web/cordis.patch.yml".source =  "${profile-web}/cordis.patch.yml";
      # ".dsh/profiles/web/node_modules".source =  "${profile-web}/node_modules";
      ".dsh/pet.json".source = ./pet.json;
      ".dsh/.agent-presets/anchored-standard/".source = "${presets}/preset";
      ".dsh/.agent-presets/zero-anchored-standard/".source = "${presets}/zero-anchored-standard";
      ".dsh/.agent-presets/whoami-standard/".source = "${presets}/whoami-standard";
      ".dsh/cordis.patch.yml".source = (pkgs.formats.yaml { }).generate "cordis.patch.yml" [
      ];
    };

    home.packages = with pkgs; [
      (writeScriptBin "dsh" ''
        #!${bash}/bin/bash
        if [ -e /run/secrets/apikey/moonshotai-cn ]; then
          export KIMI_API_KEY=$(cat /run/secrets/apikey/moonshotai-cn)
        fi
        if [ -e /run/secrets/apikey/deepseek ]; then
          export DEEPSEEK_API_KEY=$(cat /run/secrets/apikey/deepseek)
        fi
        if [ -e /run/secrets/hindsight/apikey ]; then
          export HINDSIGHT_API_KEY=$(cat /run/secrets/hindsight/apikey)
        fi
        export NODE_PATH=${dsh-unwrapped}/lib:${config.home.homeDirectory}/.dsh/profiles:$NODE_PATH
        exec ${dsh-unwrapped}/bin/dsh "$@"
      '')
    ];

    home.activation.dsh = ''
      # profile web
      rm -rf $HOME/.dsh/profiles/web/node_modules
      mkdir -p $HOME/.dsh/profiles/web/node_modules
      # chmod -R +w $HOME/.dsh/profiles/web/
      cp -rf ${profile-web}/node_modules $HOME/.dsh/profiles/web/
      # chmod -R -w $HOME/.dsh/profiles/web/

      # merge setting
      # if [ -e  $HOME/.dsh/cordis.patch.yml ]; then
      #   ${pkgs.nushell}/bin/nu -c '(open ${./cordis.patch.yml}) | merge deep (open ${config.home.homeDirectory}/.dsh/cordis.patch.yml) | to yaml | save -f ${config.home.homeDirectory}/.dsh/cordis.patch.yml;'
      # else
      #   cp -f ${./cordis.patch.yml} $HOME/.dsh/cordis.patch.yml
      # fi
    '';
  };
}
