{
  config,
  pkgs,
  lib,
  hermes-agent,
  ...
}:
let
  sops-enable = config.sops.defaultSopsFile != "/";
  neo4j-apoc = pkgs.fetchurl {
    url = "https://github.com/neo4j/apoc/releases/download/2026.03.1/apoc-2026.03.1-core.jar";
    hash = "sha256-12bb33963a50a942a960a5f9912a4d348c6628081297d5e73d46d4017ca686ab";
  };
  neo4j-plugins = pkgs.stdenv.mkDerivation {
    name = "neo4j-plugins";
    srcs = [ neo4j-apoc ];
    installPhase = ''
      mkdir -p $out/
      cp ${neo4j-apoc} $out/
    '';
  };
in
{
  config = lib.mkMerge [
    (lib.mkIf config.cluster.nodeConfig.ollama.enable {
      services.ollama = {
        enable = true;
        host = "0.0.0.0";
        port = 11434;
        loadModels = [
          "deepseek-r1:8b"
          "embeddinggemma"
        ];
        environmentVariables = {
          # HTTP_PROXY = "http://aliyun-hk.wg:8889";
          # HTTPS_PROXY = "http://aliyun-hk.wg:8889";
        };
      };

      # services.chromadb = {
      #   enable = true;
      #   port = 11437;
      # };

      # services.nextjs-ollama-llm-ui = {
      #   enable = true;
      #   hostname = "0.0.0.0";
      #   port = 11435;
      # };
    })
    (lib.mkIf config.cluster.nodeConfig.hermes.enable {
      services.hermes-agent =
        let
          model = "deepseek/deepseek-v4-flash";
        in
        {
          enable = true;
          # package = hermes-agent.packages.${pkgs.stdenv.system}.full;
          container = {
            enable = false;
            image = "nixos/nix";
            extraVolumes = [ "/nix/var/nix/daemon-socket/socket:/nix/var/nix/daemon-socket/socket" ];
          };
          environmentFiles = [ config.sops.secrets."hermes-env".path ];
          addToSystemPackages = true;
          settings = {
            model.default = model;
            display = {
              skin = "slate";
            };
            toolsets = [ "all" ];
            terminal = {
              backend = "local";
              timeout = 180;
            };
            compression = {
              enabled = true;
              threshold = 0.85;
              summary_model = model;
            };
            display = {
              compact = false;
              personality = "kawaii";
            };
            memory = {
              memory_enabled = true;
              user_profile_enabled = true;
              provider = "hindsight";
            };
            agent = {
              max_turns = 60;
              verbose = false;
            };
            plugins.enabled = [
              "disk-cleanup"
              "hermes-lcm"
              "rtk-rewrite"
            ];
            platforms = {
              # qqbot = {
              #   enabled = false;
              # };
            };
            dashboard = {
              enable = true;
              host = "0.0.0.0";
              port = 11436;
            };
          };
          environment = {
            "NIX_REMOTE" = "daemon";
          };
          mcpServers = {
            nixos = {
              command = "${pkgs.mcp-nixos}/bin/mcp-nixos";
            };
            neovim = {
              command = "${pkgs.mcp-neovim-server}/bin/mcp-neovim-server";
              env = {
                "ALLOW_SHELL_COMMANDS" = "true";
                "NVIM_SOCKET_PATH" = "/tmp/nvim";
              };
            };
            github = {
              url = "https://api.githubcopilot.com/mcp/";
              headers = {
                "Authorization" = "Bearer \${GITHUB_TOKEN}";
              };
            };
          };

          extraDependencyGroups = [
            "exa"
            "hindsight"
          ];

          extraPackages = [
            pkgs.nix
            pkgs.nushell
          ];

          extraPlugins = [
            pkgs.hermes-lcm
          ];

          extraPythonPackages = [
            pkgs.hermes-rtk
          ];

        };

      sops.secrets."hermes-env" = {
        sopsFile = config.cluster.ssh.publicKeySops;
      };

      environment.variables = {
        HERMES_HOME = "/var/lib/hermes/.hermes";
        HERMES_TUI = "1";
        HERMES_TUI_RESUME = "1";
      };

    })
    {
      sops.secrets.ai = lib.mkIf sops-enable {
        sopsFile = config.cluster.ssh.publicKeySops;
        mode = "0555";
      };
      sops.secrets."apikey/deepseek" = lib.mkIf sops-enable {
        sopsFile = config.cluster.ssh.publicKeySops;
        mode = "0555";
      };
      sops.secrets."apikey/moonshotai-cn" = lib.mkIf sops-enable {
        sopsFile = config.cluster.ssh.publicKeySops;
        mode = "0555";
      };
      sops.secrets."apikey/zen" = lib.mkIf sops-enable {
        sopsFile = config.cluster.ssh.publicKeySops;
        mode = "0555";
      };
      sops.secrets."apikey/context7" = lib.mkIf sops-enable {
        sopsFile = config.cluster.ssh.publicKeySops;
        mode = "0555";
      };
    }
  ];
}
