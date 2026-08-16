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
  hindsight-port = 11438;
  hindsight-host = "127.0.0.1";
  hindsight-url = "http://${hindsight-host}:${toString hindsight-port}";
in
{
  config = lib.mkMerge [
    (lib.mkIf config.cluster.nodeConfig.ollama.enable {
      services.ollama = {
        enable = true;
        host = "0.0.0.0";
        port = 11434;
        loadModels = [
          "embeddinggemma"
          "qwen3-embedding:0.6b"
        ];
        environmentVariables = {
          # HTTP_PROXY = "http://aliyun-hk.wg:8889";
          # HTTPS_PROXY = "http://aliyun-hk.wg:8889";
        };
      };

      # services.chromadb = {
      #   enable = true;
      #   port = 11439;
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
          package = hermes-agent.packages.${pkgs.stdenv.system}.default;
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
              qqbot = {
                enabled = true;
              };
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
    (lib.mkIf config.cluster.nodeConfig.hindsight.enable {
      users.users.hindsight = {
        isSystemUser = true;
        group = "hindsight";
        home = "/var/lib/hindsight";
        createHome = true;
      };
      users.groups.hindsight = { };

      # Use the NixOS-managed PostgreSQL (with pgvector) instead of pg0's
      # embedded server, whose bundled postgres needs system libs + zoneinfo
      # that aren't present on NixOS. hindsight connects over the unix socket
      # with peer auth (OS user `hindsight` == role `hindsight`).
      services.postgresql = {
        enable = true;
        extensions = ps: [ ps.pgvector ];
        ensureDatabases = [ "hindsight" ];
        ensureUsers = [
          {
            name = "hindsight";
            ensureDBOwnership = true;
            # hindsight runs its own schema migrations and creates the pgvector
            # extension (`CREATE EXTENSION vector`). The nixpkgs pgvector isn't
            # marked `trusted`, so this requires superuser.
            ensureClauses.superuser = true;
          }
        ];
      };

      # Render the DeepSeek API key into a KEY=VALUE EnvironmentFile via a
      # sops-nix template. No hand-written plaintext copy: the rendered file
      # lives in tmpfs (/run/secrets/rendered/), is root-readable only, and is
      # regenerated from the encrypted sops secret on every boot/switch.
      sops.templates."hindsight-env" = {
        content = ''
          HINDSIGHT_API_LLM_API_KEY=${config.sops.placeholder."apikey/deepseek"}
          HINDSIGHT_API_TENANT_API_KEY=${config.sops.placeholder."hindsight/apikey"}
        '';
        mode = "0400";
        restartUnits = [ "hindsight.service" ];
      };

      # The API server ships as a regular nixpkgs package (pkgs.hindsight-api),
      # built by overlays/ai.nix — no runtime venv/uv bootstrap.
      systemd.services.hindsight = {
        description = "Hindsight memory API server";
        wantedBy = [ "multi-user.target" ];
        after = [ "network-online.target" "postgresql.service" ];
        wants = [ "network-online.target" ];
        requires = [ "postgresql.service" ];
        environment = {
          HOME = "/var/lib/hindsight";
          HF_ENDPOINT = "https://hf-mirror.com";
          HINDSIGHT_API_HOST = hindsight-host;
          HINDSIGHT_API_PORT = toString hindsight-port;
          HINDSIGHT_API_WORKER_ID = "hindsight";
          HINDSIGHT_API_LLM_PROVIDER = "deepseek";
          HINDSIGHT_API_LLM_MODEL = "deepseek-v4-flash";
          HINDSIGHT_API_TENANT_EXTENSION = "hindsight_api.extensions.builtin.tenant:ApiKeyTenantExtension";
          HINDSIGHT_API_DATABASE_URL = "postgresql://hindsight@/hindsight?host=/run/postgresql";
        };
        serviceConfig = {
          User = "hindsight";
          Group = "hindsight";
          ExecStart = "${pkgs.hindsight-api}/bin/hindsight-api";
          EnvironmentFile = [ config.sops.templates."hindsight-env".path ];
          Restart = "always";
          RestartSec = "5";
          TimeoutStopSec = "30";
          NoNewPrivileges = true;
          PrivateTmp = true;
          ProtectSystem = "strict";
          ReadWritePaths = [ "/var/lib/hindsight" ];
          UMask = "0077";
        };
      };

      # Point the hermes hindsight memory provider at this local service. The
      # config.json is rendered by sops-nix so its apiKey never appears in the
      # Nix source; it is decrypted from `hindsight/apikey` at activation time.
      sops.templates."hermes-hindsight-config" = lib.mkIf config.cluster.nodeConfig.hermes.enable {
        content = builtins.toJSON {
          mode = "local_external";
          api_url = hindsight-url;
          apiKey = config.sops.placeholder."hindsight/apikey";
          bank_id = "hermes";
        };
        path = "/var/lib/hermes/.hermes/hindsight/config.json";
        owner = "hermes";
        mode = "0600";
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
      sops.secrets."hindsight/apikey" = lib.mkIf sops-enable {
        sopsFile = config.cluster.ssh.publicKeySops;
        mode = "0555";
      };
      sops.templates."dsh-env" = {
        content = ''
          DEEPSEEK_API_KEY=${config.sops.placeholder."apikey/deepseek"}
        '';
        mode = "0400";
      };
    }
  ];
}
