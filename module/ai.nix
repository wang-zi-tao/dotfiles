{
  config,
  pkgs,
  lib,
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

      # services.neo4j = {
      #   enable = true;
      #   config = {
      #     "dbms.security.procedures.unrestricted" = "apoc.*";
      #     "dbms.security.procedures.allowlist" = "apoc.*";
      #     "apoc.import.file.enabled" = "true";
      #     "dbms.security.auth_enabled" = "false";
      #   };
      #   directories.plugins = "${neo4j-plugins}";
      # };
      #
      # services.postgresql = {
      #   enable = true;
      #   ensureDatabases = [ "mem0" ];
      #   ensureUsers = [
      #     {
      #       name = "mem0";
      #     }
      #   ];
      # };
      #
      # virtualisation.oci-containers.containers.mem0 = {
      #   image = "mem0-selfhost:latest";
      #   environment = {
      #     NEO4J_URI = "bolt://host.docker.internal:7687";
      #     POSTGRES_HOST = "host.docker.internal";
      #     POSTGRES_PORT = "5432";
      #     POSTGRES_DB = "mem0";
      #     POSTGRES_USER = "mem0";
      #   };
      # };

      services.chromadb = {
        enable = true;
        port = 11437;
      };

      # virtualisation.oci-containers.containers.openmemory-mcp = {
      #   image = "mem0/openmemory-mcp";
      #   environment = {
      #     USER = "wangzi";
      #     CHROMADB_HOST = "host.docker.internal";
      #     CHROMADB_PORT = "11437";
      #     LLM_PROVIDER = "ollama";
      #     LLM_MODEL = "deepseek-r1-8b";
      #     EMBEDDER_PROVIDER = "ollama";
      #     EMBEDDER_MODEL = "nomic-embed-text";
      #     OLLAMA_BASE_URL = "http://host.docker.internal:11434";
      #   };
      #   ports = [ "11436:11436" ];
      #   volumes = [ "openmemory-mcp:/usr/src/openmemory" ];
      # };

      # virtualisation.oci-containers.containers.openmemory-ui = {
      #   image = "mem0/openmemory-ui:latest";
      #   ports = [ "11438:3000" ];
      #   dependsOn = [ "openmemory-mcp" ];
      #   environment = {
      #     NEXT_PUBLIC_USER_ID = "wangzi";
      #     NEXT_PUBLIC_API_URL = "http://host.docker.internal:11436";
      #   };
      # };

      services.nextjs-ollama-llm-ui = {
        enable = true;
        hostname = "0.0.0.0";
        port = 11435;
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
