pkgs: prev:
let
  # Hindsight 0.9.x needs 2026-era python deps (fastapi>=0.120, transformers 5.x,
  # litellm 1.9x, …) that the repo's 2025 base nixpkgs is too old for, so the
  # whole hindsight stack builds against nixpkgs-unstable.
  py = pkgs.python314Packages;
in
rec {
  mcp-neovim-server = pkgs.buildNpmPackage {
    pname = "mcp-neovim-server";
    version = "9076bb";
    src = pkgs.fetchFromGitHub {
      owner = "bigcodegen";
      repo = "mcp-neovim-server";
      rev = "9076bbb34a08f44a743ad66c78638ef22da58ab0";
      sha256 = "sha256-rSnizEKhuvHSxwmOG/V+QIaAx7TCN1lGUiP28usaeng=";
    };
    npmDepsHash = "sha256-vqRPSO8Oji0HvTMBDUXrhQxe+M6cfFpALnqsBfrctPQ=";
  };

  mcp-open-websearch = pkgs.buildNpmPackage {
    pname = "mcp-open-websearch";
    version = "1.2.5";
    src = pkgs.fetchFromGitHub {
      owner = "Aas-ee";
      repo = "open-webSearch";
      rev = "v1.2.5";
      sha256 = "sha256-ICT8pTwIL20/Yz6vz9cF4gwMqbNFS9uScB/Gt5qQais=";
    };
    npmDepsHash = "sha256-HHsWdhqGlIKyTNL2Jd0RnJmSJC8+T4Iz79oUTUGvt8M=";
  };

  mcp-obsidian = pkgs.buildNpmPackage {
    pname = "mcp-obsidian";
    version = "487625a";
    src = pkgs.fetchFromGitHub {
      owner = "bitbonsai";
      repo = "mcpvault";
      rev = "487625aa38302fa5faaacb51ced949ff9c71880a";
      sha256 = "sha256-QpFhAEzfrHtsKGKROBJfNgFSbV3B5ZwbqEYTh2PrH9c=";
    };
    npmDepsHash = "sha256-cKEzttAbFBPZ7dhNs4JIcltkIftU2Y5PuxiCFCm14ew=";
  };

  git-mcp-server = pkgs.buildNpmPackage {
    pname = "git-mcp-server";
    version = "v2.10.3";
    src = pkgs.fetchgit {
      url = "https://github.com/wang-zi-tao/git-mcp-server";
      rev = "ae406188a021f560e469ab77e823eebccd75b417";
      sha256 = "sha256-j1T+pim9FKmBQAd/aj3FCMzyUBhhGkMuKGpKD7wmbjE=";
    };
    npmDepsHash = "sha256-5Gw7tOLPl7t8b+TGCM+qAKhZ+sTavuDSb8J4Hlw1PF8=";

    nativeBuildInputs = [ pkgs.bun ];
    buildInputs = [ pkgs.bun ];
  };

  opencode-mem = pkgs.buildNpmPackage {
    pname = "opencode-mem";
    version = "v2.13.0";
    src = pkgs.fetchgit {
      url = "https://github.com/tickernelz/opencode-mem";
      rev = "v2.13.0";
      sha256 = "sha256-Xrf37Dury44kzACpVPf9WO3U8gwEx6/DKxbUFSoq4e0=";
    };
    npmDepsHash = "";

    nativeBuildInputs = [ pkgs.bun ];
    buildInputs = [ pkgs.bun ];
  };

  opencode-bunx = pkgs.writeShellScriptBin "opencode" ''
    #!${pkgs.stdenv.shell}
    exec ${pkgs.bun}/bin/bunx opencode-ai "$@"
  '';

  hermes-lcm = pkgs.fetchFromGitHub {
    owner = "stephenschoettler";
    repo = "hermes-lcm";
    rev = "v0.7.0";
    hash = "sha256-0D5htaT/Y7uhYfI0yV1L7tiPjGf4kOJDdTMsb96uvhk=";
  };

  hermes-rtk = pkgs.python312Packages.buildPythonPackage {
    pname = "rtk-hermes";
    version = "1.0.0";
    src = pkgs.fetchFromGitHub {
      owner = "ogallotti";
      repo = "rtk-hermes";
      rev = "v1.0.0";
      hash = "sha256-0D5htaT/Y7uhYfI0yV1L7tiPjGf4kOJDdTMsb96uvhk=";
    };
    format = "pyproject";
    build-system = [ pkgs.python312Packages.setuptools ];
  };

  # ── Hindsight memory server (https://github.com/vectorize-io/hindsight) ──
  # `hindsight-api` == `hindsight-api-slim` minus the provider SDKs we never use
  # (claude-code/anthropic/cohere/bedrock/object-storage/file-parsing/flashrank/
  # mlx/onnx are lazily imported and skipped), plus the local embeddings/reranker
  # stack (torch + sentence-transformers). The database is a NixOS-managed
  # PostgreSQL (see module/ai.nix) — pg0-embedded is deliberately omitted because
  # its bundled postgres needs system libs/zoneinfo that aren't present on NixOS.
  hindsight-api = py.buildPythonPackage rec {
    pname = "hindsight-api-slim";
    version = "0.9.1";
    pyproject = true;
    src = pkgs.fetchurl {
      # Exact URL (fetchPypi builds the wrong one: the sdist filename uses
      # underscores `hindsight_api_slim`, not the hyphenated project name).
      url = "https://files.pythonhosted.org/packages/bd/98/e405ff40dde49769ca8f959f9ff452b28c28d34590da2c7ff016fd29d0ae/hindsight_api_slim-0.9.1.tar.gz";
      hash = "sha256-4sw3i+r63JaoRughtYiUhHAWt5GS4a0O+zm4QgJlrR0=";
    };
    build-system = [ py.hatchling ];
    # Nix hand-picks versions instead of pip-resolving, and we intentionally
    # dropped the lazily-imported provider SDKs, so reconcile the wheel's
    # Requires-Dist metadata with what is actually in the closure:
    #   - remove the SDKs we don't ship (and psycopg2-binary → we ship psycopg2),
    #   - relax version pins that nixpkgs satisfies only approximately
    #     (greenlet<3.4 is an arm64-wheel-availability pin; litellm/otel floors
    #     are newer than nixpkgs but their APIs are compatible for our providers).
    pythonRelaxDeps = true;
    pythonRemoveDeps = [
      "anthropic"
      "boto3"
      "claude-agent-sdk"
      "cohere"
      "markitdown"
      "obstore"
      "psycopg2-binary"
    ];
    dependencies = [
      # base
      py.aiohttp
      py.alembic
      py.asyncpg
      py.authlib
      py.croniter
      py.cryptography
      py.dateparser
      py.fastapi
      py.httpx
      py.jinja2
      py.email-validator
      py.python-multipart
      py.uvicorn
      py.fastapi-cli
      py.fastmcp
      py.filelock
      py.google-auth
      py.google-genai
      py.greenlet
      py.json-repair
      py.langchain-core
      py.langchain-text-splitters
      py.langsmith
      py.litellm
      py.openai
      py.opentelemetry-api
      py.opentelemetry-exporter-otlp-proto-http
      py.opentelemetry-exporter-prometheus
      py.opentelemetry-instrumentation-fastapi
      py.opentelemetry-sdk
      py.opentelemetry-semantic-conventions
      py.orjson
      py.pgvector
      py.pillow
      py.protobuf
      py.psycopg2
      py.pyasn1
      py.pydantic
      py.pygments
      py.pyjwt
      py.python-dateutil
      py.python-dotenv
      py.rich
      py.sqlalchemy
      py.tiktoken
      py.tornado
      py.typer
      py.urllib3
      py.uvloop
      py.wsproto
      # local embeddings/reranker
      py.einops
      py.huggingface-hub
      py.numpy
      py.safetensors
      py.sentence-transformers
      py.tokenizers
      py.torch
      py.transformers
    ];
    meta.mainProgram = "hindsight-api";
  };


  mcp-nixos =  pkgs.writeShellScriptBin "mcp-nixos" ''
    exec ${prev.mcp-nixos}/bin/mcp-nixos "$@" 2> /dev/null
  '';
}
