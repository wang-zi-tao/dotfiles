pkgs: prev:
let
  # Hindsight 0.9.x needs 2026-era python deps (fastapi>=0.120, transformers 5.x,
  # litellm 1.9x, …) that the repo's 2025 base nixpkgs is too old for, so the
  # whole hindsight stack builds against nixpkgs-unstable.
  py = pkgs.python314Packages;
  lib = pkgs.lib;
  fetchPnpmPackage =
    {
      src,
      pname,
      version,
      hash,
    }:
    pkgs.fetchPnpmDeps {
      pname = "${pname}-deps";
      src = src;
      # First build fails with a hash mismatch; fill in the printed sha256 here.
      hash = hash;
      fetcherVersion = 4;
      nativeBuildInputs = [ pkgs.git ];
    };

  buildDshNpmPackage =
    args:
    pkgs.buildNpmPackage (
      args
      // {
        installPhase = ''
          mkdir -p "$out/lib"
          cp -r * "$out/lib/"
        '';
      }
    );

  buildDshPnpmPackage =
    {
      src,
      pname,
      version,
      hash,
      pnpmWorkspaces ? [ ],
      pnpmInstallFlags ? [ ],
      ...
    }@args:
    let
      filterFlags = lib.map (package: "--filter=${package}") pnpmWorkspaces;
    in
    pkgs.stdenvNoCC.mkDerivation (
      args
      // {
        nativeBuildInputs = [
          pkgs.nodejs
          pkgs.pnpm
          pkgs.pnpmConfigHook
          pkgs.git
          pkgs.pnpmConfigHook
          pkgs.pnpmBuildHook
          pkgs.npmHooks.npmInstallHook
        ];
        pnpmDeps = fetchPnpmPackage {
          inherit
            src
            pname
            version
            hash
            ;
        };

        installPhase = ''
          mkdir -p "$out/lib"
          cp ./* "$out/lib" -r
        '';
      }
    );
  dsh-client-ui = pkgs.fetchgit {
    url = "https://github.com/zhu1090093659/dsh-web-ui/";
    rev = "b391cc6cc97f6ba3c1dc743b5383519abeb106c4";
    sha256 = "sha256-bjWDb1IuTUOc+2toj3auBuEv6ywxoTZJBWwKQo6z0AE=";
  };
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

  dsh-lsp = buildDshNpmPackage rec {
    pname = "dsh-lsp";
    version = "6634206";
    src = pkgs.fetchgit {
      url = "https://github.com/omdsh-dev/dsh-lsp";
      rev = version;
      sha256 = "sha256-LJ/TKgn9Xrv6GWYSonkOakQG00h1/D613FePeJXRm6A=";
    };
    npmDepsHash = "sha256-sjRCXM9y4sSN6XO9rugquc3bTFfHK/ryU4i0qB3dYfc=";
  };

  dsh-agent-teams = buildDshPnpmPackage rec {
    pname = "dsh-agent-teams";
    version = "763d88";
    src = pkgs.fetchgit {
      url = "https://github.com/NanmiCoder/dsh-agent-teams";
      rev = version;
      sha256 = "sha256-oZEIHa6gUIz5q+jI7b5SkTVkBVOSbbuuwZATwsY5X0U=";
    };
    hash = "sha256-xv7QD/ecCYbwr8KBJSUet0M3ZmdHyBjE9/DxBtbAEjk=";
  };

  dsh-genui = buildDshPnpmPackage rec {
    pname = "dsh-genui";
    version = "2187fa4";
    src = pkgs.fetchgit {
      url = "https://github.com/omdsh-dev/dsh-genui";
      rev = version;
      sha256 = "sha256-FU0VrkilMivm2rHzLGvXl57KKNYHc8ROnUgQvYNrZgI=";
    };
    hash = "sha256-8GaDJuO8z1RJNCJQ8xFy2ofwaWGYCcqUtfEoHKV6t24=";
  };

  dsh-at-file = buildDshPnpmPackage rec {
    pname = "dsh-at-file";
    version = "898369e";
    src = pkgs.fetchgit {
      url = "https://github.com/omdsh-dev/dsh-at-file";
      rev = version;
      sha256 = "sha256-G3XbsI9BaEnBUmYEXkqGxQi78OHrF6wxnK3CPEnJ1pU=";
    };
    hash = "sha256-pTHoDj3MwGC4snJ5J8eKW0slfMdcEhvgmLgD+Kqa8eM=";
    nativeBuildInputs = with pkgs; [
      esbuild
    ];
  };

  dsh-client-ui-all = buildDshPnpmPackage {
    pname = "dsh-client-ui-all";
    version = "763d88";
    src = dsh-client-ui;
    hash = "sha256-ob/DC7I2q88s6o6ZCdj0QUy8krOyrU4JGIYHHRVsYf4=";
    preFixup = ''
      cp ./packages $out/ -r
    '';
  };

  dsh-tui = buildDshPnpmPackage rec {
    pname = "dsh-tui";
    version = "f7db605";
    src = pkgs.fetchgit {
      url = "https://github.com/ccch1mneyyy/dsh-TUI";
      rev = version;
      sha256 = "sha256-Dx1nMu/onJZlqiN56M0hq/5r0ggNC59xmjVV98TtSnA=";
    };
    hash = "sha256-U4c5/enAwPbJypxngBwtMy1IY+7xdw1MEB+vJA0MZZo=";
  };

  dsh-memory-evolve = buildDshPnpmPackage rec {
    pname = "dsh-memory-evolve";
    version = "1e6e7eb";
    src = pkgs.fetchgit {
      url = "https://github.com/csyangwen/dsh-memory-evolve";
      rev = version;
      sha256 = "sha256-fNFBsveLlLKMSOoIZvVu2u9Dcp3xjX+SRcrpLTbX2vQ=";
    };
    hash = "sha256-dIp6CNh1Kn4aqJWku1G/FUdn/u+epzhqlqwnAkB2uW0=";
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

  mcp-nixos = pkgs.writeShellScriptBin "mcp-nixos" ''
    exec ${prev.mcp-nixos}/bin/mcp-nixos "$@" 2> /dev/null
  '';
}
