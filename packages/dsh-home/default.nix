{
  lib,
  dsh-hindsight,
  dsh-lsp,
  dsh-neovim,
  stdenvNoCC,
  fetchPnpmDeps,
  git,
  rolldown,
  nodejs,
  pnpm,
  pnpmConfigHook,
  pnpmBuildHook,
  npmHooks,
  fetchgit,
  fetchurl,
  writeScript,
  busybox,
  codegraph,
  vectorcode,
  mcp-nixos,
  formats,
  cua-driver,
  dsh,

  enable_develop ? true,
  enable_desktop ? true,
}:
let

  fetchPnpmPackage =
    {
      src,
      pname,
      version,
      hash,
    }:
    fetchPnpmDeps {
      pname = "${pname}-deps";
      src = src;
      # First build fails with a hash mismatch; fill in the printed sha256 here.
      hash = hash;
      fetcherVersion = 4;
      nativeBuildInputs = [
        git
        rolldown
      ];
    };

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
    stdenvNoCC.mkDerivation (
      args
      // {
        nativeBuildInputs = [
          nodejs
          pnpm
          git
          pnpmConfigHook
          pnpmBuildHook
          npmHooks.npmInstallHook
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

  dsh-continual-harness = buildDshPnpmPackage rec {
    pname = "dsh-continual-harness";
    version = "v0.3.3";
    src = fetchgit {
      url = "https://github.com/jasen215/dsh-continual-harness";
      rev = version;
      sha256 = "sha256-HEx21GTAYMFOZhdJMQr165l7Ay94MOH/UyF1YNz+U0M=";
    };
    hash = "sha256-dZXm/Sg7R4EZYVXAy9Fq29guOLXy9HYn3zEjJ8sWuv0=";
  };

  billion-context = stdenvNoCC.mkDerivation rec {
    pname = "billion-context";
    version = "0.1.204";
    src = fetchurl {
      url = "https://registry.npmjs.org/billion-context/-/billion-context-${version}.tgz";
      hash = "sha256-ftwWrlzk5e5oEw5wajEqm/QW1fDY4f3RuhZV+UXDLXU=";
    };
    dontConfigure = true;
    dontBuild = true;
    # 与其它 dsh 包保持同一布局：包根 = $out/lib
    installPhase = ''
      runHook preInstall
      mkdir -p "$out/lib"
      cp -r ./* "$out/lib/"
      runHook postInstall
    '';
  };

  mcpPatch =
    name: server:
    let
      wrapper = writeScript "dsh-mcp-${name}-wrapper" ''
        #!${busybox}/bin/sh
        exec "$@" 2> >(logger -t ${name})
      '';
    in
    {
      id = "mcp-${name}";
      name = "@deepseek-ai/dsh-mcp-client";
      config =
        if builtins.hasAttr "url" server then
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

  profile_cordis_patch = [
    {
      insert = [
      ];
    }
  ];
  plugins = [
    dsh-hindsight
    dsh-lsp
    dsh-neovim
    dsh-continual-harness
    billion-context
  ];
  extra_bundles = [
    "@deepseek-ai/dsh-experimental-agent-team-profile"
    "@deepseek-ai/dsh-experimental-schedule-bundle"
    "@deepseek-ai/dsh-experimental-session-search"
  ];

  mcps = {
    nixos = {
      command = "${mcp-nixos}/bin/mcp-nixos";
    };
  }
  // lib.optionalAttrs enable_develop {
    codegraph = {
      command = "${codegraph}/bin/codegraph";
      args = [
        "serve"
        "--mcp"
      ];
    };
    vectorcode = {
      command = "${vectorcode}/bin/vectorcode-mcp-server";
    };
  };

  cordis-patch = [
    {
      insert =
        (builtins.attrValues (builtins.mapAttrs mcpPatch mcps))
        ++ [
          {
            id = "skills-nix";
            name = "@deepseek-ai/dsh-skill-filesystem";
            customSkillDirs = [
              ../../skills
            ];
          }
        ]
        ++ lib.optionals enable_develop [
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
        ]
        ++ lib.optionals enable_desktop [
          {
            id = "computer-use";
            name = "@deepseek-ai/dsh-computer-use";
          }
          {
            id = "computer-use-cua-driver-mcp";
            name = "@deepseek-ai/dsh-experimental-computer-use-cua-driver-mcp";
            config = {
              command = "${cua-driver}/bin/cua-driver";
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
        apiKey = "!!js process.getBuiltinModule('fs').readFileSync('/run/secrets/hindsight/apikey','utf8').trim()";
      };
    }
    {
      id = "agent-default-model";
      config = {
        provider = "deepseek-official";
        model = "deepseek-flash";
        reasoningEffort = "high";
      };
    }
    {
      id = "ui-settings-general";
      name = "@deepseek-ai/dsh-client-ui-settings-general";
      config = {
        welcomeNoticeVersion = "2026-09-28.1";
      };
    }
    {
      id = "agent-preset-registry";
      name = "@deepseek-ai/dsh-agent-preset-registry";
      config = {
        default = "ptc";
        selectedDefault = "ptc";
      };
    }
    {
      id = "permission";
      name = "@deepseek-ai/dsh-permission-presets";
      config = {
        defaultPreset = "workspace-write";
      };
    }
    {
      id = "ui-conversation";
      name = "@deepseek-ai/dsh-client-ui-conversation";
      config = {
        busyEnter = "steer";
      };
    }
    {
      id = "tailnet-admin";
      name = "@he0119/dsh-tailnet-admin";
      config = {
        disableBrowserAuth = true;
      };
    }
  ];

  build-dsh-profile =
    {
      name,
      src,
      hash,
      plugins,
      extra_bundles,
    }:
    stdenvNoCC.mkDerivation {
      pname = name;
      version = "0.1.0";

      src = src;

      nativeBuildInputs = [
        nodejs
        pnpm
        pnpmConfigHook
        git
      ];

      buildInputs = plugins ++ [ dsh ];

      pnpmDeps =
        (fetchPnpmDeps {
          pname = "dsh-profile-deps-${name}-${stdenvNoCC.system}";
          src = src;
          # First build fails with a hash mismatch; fill in the printed sha256 here.
          hash = hash;
          fetcherVersion = 4;
          nativeBuildInputs = [ git ];
        }).overrideAttrs
          (old: {
            installPhase = builtins.replaceStrings [ "--force" ] [ "" ] old.installPhase;
          });

      extra_bundles = lib.escapeShellArgs extra_bundles;
      dsh_node_modules = "${dsh}/lib/node_modules/@deepseek-ai/dsh/node_modules";

      installPhase = ''
        runHook preInstall

        mkdir -p "$out/lib"
        cp -r package.json pnpm-lock.yaml pnpm-workspace.yaml "$out"
        cp -r node_modules $out/lib
        ln -s $cordis_patch_yaml $out/cordis.patch.yml

        node ${./generate-bundles.mjs} "$out/package.json" "$out/lib/node_modules" ${lib.escapeShellArgs (map toString plugins)}
        node ${./add-bundles.mjs} "$out/package.json" $extra_bundles

        ln -s $dsh_node_modules "$out/"
        runHook postInstall
      '';

      meta = with lib; {
        description = "Nix-built dsh web profile: pnpm-managed plugins as a read-only store layer";
        platforms = platforms.unix;
      };
    };

  jsYaml = stdenvNoCC.mkDerivation {
    pname = "dsh-cordis-patch-js-yaml";
    version = "4.2.0";
    src = fetchurl {
      url = "https://registry.npmjs.org/js-yaml/-/js-yaml-4.2.0.tgz";
      hash = "sha256-ULr8SqTLJjstOz9DBTXtjUkpTrEHeW52fXalykdaymY=";
    };
    dontConfigure = true;
    dontBuild = true;
    installPhase = ''
      runHook preInstall
      mkdir -p "$out/lib/node_modules/js-yaml"
      cp -r ./* "$out/lib/node_modules/js-yaml/"
      runHook postInstall
    '';
  };

  profiles = builtins.map (profile: build-dsh-profile profile) [
    {
      name = "web";
      hash = "sha256-bPQTKI4JCTkANYmQW7Nipt8eI3xz0oCSfl77qwD1Pvk=";
      plugins = plugins ++ [ ];
      src = ./web;
      extra_bundles = extra_bundles ++ [ "@deepseek-ai/dsh-web-app" ];
    }
    {
      name = "tui";
      hash = "sha256-nj9CDtQRo8qxPxUzKjOWvuuEGsnQVZneA9zYpbAGTTs=";
      plugins = plugins ++ [ ];
      src = ./tui;
      extra_bundles = extra_bundles;
    }
  ];

  dsh-home = stdenvNoCC.mkDerivation {
    pname = "dsh-home";
    version = "1.0";

    src = ./.;

    nativeBuildInputs = [ nodejs ];

    profile_cordis_patch_yaml = (formats.yaml { }).generate "cordis.raw.yml" profile_cordis_patch;
    cordis_patch_yaml = (formats.yaml { }).generate "cordis.raw.yml" cordis-patch;

    preInstall = "";

    installPhase = ''
      runHook preInstall

      mkdir -p $out/.dsh
      mkdir -p node_modules
      ln -s ${jsYaml}/lib/node_modules/js-yaml node_modules/js-yaml
      node ./js-tag.mjs $cordis_patch_yaml $out/.dsh/cordis.patch.yml
    ''
    + builtins.concatStringsSep "\n" (
      builtins.map (profile: ''
        mkdir -p $out/.dsh/profiles/${profile.pname}
        pushd $out/.dsh/profiles/${profile.pname}
          ln -s \
            ${profile}/package.json \
            ${profile}/pnpm-lock.yaml \
            ${profile}/pnpm-workspace.yaml \
            ${profile}/lib/node_modules \
            $out/.dsh/profiles/${profile.pname}
        popd
      '') profiles
    )
    + ''
      mkdir $out/.dsh-tui
      cp tui/settings.json $out/.dsh-tui/settings.json
      cp AGENTS.md $out/.dsh

      runHook postInstall
    '';
  };

in
dsh-home
// rec {
  home-file-factory =
    dsh-home:
    {
      ".dsh-tui/settings.json".source = "${dsh-home}/.dsh-tui/settings.json";
      ".dsh/profiles/node_modules".source = "${dsh}/lib/node_modules";
      ".dsh/AGENTS.md".source = "${dsh-home}/.dsh/AGENTS.md";
      ".dsh/cordis.patch.yml".source = "${dsh-home}/.dsh/cordis.patch.yml";
    }
    // builtins.listToAttrs (
      builtins.concatMap (
        profile:
        let
          name = profile.pname;
        in
        lib.attrsets.attrsToList {
          ".dsh/profiles/${name}/package.json".source = "${dsh-home}/.dsh/profiles/${name}/package.json";
          ".dsh/profiles/${name}/pnpm-lock.yaml".source = "${dsh-home}/.dsh/profiles/${name}/pnpm-lock.yaml";
          ".dsh/profiles/${name}/pnpm-workspace.yaml".source =
            "${dsh-home}/.dsh/profiles/${name}/pnpm-workspace.yaml";
        }
        ++ map (e: {
          name = ".dsh/profiles/${name}/node_modules/${e}";
          value = {
            source = "${profile}/lib/node_modules/${e}";
            force = true;
          };
        }) (lib.attrNames (builtins.readDir "${profile}/lib/node_modules"))
      ) profiles
    );
  home-file = home-file-factory dsh-home;
}
