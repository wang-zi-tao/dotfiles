{
  lib,
  stdenvNoCC,
  nodejs,
  dsh,
  fetchPnpmDeps,
  pnpmConfigHook,
  pnpmBuildHook,
  pnpm,
}:

stdenvNoCC.mkDerivation rec {
  pname = "dsh-hindsight";
  version = "0.2.0";

  src = ./.;

  pnpmDeps = fetchPnpmDeps {
    pname = "${pname}-deps";
    src = src;
    hash = "sha256-n4cMWyb2UmPInPrBIbvLGyaxt/Jpg2HZmWnSKstaC2I=";
    fetcherVersion = 4;
  };

  # No nixpkgs `typescript` here on purpose: it is now the 7.x native port
  # (tsgo), which does not pick up node_modules/@types the way the project's
  # pinned 5.x does, so a bare `tsc` in checkPhase resolves to the wrong
  # compiler. The TypeScript version is the devDependency from pnpm-lock.yaml.
  nativeBuildInputs = [
    nodejs
    pnpm
    pnpmConfigHook
    pnpmBuildHook
  ];

  preConfig = ''
    cp -rsf ${dsh}/lib/node_modules/@deepseek-ai/dsh/node_modules/ .
  '';

  doCheck = true;
  checkPhase = ''
    runHook preCheck
    ./node_modules/.bin/tsc -p tsconfig.test.json
    node --test build/test/*.test.js
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p "$out/lib"
    cp -r lib cordis.patch.yml package.json README.md ARCHITECTURE.md node_modules "$out/lib"
    runHook postInstall
  '';

  meta = with lib; {
    description = "Hindsight long-term memory plugin for DeepSeek Harness";
    longDescription = ''
      dsh-hindsight provides hindsight_retain / hindsight_recall /
      hindsight_reflect / hindsight_status tools, a /hindsight-import slash
      command for importing historical sessions, automatic per-turn
      retention from the dsh session event log, and
      automatic cross-session recall injected as a user-role message at the
      start of each agent turn. Server address, bank id, budget and memory
      mode are configurable through the Cordis row config and HINDSIGHT_*
      environment variables. The package is written in TypeScript
      and compiled with the project's pinned TypeScript.
    '';
    homepage = "https://hindsight.vectorize.io";
    license = licenses.mit;
    platforms = nodejs.meta.platforms;
  };
}
