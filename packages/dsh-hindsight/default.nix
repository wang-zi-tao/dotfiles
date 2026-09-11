{
  lib,
  stdenvNoCC,
  nodejs,
  typescript,
  dsh,
}:

stdenvNoCC.mkDerivation {
  pname = "dsh-hindsight";
  version = "0.2.0";

  src = ./.;

  nativeBuildInputs = [
    nodejs
    typescript
  ];

  buildPhase = ''
    runHook preBuild

    cp -rsf ${dsh}/lib/node_modules/@deepseek-ai/dsh/node_modules/ .

    tsc -p tsconfig.build.json
    runHook postBuild
  '';

  doCheck = true;
  checkPhase = ''
    runHook preCheck
    tsc -p tsconfig.test.json
    node --test build/test/*.test.js
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p "$out/lib"
    cp -r lib cordis.patch.yml package.json README.md ARCHITECTURE.md "$out/lib"
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
      and compiled with nixpkgs TypeScript.
    '';
    homepage = "https://hindsight.vectorize.io";
    license = licenses.mit;
    platforms = nodejs.meta.platforms;
  };
}
