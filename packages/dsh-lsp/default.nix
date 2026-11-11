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
  pname = "dsh-lsp";
  version = "0.1.0";

  src = ./.;

  pnpmDeps = fetchPnpmDeps {
    pname = "${pname}-deps";
    src = src;
    hash = "sha256-T+A7u0p1X9Wyr3EizmkBHOFEdedoYCHkkxaStlRrzCg=";
    fetcherVersion = 4;
  };

  # No nixpkgs `typescript` here on purpose: it is now the 7.x native port
  # (tsgo), which does not pick up node_modules/@types the way the project's
  # pinned 5.9.3 does, so a bare `tsc` in checkPhase resolves to the wrong
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
}
