{
  lib,
  stdenvNoCC,
  nodejs,
  typescript,
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
    hash = "sha256-T5Ks2bnUWknjZ/Vu1pFciI8QacwXyokiFfYQX9eBSLI=";
    fetcherVersion = 4;
  };

  nativeBuildInputs = [
    nodejs
    typescript
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
    tsc -p tsconfig.test.json
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
