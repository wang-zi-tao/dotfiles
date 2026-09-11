{
  lib,
  stdenvNoCC,
  nodejs,
  typescript,
  dsh,
}:

stdenvNoCC.mkDerivation {
  pname = "dsh-lsp";
  version = "0.1.0";

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
    cp -r lib cordis.patch.yml package.json package-lock.json README.md ARCHITECTURE.md "$out/lib"
    runHook postInstall
  '';
}
