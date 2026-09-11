{
  lib,
  stdenvNoCC,
  nodejs,
  typescript,
  dsh,
  fetchPnpmDeps,
  git,
  pnpmConfigHook,
  pnpmBuildHook,
  pnpm,
}:

stdenvNoCC.mkDerivation rec {
  pname = "dsh-neovim";
  version = "0.1.0";

  src = ./.;

  pnpmDeps = fetchPnpmDeps {
    pname = "${pname}-deps";
    src = src;
    # First build fails with a hash mismatch; fill in the printed sha256 here.
    hash = "sha256-NHOkebvMOkzzwpJtxo+BpUjM6PSQ8OhRr22gxHiVo5g=";
    fetcherVersion = 4;
    nativeBuildInputs = [ git ];
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

  buildPhase = ''
    runHook preBuild
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
    cp -r lib cordis.patch.yml package.json README.md ARCHITECTURE.md node_modules "$out/lib"
    runHook postInstall
  '';
}
