{
  fetchFromGitHub,
  fetchPnpmDeps,
  nodejs,
  pnpm,
  pnpmConfigHook,
  pnpmBuildHook,
  stdenv,
  git,
  lib,
  bashInteractive,
  makeWrapper,
}:
let
  pname = "dsh";
  version = "v0.1.5-alpha.1";
  src = fetchFromGitHub {
    owner = "deepseek-ai";
    repo = "deepseek-harness";
    rev = "dsh-${version}";
    sha256 = "sha256-pRgjpRdJerG2NIEQ9z3scaS9EvLgDsfhZeVXaqQpWkE=";
    leaveDotGit = true;
  };
  pnpmDeps =
    (fetchPnpmDeps {
      pname = "dsh-deps";
      src = src;
      fetcherVersion = 4;
      hash = "sha256-Mx3VVHqmjqbRvsQ4k8RoAkWd94ifxp1eIE2ufxvFNL4=";
      nativeBuildInputs = [
      ];
      prePnpmInstall = ''
        export SYSTEM=${stdenv.system}
      '';

    }).overrideAttrs
      (old: {
        installPhase = builtins.replaceStrings [ "--force" ] [ "" ] old.installPhase;
      });
in
stdenv.mkDerivation {
  inherit
    pname
    version
    src
    pnpmDeps
    ;
  nativeBuildInputs = [
    nodejs
    pnpm
    pnpmConfigHook
    pnpmBuildHook
    git
    makeWrapper
  ];
  dontNpmPrune = true;
  dontCheckForBrokenSymlinks = true;

  npmWorkspace = "dsh";

  installPhase = ''
    root=$out/lib/node_modules/@deepseek-ai/dsh-root
    mkdir -p $root
    cp -r * $root

    dsh_package=$out/lib/node_modules/@deepseek-ai/dsh/node_modules/
    mkdir -p $dsh_package
    ln -s $root/apps/cli/node_modules $dsh_package

    mkdir -p $out/bin
    makeWrapper ${lib.getExe nodejs} $out/bin/dsh \
      --argv0 dsh \
      --add-flags "--expose-internals" \
      --add-flags "$root/apps/cli/lib/bin.js"
  '';
}
