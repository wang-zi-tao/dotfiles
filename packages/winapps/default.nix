{
  stdenv,
  fetchFromGitHub,
  lib,
}:
stdenv.mkDerivation rec {
  name = "winapps";
  version = "5431f94";
  src = fetchFromGitHub {
    owner = "Fmstrat";
    repo = "winapps";
    rev = version;
    sha256 = "sha256-2ZdyWnEiDsB/Znyw8rkiIhe6BFMzlwBuMF8uzZW5VlA=";
  };

  installPhase = ''
    mkdir -p $out
    cp -r ./* $out
  '';
}
