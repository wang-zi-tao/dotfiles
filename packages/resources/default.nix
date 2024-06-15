{ stdenv, lib }:
stdenv.mkDerivation rec {
  pname = "resources";
  version = "v1.0.0";
  src = ./.;
  nativeBuildInputs = [ ];

  installPhase = ''
    mkdir $out/share/backgrounds -p
    cp *.png *.jpg -r $out/share/backgrounds/
  '';
}
