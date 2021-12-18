{ stdenv, fetchgit, lib, cmake }:
stdenv.mkDerivation rec {
  pname = "wcp";
  version = "57c5e64";
  src = fetchgit {
    url = "https://github.com/wheybags/wcp/";
    rev = "57c5e64fb65dd8c282309b7cd9d26484fc9b7e56";
    sha256 = "sha256-fYirw7plUEZV6OKdazEMnf2uYCI4u/fa4LOjy6B9Lb8=";
  }; 
  installPhase = ''
    mkdir -p $out/bin
    cp wcp $out/bin
  '';
  nativeBuildInputs = [ cmake ];
  buildInputs = [ ];
  meta = with lib; {
    description = "";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}
