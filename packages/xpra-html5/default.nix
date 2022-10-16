{ stdenv, fetchgit, lib, makeWrapper, xpra }:
stdenv.mkDerivation rec {
  pname = "xpra-html5";
  version = "bf0d12";
  src = fetchgit {
    url = "https://github.com/Xpra-org/xpra-html5";
    rev = "bf0d129c77f3bc697b470343a3ec3b58b5a4937d";
    sha256 = "sha256-6hAnwC/6fvw/PsNxmg2+1mn75jnFGuX2jLKM9ZbRW1g=";
  };
  installPhase = ''
    mkdir -p $out/bin
    cp . $out -r
    makeWrapper ${xpra}/bin/xpra $out/bin/xpra-html5-start \
        --add-flags "start --html=$out"
    makeWrapper ${xpra}/bin/xpra $out/bin/xpra-html5-shadow \
        --add-flags "shadow --html=$out"
  '';
  nativeBuildInputs = [ ];
  buildInputs = [ makeWrapper ];
  meta = with lib; {
    description = "";
    license = licenses.mpl20;
    platforms = platforms.unix;
  };
}

