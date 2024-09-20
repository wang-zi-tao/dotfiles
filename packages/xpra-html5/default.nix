{
  stdenv,
  fetchgit,
  lib,
  python3,
}:
stdenv.mkDerivation rec {
  pname = "xpra-html5";
  version = "bf0d12";
  src = fetchgit {
    url = "https://github.com/Xpra-org/xpra-html5";
    rev = "bf0d129c77f3bc697b470343a3ec3b58b5a4937d";
    sha256 = "sha256-6hAnwC/6fvw/PsNxmg2+1mn75jnFGuX2jLKM9ZbRW1g=";
  };
  installPhase = ''
    mkdir $out
    python ./setup.py install / $out/install $out/config
  '';
  nativeBuildInputs = [ python3 ];
  buildInputs = [ ];
  meta = with lib; {
    description = "";
    license = licenses.mpl20;
    platforms = platforms.unix;
  };
}
