{
  stdenv,
  fetchgit,
  lib,
  cmake,
  lua,
  luajit,
  linux-pam,
}:
stdenv.mkDerivation rec {
  pname = "lua-pam";
  version = "57c5e64";
  src = fetchgit {
    url = "https://github.com/RMTT/lua-pam";
    rev = "482071137257e55dac62a510f792104a9d910ea1";
    sha256 = "sha256-Kn0ozckmgxs19TmCuSChdiYMch92eif7DTEK+UoBtjw=";
  };
  installPhase = ''
    mkdir -p $out/lib
    cp liblua_pam.so $out/lib
  '';
  nativeBuildInputs = [
    cmake
    linux-pam
  ];
  buildInputs = [ lua ];
  meta = with lib; {
    description = "";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}
