{
  stdenv,
  fetchgit,
  lib,
  gnumake,
  pkg-config,
  git,
  glibc,
}:
stdenv.mkDerivation rec {
  pname = "udp2raw";
  version = "20200818.0";
  src = fetchgit {
    url = "https://github.com/wangyu-/udp2raw.git";
    rev = version;
    sha256 = "sha256-TkTOfF1RfHJzt80q0mN4Fek3XSFY/8jdeAVtyluZBt8=";
  };
  installPhase = ''
    mkdir -p $out/bin
    cp udp2raw $out/bin
  '';
  nativeBuildInputs = [
    pkg-config
    gnumake
    git
    glibc.static
  ];
  buildInputs = [ ];
  meta = with lib; {
    description = " A Tunnel which Turns UDP Traffic into Encrypted UDP/FakeTCP/ICMP Traffic by using Raw Socket,helps you Bypass UDP FireWalls(or Unstable UDP Environment) ";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}
