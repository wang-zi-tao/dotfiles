{ stdenv, fetchgit, lib, gnumake, pkg-config, git, glibc }:
stdenv.mkDerivation rec {
  pname = "UDPspeeder";
  version = "fd7236";
  src = fetchgit {
    url = "https://github.com/wangyu-/UDPspeeder";
    rev = "fd72361deae7eebbaa814c3869babfd5539b1162";
    sha256 = "sha256-MkShEMFPzvmWxai5r7mO5ysOmu9UdhkycAQ4vRXsQDM=";
  };
  installPhase = ''
    mkdir -p $out/bin
    cp speederv2 $out/bin
  '';
  nativeBuildInputs = [ pkg-config gnumake git glibc.static ];
  buildInputs = [ ];
  meta = with lib; {
    description = "A Tunnel which Improves your Network Quality on a High-latency Lossy Link by using Forward Error Correction, possible for All Traffics(TCP/UDP/ICMP) ";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}
