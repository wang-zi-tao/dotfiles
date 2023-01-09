{ lib
, rustPlatform
, fetchFromGitHub
, pkg-config
, libvirt
, libvirt-glib
}:

rustPlatform.buildRustPackage
rec {
  pname = "balloond";
  version = "0.1.1";

  src = fetchFromGitHub {
    owner = "wang-zi-tao";
    repo = pname;
    rev = version;
    sha256 = "sha256-QflkclTFRk7ZioBTwYL+HMiaNHcsjBx+BsFhVUZkV0Q=";
  };

  cargoSha256 = "sha256-+vyU8yCM5VyPLLNzFOSnSsdSN2yV/jTZRsGW1+2y5Ng=";

  nativeBuildInputs = [
    pkg-config
    libvirt-glib.dev
  ];

  buildInputs = [
    libvirt
  ];

  meta = with lib; {
    description = "auto balloon service for libvirt";
    homepage = "https://github.com/wang-zi-tao/balloond";
    license = licenses.mit;
  };
}
